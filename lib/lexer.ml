type fat_token = {
	token : Token.t;
	pos : int;
	line : int;
	col : int;
} [@@deriving show, eq]

type resolve_result = (fat_token list, fat_token option * string) result [@@deriving show]

type lexer_result = (unit, fat_token option * string) result [@@deriving show]

type coerce = LeftBinary | Left | Binary | Right | RightBinary | Any [@@deriving show]

type filter =
	| Open
	| Closed
	| Operator of coerce [@@deriving show]

type triple_coerce = {
	left : coerce;
	self : filter; (* None = not an operator *)
	right : coerce;
} [@@deriving show]

type t = {
	reader : Reader.t;
	mutable buf : fat_token list;
	mutable context : fat_token array;
	(* mutable last_triple : triple_coerce; *)
	(* frame : fat_token Vector.t; *)
	(* coercion_ops : triple_coerce Vector.t; *)
} [@@deriving show]

let eof = { token = Token.Eof; pos = -1; line = -1; col = -1 }

let init (flow : _ Eio.Flow.source) (max_size : int) : t = {
	reader = Reader.init flow max_size;
	buf = [];
	context = [| { token = Token.Eof; pos = -1; line = -1; col = -1 }; { token = Token.Eof; pos = -1; line = -1; col = -1 } |];
	(* last_triple = { left = Binary; self = Binary; right = Left }; *)
	(* frame = Vector.from_capacity 10 { token = Token.Unknown; pos = -1; line = -1; col = -1 }; *)
	(* coercion_ops = Vector.init (); *)
}

let determine (l : t) : resolve_result =
	(* small: get a raw token from lexer *)
	let raw_fat_token (l : t) : (fat_token, string) result =
		Reader.skip_whitespace l.reader;
		let (pos, line, col) = (Reader.pos l.reader, Reader.line l.reader, Reader.col l.reader) in
		match Token.tokenize l.reader with
		| Ok Token.Eof -> Ok { token = Token.Eof; pos = -1; line = -1; col = -1 }
		| Ok token -> Ok { token = token; pos = pos; line = line; col = col }
		| Error e -> Error e
	in

	(* small: apply filter to single pair *)
	let apply_filter (fat, filter) =
		let fat = {
			fat with
			token = (
				let {token;_} = fat in
				match filter.left with
				| Left -> Token.leftify token
				| Right -> Token.rightify token
				| Binary -> Token.binaryify token
				| LeftBinary -> (match filter.right with
					| Left -> Token.leftify token
					| RightBinary -> Token.binaryify token
					| _ -> token)
				| RightBinary -> (match filter.right with
					| Right -> Token.rightify token
					| LeftBinary -> Token.binaryify token
					| _ -> token)
				| Any -> (match filter.right with
					| Left -> Token.leftify token
					| Right -> Token.rightify token
					| Binary -> Token.binaryify token
					| _ -> token)
			);
		} in
		let filter = {
			filter with
			self = (
				match filter.self with
				| Operator _ -> (
					match filter.left with
					| Left -> Operator Left
					| Right -> Operator Right
					| Binary -> Operator Binary
					| LeftBinary -> (match filter.right with
						| Left -> Operator Left
						| RightBinary -> Operator Binary
						| _ -> filter.self)
					| RightBinary -> (match filter.right with
						| Right -> Operator Right
						| LeftBinary -> Operator Binary
						| _ -> filter.self)
					| Any -> (match filter.right with
						| Left -> Operator Left
						| Right -> Operator Right
						| Binary -> Operator Binary
						| _ -> filter.self)
				)
				| _ -> filter.self
			)
		} in (fat, filter)
	in

	(* small: apply filter to single pair (errors if ambiguous) *)
	let fatal_apply_filter (fat, filter) =
		let token = (
			let {token;_} = fat in
			match filter.left with
			| Left -> Ok (Token.leftify token)
			| Right -> Ok (Token.rightify token)
			| Binary -> Ok (Token.binaryify token)
			| LeftBinary -> (match filter.right with
				| Left -> Ok (Token.leftify token)
				| Right | RightBinary -> Ok (Token.binaryify token)
				| _ -> if Token.ambiguous token then Error (Some fat, "ambiguous expression, please add parenthesis.") else Ok token)
			| RightBinary -> (match filter.right with
				| Right -> Ok (Token.rightify token)
				| Left | LeftBinary -> Ok (Token.binaryify token)
				| _ -> if Token.ambiguous token then Error (Some fat, "ambiguous expression, please add parenthesis.") else Ok token)
			| Any -> (match filter.right with
				| Left -> Ok (Token.leftify token)
				| Right -> Ok (Token.rightify token)
				| Binary -> Ok (Token.binaryify token)
				| _ -> if Token.ambiguous token then Error (Some fat, "ambiguous expression, please add parenthesis.") else Ok token)
		) in
		match token with
		| Ok token -> (
			let fat = {
				fat with
				token = token;
			} in
			let filter = {
				filter with
				self = (
					match filter.self with
					| Operator _ -> (
						match filter.left with
						| Left -> Operator Left
						| Right -> Operator Right
						| Binary -> Operator Binary
						| LeftBinary -> (match filter.right with
							| Left -> Operator Left
							| RightBinary -> Operator Binary
							| _ -> filter.self)
						| RightBinary -> (match filter.right with
							| Right -> Operator Right
							| LeftBinary -> Operator Binary
							| _ -> filter.self)
						| Any -> (match filter.right with
							| Left -> Operator Left
							| Right -> Operator Right
							| Binary -> Operator Binary
							| _ -> filter.self)
					)
					| _ -> filter.self
				)
			} in Ok (fat, filter)
		)
		| Error e -> Error e
	in

	(* forward pass -> returns reversed frame + partial resolution *)
	let cache_frame (l : t) : ('a list, string) result =
		let rec cache_frame (l : t) (frame : 'a list) : ('a list, string) result =
			match raw_fat_token l with
			| Ok fat -> (
				match fat.token with
				| Token.Eof -> (
					match frame with
					| [] -> Ok [(fat, { left = Any; self = Closed; right = Any })]
					| h::t -> (
						let (fat, filter) = h in
						let fat = { fat with token = Token.rightify fat.token } in
						Ok ((fat, filter) :: t)
					)
				)
				| _ -> (
					let filter = {
						(* What restrictions does the token to the left of this token impose on this token? *)
						left = (
							match frame with
							| [] -> (
								(* empty = use cached unambiguous token to guide decision *)
								match l.context.(1).token with
								| t when Token.left t || Token.binary t -> Left
								| t when Token.right t || (Token.scope t) < 0 || Token.literal t || Token.id t -> RightBinary
								| _ -> Left
							)
							| h::_ -> (
								let (_, {self;_}) = h in
								match self with
								| Closed -> RightBinary
								| Open -> Left
								| Operator c -> (match c with
									| Left -> Left
									| Right -> RightBinary
									| Binary -> Left
									| LeftBinary -> Left
									| RightBinary -> Any (* This shouldn't happen ever *)
									| Any -> Any)
							)
						);
						self = (
							let {token;_} = fat in
							if Token.oper token then (
								match token with
								| t when Token.has_left t ->
									if Token.has_binary token then Operator LeftBinary else Operator Left
								| t when Token.has_right t ->
									if Token.has_binary token then Operator RightBinary else Operator Right
								| t when Token.has_binary t ->
									Operator Binary
								| _ -> failwith "impossible"
							) else if Token.scope token < 0 then (
								Closed
							) else (
								Open
							)
						);
						right = Any;
					} in
					let pair = apply_filter (fat, filter) in
					if not (Token.ambiguous fat.token) then
						Ok (pair :: frame)
					else
						cache_frame l (pair :: frame)
				)
			)
			| Error e -> Error e
		in cache_frame l []
	in

	(* backward pass -> unreverses frame + bidirectional resolution *)
	let backtrack (frame : 'a list) : ('a list, fat_token option * string) result =
		let rec backtrack (frame : 'a list) (acc : 'a list) : ('a list, fat_token option * string) result =
			match frame with
			| [] -> Ok acc
			| h::t -> (
				let (fat, filter) = h in
				let filter = {
					filter with
					right = (
						(* tried frame, t, now acc *)
						match acc with
						| [] -> Right (* Doesn't matter cuz should be unambiguous anyway right? Becomes LeftIncrement tho for whatever reason *)
						| h::_ -> (
							let (_, {self;_}) = h in
							match self with
							| Open -> LeftBinary
							| Closed -> Right
							| Operator c -> (match c with
								| Left -> LeftBinary
								| Right -> Right
								| Binary -> Right
								| LeftBinary -> Any
								| RightBinary -> Right (* This shouldn't happen ever *)
								| Any -> Any)
						)
					);
				} in
				match fatal_apply_filter (fat, filter) with
				| Ok (fat, filter) -> backtrack t ((fat, filter) :: acc)
				| Error e -> Error e
			)
		in backtrack frame []
	in

	match cache_frame l with
	| Error e -> Error (
		Some {
			token = Token.Unknown;
			pos = Reader.pos l.reader;
			line = Reader.line l.reader;
			col = Reader.col l.reader
		},
		e
	)
	| Ok frame -> (
		match backtrack frame with
		| Ok frame -> Ok (List.map (fun (fat, _) -> fat) frame)
		| Error e -> Error e
	)

let pop (l : t) : (fat_token, fat_token option * string) result =
	let cycle (fat : fat_token) (l : t) : unit =
		l.context.(0) <- l.context.(1);
		l.context.(1) <- fat;
	in

	match l.buf with
	| { token = Token.Eof; _ } as eof :: _ ->
		Ok eof
	| h::t ->
		l.buf <- t; cycle h l;
		Ok h
	| [] ->
		(* match _resolve l with *)
		match determine l with
		| Ok ({ token = Token.Eof; _ } as eof :: _) ->
			(* Cache single eof so subsequent calls short circuit into eof *)
			l.buf <- [eof]; cycle eof l;
			Ok eof
		| Ok (h::t) ->
			l.buf <- t; cycle h l;
			Ok h
		| Ok [] ->
			Error (None, "empty frame from resolver")
		| Error e ->
			Error e

let push tok (l : t) : unit =
	l.buf <- (tok :: l.buf)

let peek (l : t) : (fat_token, fat_token option * string) result =
	let (f, s) = (l.context.(0), l.context.(1)) in
	match pop l with
	| Ok tok -> (
		push tok l;
		l.context.(0) <- f;
		l.context.(1) <- s;
		Ok tok
	)
	| Error e -> Error e
