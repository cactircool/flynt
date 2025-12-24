type fat_token = {
	token : Token.t;
	pos : int;
	line : int;
	col : int;
} [@@deriving show, eq]

type resolve_result = (fat_token list, fat_token option * string) result [@@deriving show]

type lexer_result = (unit, fat_token option * string) result [@@deriving show]

type coerce = LeftBinary | Left | Binary | Right | RightBinary [@@deriving show]

type triple_coerce = {
	left : coerce;
	self : coerce;
	right : coerce;
} [@@deriving show]

type t = {
	reader : Reader.t;
	mutable buf : fat_token list;
	mutable last_triple : triple_coerce;
	frame : fat_token Vector.t;
	coercion_ops : triple_coerce Vector.t;
} [@@deriving show]

let eof = { token = Token.Eof; pos = -1; line = -1; col = -1 }

let init (flow : _ Eio.Flow.source) (max_size : int) : t = {
	reader = Reader.init flow max_size;
	buf = [];
	last_triple = { left = Binary; self = Binary; right = Left };
	frame = Vector.from_capacity 10 { token = Token.Unknown; pos = -1; line = -1; col = -1 };
	coercion_ops = Vector.init ();
}

let _resolve (l : t) : resolve_result =
	(* Private helper function definitions: *)
	let get_fat_tok () : (fat_token, string) result =
		Reader.skip_whitespace l.reader;
		let pos = Reader.pos l.reader in
		let line = Reader.line l.reader in
		let col = Reader.col l.reader in
		match Token.tokenize l.reader with
		| Ok Token.Eof -> Ok { token = Token.Eof; pos = -1; line = -1; col = -1 }
		| Ok token -> Ok { token = token; pos = pos; line = line; col = col }
		| Error e -> Error e
	in

	(* Read a full frame bounded by 2 unambiguous tokens *)
	let read_frame (frame : fat_token Vector.t) : (unit, string) result =
		let rec read_frame (frame : fat_token Vector.t) : (unit, string) result =
			match get_fat_tok () with
			| Ok fat -> (
				Vector.push fat frame;
				(* Also works for Eof case since its non ambiguous or will result in [Eof, Eof] which auto returns *)
				if Token.ambiguous fat.token then (
					read_frame frame
				) else Ok ()
			)
			| Error e -> Error e
		in read_frame frame
	in

	(* Given a token, what must the token to the right of it look like? -> used on left bound *)
	let right_binding ({token; _} : fat_token) : coerce =
		if Token.id token || Token.literal token || (Token.scope token) < 0 || Token.right token then (
			RightBinary
		) else if Token.symbol token || Token.keyword token || Token.left token|| Token.binary token then (
			Left
		) else (
			Binary (* Default binary? Reaching this branch is a syntax error anyway *)
		)
	in

	(* Given a token, what must the token to the left of it look like? -> used on right bound *)
	let left_binding ({token; _} : fat_token) : coerce =
		if Token.id token || Token.literal token || (Token.scope token) > 0 || Token.left token then (
			LeftBinary
		) else if Token.symbol token || Token.keyword token || Token.right token || Token.binary token then (
			Right
		) else (
			Binary
		)
	in

	(* Given two equivalent definitions for a token, what is the overlapping definition? *)
	let collapse (x : coerce) (y : coerce) : (coerce, string) result =
		if x = y then Ok x
		else match x with
		| LeftBinary -> (
			match y with
			| Right -> Error "incomprehensible ambiguity collapse"
			| RightBinary | LeftBinary | Binary -> Ok Binary
			| _ -> Ok Left
		)
		| Left -> (
			match y with
			| Binary | Right | RightBinary -> Error "incomprehensible ambiguity collapse"
			| _ -> Ok Left
		)
		| Binary -> (
			match y with
			| Left | Right -> Error "incomprehensible ambiguity collapse"
			| _ -> Ok Binary
		)
		| Right -> (
			match y with
			| Binary | Left | LeftBinary -> Error "incomprehensible ambiguity collapse"
			| _ -> Ok Right
		)
		| RightBinary -> (
			match y with
			| Left -> Error "incomprehensible ambiguity collapse"
			| LeftBinary | RightBinary | Binary -> Ok Binary
			| _ -> Ok Right
		)
	in

	(* assuming the left/self information for the coercion ops to the right of i are populated *)
	(* populate i's left and (i - 1)'s self *)
	let right_abstract_bind (frame : fat_token Vector.t) (coercion_ops : triple_coerce Vector.t) (i : int) : lexer_result =
		let right_abstract_binding (frame : fat_token Vector.t) (coercion_ops : triple_coerce Vector.t) (i : int) : (coerce, string) result =
			let self_triple = (Vector.get i coercion_ops) in
			let tok = (Vector.get i frame).token in
			if Token.id tok || Token.literal tok || (Token.scope tok) < 0 then (
				Ok RightBinary
			) else if Token.symbol tok || Token.keyword tok then (
				Ok Left
			) else (
				match self_triple.self with
				| LeftBinary | RightBinary -> Error "invalid self state after collapse"
				| Left | Binary -> Ok Left
				| Right -> Ok RightBinary
			)
		in

		let right = (Vector.get i coercion_ops).right in
		let left = (Vector.get (i + 2) coercion_ops).left in
		match collapse right left with
		| Ok self -> (
			match right_abstract_binding frame coercion_ops (i + 1) with
			| Ok right ->
				Ok (Vector.set { (Vector.get (i + 1) coercion_ops) with self = self; right = right; } (i + 1) coercion_ops)
			| Error e -> Error (Some (Vector.get (i + 1) frame), e)
		)
		| Error e -> Error (Some (Vector.get i frame), e)
	in

	(* assuming the right/self information for the coercion ops to the left of i are populated *)
	(* populate i's right and (i + 1)'s self *)
	let left_abstract_bind (frame : fat_token Vector.t) (coercion_ops : triple_coerce Vector.t) (i : int) : lexer_result =
		let left_abstract_binding (frame : fat_token Vector.t) (coercion_ops : triple_coerce Vector.t) (i : int) : (coerce, string) result =
			let self_triple = (Vector.get i coercion_ops) in
			let tok = (Vector.get i frame).token in
			if Token.id tok || Token.literal tok || (Token.scope tok) > 0 then (
				Ok LeftBinary
			) else if Token.symbol tok || Token.keyword tok then (
				Ok Right
			) else (
				match self_triple.self with
				| LeftBinary | RightBinary -> Error "invalid self state after collapse"
				| Left -> Ok LeftBinary
				| Binary | Right -> Ok Right
			)
		in

		let left = (Vector.get i coercion_ops).left in
		let right = (Vector.get (i - 2) coercion_ops).right in
		match collapse left right with
		| Ok self -> (
			match left_abstract_binding frame coercion_ops (i - 1) with
			| Ok left ->
				Ok (Vector.set { (Vector.get (i - 1) coercion_ops) with self = self; left = left; } (i - 1) coercion_ops)
			| Error e -> Error (Some (Vector.get (i - 1) frame), e)
		)
		| Error e -> Error (Some (Vector.get i frame), e)
	in

	(* Using the abstract_bind functions, go left-right and right-left to incrementally populate the coercion_ops array *)
	let propagate_coercion_ops (frame : fat_token Vector.t) (coercion_ops : triple_coerce Vector.t) : lexer_result =
		let rec loop (left_idx : int) (right_idx : int) : lexer_result =
			if left_idx >= ((Vector.size coercion_ops) - 2) || right_idx < 2 then Ok ()
			else (
				match right_abstract_bind frame coercion_ops left_idx with
				| Error e -> Error e
				| Ok _ -> (
					match left_abstract_bind frame coercion_ops right_idx with
					| Error e -> Error e
					| Ok _ -> loop (left_idx + 1) (right_idx - 1)
				)
			)
		in loop 0 ((Vector.size coercion_ops) - 1)
	in

	(* Apply all the coercions fromt he coercion_ops array to the frame *)
	let apply_coercions (frame : fat_token Vector.t) (coercion_ops : triple_coerce Vector.t) : lexer_result =
		let transform_fat f x = { x with token = (f x.token) } in
		let rec apply_coercions (c_idx : int) : lexer_result =
			let i = c_idx - 1 in
			if c_idx < (Vector.size coercion_ops) then
				match (Vector.get c_idx coercion_ops).self with
				| LeftBinary | RightBinary -> Error (Some (Vector.get i frame), "invalid self state")
				| Left ->
					Vector.transform_idx (transform_fat Token.leftify) i frame;
					apply_coercions (c_idx + 1)
				| Binary ->
					Vector.transform_idx (transform_fat Token.binaryify) i frame;
					apply_coercions (c_idx + 1)
				| Right ->
					Vector.transform_idx (transform_fat Token.rightify) i frame;
					apply_coercions (c_idx + 1)
			else Ok ()
		in apply_coercions 1 (* start at 1 to skip the injected last_triple *)
	in

	(* Actual code: *)
	(* Create and populate a frame *)
	match read_frame l.frame with
	| Error e -> Error (
		Some {
			token = Token.Unknown;
			pos = Reader.pos l.reader;
			line = Reader.line l.reader;
			col = Reader.col l.reader
		},
		e
	)
	| Ok () -> (
		let frame = l.frame in
		let coercion_ops = l.coercion_ops in
		let result =
			if (Vector.size frame) = 1 then (
				(* If no ambiguous tokens were actually read, no need to do all that work *)
				Ok (Vector.to_list frame)
			) else (
				(* Expand vector to operate on *)
				Vector.overwrite ((Vector.size frame) + 1) l.last_triple coercion_ops;
				(* First element pre-populated *)
				let right_bound = Vector.back frame in
				Vector.set_back {
					left = left_binding right_bound;
					self = Binary; (* Shouldn't matter since the token isn't ambiguous *)
					right = right_binding right_bound; (* Unused since edge *)
				} coercion_ops;

				(* Populate the coercion_ops array left_bound -> and right_bound <- *)
				match propagate_coercion_ops frame coercion_ops with
				| Error e -> Error e
				| Ok _ -> (
					(* Attempt applying coercions and then return the vector as a list to be used with the lexer *)
					match apply_coercions frame coercion_ops with
					| Ok () -> (
						l.last_triple <- Vector.back coercion_ops;
						Ok (Vector.to_list frame)
					)
					| Error e -> Error e
				)
			)
		in
		Vector.clear frame;
		result
	)

let pop (l : t) : (fat_token, fat_token option * string) result =
	match l.buf with
	| { token = Token.Eof; _ } as eof :: _ ->
		Ok eof
	| h::t ->
		l.buf <- t;
		Ok h
	| [] ->
		match _resolve l with
		| Ok ({ token = Token.Eof; _ } as eof :: _) ->
			(* Cache single eof so subsequent calls short circuit into eof *)
			l.buf <- [eof];
			Ok eof
		| Ok (h::t) ->
			l.buf <- t;
			Ok h
		| Ok [] ->
			Error (None, "empty frame from resolver")
		| Error e ->
			Error e

let push tok (l : t) : unit =
	l.buf <- (tok :: l.buf)

let peek (l : t) : (fat_token, fat_token option * string) result =
	match pop l with
	| Ok tok -> (
		push tok l;
		Ok tok
	)
	| Error e -> Error e
