exception SyntaxError of Shell_ast.bounds * string
exception IOError of string

type pres = Shell_ast.fat_node * (string list)

module StringMap = Map.Make(String)

type state = {
	parsed : Shell_ast.fat_node StringMap.t ref;
	pending : (string, unit) Hashtbl.t;
	mutex : Eio.Mutex.t;
}

type scope =
	| Root
	| ChillRoot
	| Type
	| Enum
	| AnonymousEnum
	| Tuple
	| Space
	| Code
	| Function
	| Match
	| FunctionParams
	| FunctionArguments
	| IndexArguments
	| ForParams
	| For
	| ArrayInitializer

(* TODO: tuples with one value must decay to just that value *)

(* TODO: pretty print *)
let syntax_error (first : Lexer.fat_token) (last : Lexer.fat_token) (msg : string) : 'a =
	raise (SyntaxError ((first, last), msg))

let io_error (msg : string) : 'a =
	raise (IOError msg)

let unwrap_token (s : Lexer.t) (start : Lexer.fat_token option) : Lexer.fat_token =
	match Lexer.pop s with
	| Error (last, msg) -> (
		match last with
		| Some last ->
			syntax_error
				(match start with | None -> last | Some f -> f) last
				msg
		| None ->
			io_error ("Lexer.pop fatally failed: " ^ msg)
	)
	| Ok last -> last

let unwrap_peek_token (s : Lexer.t) (start : Lexer.fat_token option) : Lexer.fat_token =
	match Lexer.peek s with
	| Error (last, msg) -> (
		match last with
		| Some last ->
			syntax_error
				(match start with | None -> last | Some f -> f) last
				msg
		| None ->
			io_error ("Lexer.peek fatally failed: " ^ msg)
	)
	| Ok last -> last

let discard_pop (s : Lexer.t) : unit =
	match Lexer.pop s with
	| Error (_, msg) ->
		io_error ("Lexer.pop fatally failed: " ^ msg)
	| _ -> ()

let expect_token (s : Lexer.t) (start : Lexer.fat_token option) (pred : Token.t -> 'b option) (msg : string) : 'b * Lexer.fat_token =
	let last = unwrap_token s start in
	match pred last.token with
	| Some thing -> (thing, last)
	| None ->
		syntax_error
			(match start with | None -> last | Some f -> f) last
			msg

let simple_expect_token (s : Lexer.t) (start : Lexer.fat_token option) (ty : Token.t) (msg : string) : Lexer.fat_token =
	let last = unwrap_token s start in
	if last.token = ty then (
		last
	) else (
		syntax_error
			(match start with | None -> last | Some f -> f) last
			msg
	)

let is_type_annotation (t : Token.t) : bool =
	match t with
	| Token.Scope | Token.Id _ | Token.Type | Token.Enum | Token.OpenParen | Token.OpenBrace -> true
	| _ -> false

let is_decl (tok : Token.t) : bool =
	match tok with
	| Token.Let
	| Token.Fn
	| Token.Type
	| Token.Priv
	| Token.Pub
	| Token.Stat
	| Token.Oper
	| Token.Enum
	| Token.Space
	| Token.Use
	| Token.Import -> true
	| _ -> false

let rec parse (allow_root_expr : bool) (imports : string list) (s : Lexer.t) : pres =
	if allow_root_expr then (
		parse_scope ChillRoot imports s
	) else (
		parse_scope Root imports s
	)

and parse_scope (scope : scope) (imports : string list) (s : Lexer.t) : pres =
	match scope with
	| Root -> (
		let single (imports : string list) (s : Lexer.t) : pres =
			let start = unwrap_peek_token s None in
			match start.token with
			| Token.Stat ->
				syntax_error
					start start
					"'stat' makes no sense in the file's root context."
			| _ -> parse_decl imports s
		in

		let rec loop (s : Lexer.t) (last : Lexer.fat_token) (imports : string list) (acc : Shell_ast.fat_node list) : (Shell_ast.fat_node list) * (string list) =
			let last = unwrap_peek_token s (Some last) in
			match last.token with
			| Token.Eof -> (acc, imports)
			| _ -> (
				let (result, imports) = single imports s in
				loop s (Shell_ast.last result) imports (result :: acc)
			)
		in

		let start = unwrap_peek_token s None in
		let (nodes, imports) = loop s start imports [] in
		let last =
			match nodes with
			| [] -> None
			| h::_ -> Some (Shell_ast.last h)
		in
		((start, (
			match last with
			| Some l -> l
			| None -> start
		)), Shell_ast.Block (List.rev nodes)), imports
	)
	| ChillRoot -> (
		let single (imports : string list) (s : Lexer.t) : pres =
			let start = unwrap_peek_token s None in
			match start.token with
			| Token.Stat ->
				syntax_error
					start start
					"'stat' makes no sense in the file's root context."
			| t when is_decl t -> parse_decl imports s
			| _ -> parse_expr imports s
		in

		let rec loop (s : Lexer.t) (last : Lexer.fat_token) (imports : string list) (acc : Shell_ast.fat_node list) : (Shell_ast.fat_node list) * (string list) =
			let last = unwrap_peek_token s (Some last) in
			match last.token with
			| Token.Eof -> (acc, imports)
			| _ -> (
				let (result, imports) = single imports s in
				loop s (Shell_ast.last result) imports (result :: acc)
			)
		in

		let start = unwrap_peek_token s None in
		let (nodes, imports) = loop s start imports [] in
		let last =
			match nodes with
			| [] -> None
			| h::_ -> Some (Shell_ast.last h)
		in
		((start, (
			match last with
			| Some l -> l
			| None -> start
		)), Shell_ast.Block (List.rev nodes)), imports
	)
	| Type -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"'type ...' must be followed by '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
			| _ -> (
				let (result, imports) = parse_decl imports s in
				loop s last imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| Enum -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"'enum ...' must be followed by '{'."
		in

		let rec loop (s : Lexer.t) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list)=
			let start = unwrap_peek_token s None in
			match start.token with
			| Token.ClosedBrace -> (discard_pop s; (start, List.rev acc, imports))
			| _ -> (
				let (name, start) =
					expect_token s (Some start) (function | Token.Id name -> Some name | _ -> None)
						"A variant in an enum must start with an id, followed by the type it represents."
				in
				let (annotation, imports) = parse_type_annotation imports s in
				loop s imports ((
					(start, Shell_ast.last annotation),
					Shell_ast.EnumVariant {
						name = name;
						type_ = annotation;
					}
				) :: acc)
			)
		in

		let (last, nodes, imports) = loop s imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| AnonymousEnum -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"anonymous enum annotation must start with '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let start = unwrap_peek_token s (Some start) in
			match start.token with
			| Token.ClosedBrace -> (discard_pop s; (start, List.rev acc, imports))
			| Token.Comma -> (
				if comma_found then (
					syntax_error
						start start
						"only one comma (+ one trailing) can delimit types in a anonymous enum."
				) else (
					(discard_pop s; loop s start true imports acc)
				)
			)
			| _ -> (
				let (result, imports) = parse_type_annotation imports s in
				loop s start false imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start false imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| Tuple -> (
		let start =
			simple_expect_token s None Token.OpenParen
				"tuple annotation must start with '('."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let start = unwrap_peek_token s (Some start) in
			match start.token with
			| Token.ClosedParen -> (discard_pop s; (start, List.rev acc, imports))
			| Token.Comma -> (
				if comma_found then (
					syntax_error
						start start
						"only one comma (+ one trailing) can delimit types in a tuple."
				) else (
					(discard_pop s; loop s start true imports acc)
				)
			)
			| _ -> (
				let (result, imports) = parse_type_annotation imports s in
				loop s start false imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start false imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| Space -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"'space ...' must be followed by '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
			| Token.Stat ->
				syntax_error last last
					"'stat' cannot be used shallowly inside a space block (since there is no clear meaning of what that would accomplish)."
			| _ -> (
				let (result, imports) = parse_decl imports s in
				loop s last imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| Code -> (
		let start = unwrap_peek_token s None in
		match start.token with
		| Token.OpenBrace -> (
			discard_pop s;
			let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
				let last = unwrap_peek_token s (Some start) in
				match last.token with
				| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
				| Token.SemiColon -> loop s last imports acc
				| _ -> (
					let (any, imports) = parse_scope Code imports s in
					loop s (Shell_ast.last any) imports (any :: acc)
				)
			in
			let (last, nodes, imports) = loop s start imports [] in
			((start, last), Shell_ast.Block nodes), imports
		)
		| t when is_decl t -> parse_decl imports s
		| _ -> parse_expr imports s
	)
	| Function -> (
		let start = unwrap_peek_token s None in
		match start.token with
		| Token.AssignmentEquals -> (discard_pop s; parse_scope Code imports s)
		| Token.OpenBrace -> parse_scope Code imports s
		| _ ->
			syntax_error start start
				"function implementation must start with '{' | '='."
	)
	| Match -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"'match ...' must be followed by '{'."
		in
		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
			| _ -> (
				let (expr, imports) = parse_expr imports s in
				let (arrow, last) =
					expect_token s (Some start) (function | Token.Arrow -> Some true | Token.Comma -> Some false | _ -> None)
						"match case expression must be followed by '->' (don't fallthrough) or ',' (fallthrough)."
				in
				let (result, imports) =
					if arrow then (
						let (logic, imports) = parse_scope Code imports s in
						(
							(start, Shell_ast.last logic),
							Shell_ast.MatchCase {
								expr = expr;
								logic = Some logic;
							}
						), imports
					)
					else (
						(
							(start, last),
							Shell_ast.MatchCase {
								expr = expr;
								logic = None;
							}
						), imports
					)
				in
				loop s last imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| FunctionParams -> (
		let start =
			simple_expect_token s None Token.OpenParen
				"function parameter list must start with '('."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let start = unwrap_peek_token s (Some start) in
			match start.token with
			| Token.ClosedParen -> (discard_pop s; (start, List.rev acc, imports))
			| Token.Comma -> (
				if comma_found then (
					syntax_error
						start start
						"only one comma (+ one trailing) can delimit function parameters."
				) else (
					(discard_pop s; loop s start true imports acc)
				)
			)
			| _ -> (
				let (result, imports) = parse_variable imports s in
				loop s start false imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start false imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| FunctionArguments -> (
		let start =
			simple_expect_token s None Token.OpenParen
				"function call argument list must start with '('."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let start = unwrap_peek_token s (Some start) in
			match start.token with
			| Token.ClosedParen -> (start, List.rev acc, imports)
			| Token.Comma -> (
				if comma_found then (
					syntax_error
						start start
						"only one comma (+ one trailing) can delimit function parameters."
				) else (
					(discard_pop s; loop s start true imports acc)
				)
			)
			| _ -> (
				let (result, imports) = parse_expr imports s in
				loop s start false imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start false imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| IndexArguments -> (
		let start =
			simple_expect_token s None Token.OpenBracket
				"index argument list must start with '['."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let start = unwrap_peek_token s (Some start) in
			match start.token with
			| Token.ClosedBracket -> (start, List.rev acc, imports)
			| Token.Comma -> (
				if comma_found then (
					syntax_error
						start start
						"only one comma (+ one trailing) can delimit function parameters."
				) else (
					(discard_pop s; loop s start true imports acc)
				)
			)
			| _ -> (
				let (result, imports) = parse_expr imports s in
				loop s start false imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start false imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)
	| ForParams -> (
		(* a; b; c -> {a}; {b}; {c} *)
		let (init, imports) = parse_scope Code imports s in
		let (condition, imports) = parse_expr imports s in
		let (iter, imports) = parse_scope Code imports s in
		((Shell_ast.start init, Shell_ast.last iter), Shell_ast.Block [init; condition; iter]), imports
	)
	| For -> (
		let start = unwrap_peek_token s None in
		match start.token with
		| Token.OpenBrace -> (
			discard_pop s;
			let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
				let last = unwrap_peek_token s (Some start) in
				match last.token with
				| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
				| Token.SemiColon -> (discard_pop s; loop s last imports acc) (* discard and continue *)
				| _ -> (
					let (any, imports) = parse_scope For imports s in
					loop s (Shell_ast.last any) imports (any :: acc)
				)
			in
			let (last, nodes, imports) = loop s start imports [] in
			((start, last), Shell_ast.Block nodes), imports
		)
		| Token.Break -> (
			discard_pop s;
			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.SemiColon -> ((start, last), Shell_ast.Break None), imports
			| _ -> (
				let (expr, imports) = parse_expr imports s in
				((start, Shell_ast.last expr), Shell_ast.Break (Some expr)), imports
			)
		)
		| Token.Continue -> (
			discard_pop s;
			((start, start), Shell_ast.Continue), imports
		)
		| t when is_decl t -> parse_decl imports s
		| _ -> parse_expr imports s
	)
	| ArrayInitializer -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"array initializer must start with '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Shell_ast.fat_node list) : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
			let start = unwrap_peek_token s (Some start) in
			match start.token with
			| Token.ClosedBrace -> (discard_pop s; (start, List.rev acc, imports))
			| Token.Comma -> (
				if comma_found then (
					syntax_error
						start start
						"only one comma (+ one trailing) can delimit elements in an array initializer."
				) else (
					(discard_pop s; loop s start true imports acc)
				)
			)
			| _ -> (
				let (result, imports) = parse_expr imports s in
				loop s start false imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start false imports [] in
		((start, last), Shell_ast.Block nodes), imports
	)

and parse_decl (imports : string list) (s : Lexer.t) : pres =
	let parse_decl_atom (imports : string list) (s : Lexer.t) : pres =
		let start = unwrap_token s None in
		match start.token with
		| Token.SemiColon -> parse_decl imports s
		| Token.Let -> parse_variable imports s
		| Token.Fn -> parse_function imports s
		| Token.Type -> parse_type imports s
		| Token.Priv | Token.Pub | Token.Stat -> parse_access_mod start imports s
		| Token.Oper -> parse_operator_overload imports s
		| Token.Enum -> parse_enum imports s
		| Token.Space -> parse_space imports s
		| Token.Use -> parse_use imports s
		| Token.Import -> parse_import imports s
		| _ ->
			syntax_error
				start start
				"declaration must start with 'let' | 'fn' | 'type' | 'priv' | 'pub' | 'stat' | 'oper' | 'enum' | 'space' | 'use' | 'import'."
	in

	let decl = parse_decl_atom imports s in
	let next = unwrap_peek_token s None in
	match next.token with
	| Token.SemiColon -> (discard_pop s; decl)
	| _ -> decl

and parse_variable (imports : string list) (s : Lexer.t) : pres =
	(*
		let name : type = value
		let name = value
		let name : type
	*)
	let (name, start) =
		expect_token s None (function | Token.Id name -> Some name | _ -> None) "'let' must be followed by an id." in
	let last = unwrap_token s (Some start) in
	match last.token with
	| Token.AssignmentEquals ->
		let (value, imports) = parse_expr imports s in
		((start, (Shell_ast.last value)), Shell_ast.Variable {
			name = name;
			type_ = None;
			value = Some value;
		}), imports
	| t when (is_type_annotation t) -> (
		Lexer.push last s;
		let (type_, imports) = parse_type_annotation imports s in

		let last = unwrap_token s (Some start) in
		match last.token with
		| Token.AssignmentEquals ->
			let (value, imports) = parse_expr imports s in
			((start, (Shell_ast.last value)), Shell_ast.Variable {
				name = name;
				type_ = Some type_;
				value = Some value;
			}), imports
		| _ -> (
			Lexer.push last s;
			((start, (Shell_ast.last type_)), Shell_ast.Variable {
				name = name;
				type_ = Some type_;
				value = None;
			}), imports
		)
	)
	| _ ->
		syntax_error
			start last
			(Printf.sprintf "'let %s' must be followed by a type annotation or '='." name)

and parse_function (imports : string list) (s : Lexer.t) : pres =
	(*
		fn name(param type) type {}
		fn name(param type) {}
		fn name() type = value
		fn name() = value
		fn name(constant) = value (acts as case like haskell ykwim) TODO
	*)
	let (name, start) =
		expect_token s None (function | Token.Id name -> Some name | _ -> None) "'fn' must be followed by a single unchained id." in
	let (params, imports) = parse_scope FunctionParams imports s in
	let (result, imports) = (
		let last = unwrap_peek_token s (Some start) in
		match last.token with
		| Token.OpenBrace | Token.AssignmentEquals -> (None, imports) (* To annotate an anoynous unnamed variant, surround with parenthesis to make a single entry tuple *)
		| t when (is_type_annotation t) -> (
			let (res, imports) = parse_type_annotation imports s in
			(Some res, imports)
		)
		| _ -> (None, imports)
	) in

	let (code, imports) = parse_scope Function imports s in
	(
		(start, Shell_ast.last code),
		Shell_ast.Function {
			name = name;
			params = params;
			result = result;
			code = code;
		}
	), imports

and parse_type (imports : string list) (s : Lexer.t) : pres =
	let (name, start) =
		expect_token s None (function | Token.Id name -> Some name | _ -> None) "'type' must be followed by an unchained id." in
	let (next, last) =
		expect_token s (Some start) (fun (t : Token.t) -> match t with | Token.OpenBrace | Token.AssignmentEquals -> Some t | _ -> None)
			(Printf.sprintf "'type %s' must be followed by '{' or '='" name)
	in
	match next with
	| Token.OpenBrace -> (
		Lexer.push last s;
		let (members, imports) = parse_scope Type imports s in
		(
			(start, Shell_ast.last members),
			Shell_ast.Type {
				name = name;
				members = members;
			}
		), imports
	)
	| Token.AssignmentEquals -> (
		let (annotation, imports) = parse_type_annotation imports s in
		(
			(start, (Shell_ast.last annotation)),
			Shell_ast.Alias (
				name,
				annotation
			)
		), imports
	)
	| _ -> syntax_error start last "unreachable"

and parse_access_mod (start : Lexer.fat_token) (imports : string list) (s : Lexer.t) : pres =
	match start.token with
	| Token.Pub -> (
		let (d, imports) = parse_decl imports s in
		((start, Shell_ast.last d), Shell_ast.Pub d), imports
	)
	| Token.Priv -> (
		let (d, imports) = parse_decl imports s in
		((start, Shell_ast.last d), Shell_ast.Priv d), imports
	)
	| Token.Stat -> (
		let (d, imports) = parse_decl imports s in
		((start, Shell_ast.last d), Shell_ast.Stat d), imports
	)
	| _ ->
		syntax_error
			start start
			"expected either 'pub' | 'priv' | 'stat'."

and parse_operator_overload (imports : string list) (s : Lexer.t) : pres =
	(* oper (+a type) return_type {} *)
	(* oper (a type+) return_type {} *)
	(* oper (a type + b type) return_type {} *)
	let start = unwrap_peek_token s None in
	let _ =
		simple_expect_token s (Some start) Token.OpenParen
			"'oper' must by followed by '('."
	in
	let (oper, params, imports) =
		let last = unwrap_peek_token s (Some start) in
		match last.token with
		| t when (Token.oper t) -> (
			discard_pop s;
			let oper = { last with token = (Token.leftify t) } in

			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.Let -> (
				discard_pop s;
				let (var, imports) = parse_variable imports s in
				(oper, [var], imports)
			)
			| _ -> (
				let (var, imports) = parse_variable imports s in
				(oper, [var], imports)
			)
		)
		| _ -> (
			let (first, imports) =
				match start.token with
				| Token.Let -> (discard_pop s; parse_variable imports s)
				| _ -> parse_variable imports s
			in

			let oper =
				let last = unwrap_token s (Some start) in
				match last.token with
				| t when (Token.oper t) ->
					{ last with token = (Token.rightify t) }
				| _ ->
					syntax_error
						start last
						"expected binary operator to separate parameters or a right unary operator in the operator overload."
			in

			let (params, len, imports) =
				let last = unwrap_peek_token s (Some start) in
				match last.token with
				| Token.Let -> (
					discard_pop s;
					let (var, imports) = parse_variable imports s in
					([first; var], 2, imports)
				)
				| Token.Id _ -> (
					let (var, imports) = parse_variable imports s in
					([first; var], 2, imports)
				)
				| _ -> ([first], 1, imports)
			in

			let oper =
				if len > 1 then (
					{ oper with token = (Token.binaryify oper.token) }
				) else (
					oper
				)
			in

			(oper, params, imports)
		)
	in

	let _ =
		simple_expect_token s (Some start) Token.ClosedParen
			"operator overload parameters and operator must be wrapped in '()'. you need a ')'."
	in
	let last = unwrap_peek_token s (Some start) in
	let (result, imports) =
		match last.token with
		| Token.OpenBrace -> (None, imports) (* non () enclosed anonymous enums not allowed *)
		| t when is_type_annotation t -> (
			let (res, imports) = parse_type_annotation imports s in
			(Some res, imports)
		)
		| _ ->
			syntax_error
				start last
				"'oper (...)' must be followed by '{' or a type annotation."
	in
	let (code, imports) = parse_scope Function imports s in
	(
		(start, Shell_ast.last code),
		Shell_ast.OperatorOverload {
			name = oper.token;
			params = params;
			result = result;
			code = code;
		}
	), imports

and parse_enum (imports : string list) (s : Lexer.t) : pres =
	let (name, start) =
		expect_token s None (function | Token.Id name -> Some name | _ -> None)
			"'enum' must be followed by an id [a-zA-Z_][a-zA-Z0-9_]*."
	in
	let (members, imports) = parse_scope Enum imports s in
	(
		(start, Shell_ast.last members),
		Shell_ast.Enum {
			name = name;
			members = members;
		}
	), imports

and parse_space (imports : string list) (s : Lexer.t) : pres =
	let start = unwrap_peek_token s None in
	let (_, name) = parse_id s in
	let (members, imports) = parse_scope Space imports s in
	(
		(start, Shell_ast.last members),
		Shell_ast.Space {
			name = name;
			members = members;
		}
	), imports

and parse_use (imports : string list) (s : Lexer.t) : pres =
	let start = unwrap_peek_token s None in
	let (last, use) = parse_id s in
	((start, last), Shell_ast.Use use), imports

and parse_import (imports : string list) (s : Lexer.t) : pres =
	(* import "<path>" *)
	let (path, start) =
		expect_token s None (function | Token.String s -> Some s | _ -> None)
			"'import' must be followed by a filepath."
	in
	(
		(start, start),
		Shell_ast.Import {
			path = path;
		}
	), (path :: imports)

and parse_if (imports : string list) (s : Lexer.t) : pres =
	(* if expr any || <if> else any *)
	let start = unwrap_peek_token s None in
	let (condition, imports) = parse_expr imports s in
	let (true_block, imports) = parse_scope Code imports s in
	let (false_block, imports) =
		let last = unwrap_peek_token s (Some start) in
		match last.token with
		| Token.Else -> (
			discard_pop s;
			let (res, imports) = parse_scope Code imports s in
			(Some res, imports)
		)
		| _ -> (None, imports)
	in
	(
		(start, match false_block with | Some fb -> (Shell_ast.last fb) | None -> (Shell_ast.last true_block)),
		Shell_ast.If {
			condition = condition;
			true_block = true_block;
			false_block = false_block;
		}
	), imports

and parse_match (imports : string list) (s : Lexer.t) : pres =
	(*
		match switcher {
			expr, expr2 -> logic
		}
	*)
	let (switcher, imports) = parse_expr imports s in
	let (cases, imports) = parse_scope Match imports s in
	(
		(Shell_ast.start switcher, Shell_ast.last cases),
		Shell_ast.Match {
			switcher = switcher;
			cases = cases;
		}
	), imports

and parse_for (imports : string list) (s : Lexer.t) : pres =
	let (params, imports) = parse_scope ForParams imports s in
	let (block, imports) = parse_scope For imports s in
	(
		(Shell_ast.start params, Shell_ast.last block),
		Shell_ast.For {
			params = params;
			block = block;
		}
	), imports

and parse_until (imports : string list) (s : Lexer.t) : pres =
	(* until expr { ... } *)
	let (condition, imports) = parse_expr imports s in
	let (block, imports) = parse_scope Code imports s in
	(
		(Shell_ast.start condition, Shell_ast.last block),
		Shell_ast.Until {
			condition = condition;
			block = block;
		}
	), imports

and parse_tuple (imports : string list) (expr : Shell_ast.fat_node) (s : Lexer.t) : pres =
	let rec parse_tuple (imports : string list) (exprs : Shell_ast.fat_node list) (s : Lexer.t) : (Lexer.fat_token * (Shell_ast.fat_node list) * (string list)) =
		let (expr, imports) = parse_expr_bp imports s max_int in
		let exprs = expr :: exprs in
		let next = unwrap_peek_token s None in
		match next.token with
		| Token.Comma -> (discard_pop s; parse_tuple imports exprs s)
		| _ -> (Shell_ast.last expr, exprs, imports)
	in
	let (last, exprs, imports) = parse_tuple imports [expr] s in
	(((Shell_ast.start expr, last), Shell_ast.TupleExpr exprs), imports)

and parse_expr (imports : string list) (s : Lexer.t) : pres =
	let (expr, imports) = parse_expr_bp imports s max_int in
	let next = unwrap_peek_token s None in
	match next.token with
	| Token.SemiColon -> (discard_pop s; (expr, imports))
	| _ -> (expr, imports)

and parse_expr_bp (imports : string list) (s : Lexer.t) (max_bp : int) : pres =
	let binary_bp (tok : Token.t) : int * int =
		match Token.bp tok with
		| (Some a, Some b) -> (a, b)
		| _ -> failwith "expected binary operator token."
	in

	let left_bp (tok : Token.t) : int =
		match Token.bp tok with
		| (None, Some a) -> a
		| _ -> failwith "expected left unary operator token."
	in

	let right_bp (tok : Token.t) : int =
		match Token.bp tok with
		| (Some a, None) -> a
		| _ -> failwith "expected right unary operator token."
	in

	let (lhs, imports) =
		let start = unwrap_peek_token s None in
		match start.token with
		| t when Token.left t -> (
			discard_pop s;
			let r_bp = left_bp t in
			let (arg, imports) = parse_expr_bp imports s r_bp in
			(
				(
					(start, Shell_ast.last arg),
					Shell_ast.Unary { op = t; arg = Some arg }
				),
				imports
			)
		)
		| Token.OpenParen -> (
			discard_pop s;
			let (first, imports) = parse_expr imports s in
			let last = unwrap_peek_token s None in
			match last.token with
			| Token.Comma -> (
				discard_pop s;
				let (((_, last), expr), imports) = parse_tuple imports first s in
				let last =
					simple_expect_token s (Some last) Token.ClosedParen
						"expected ')'"
				in
				(((start, last), expr), imports)
			)
			| Token.ClosedParen -> (
				discard_pop s;
				let (_, expr) = first in
				(((start, last), expr), imports)
			)
			| _ -> syntax_error start last
				"expected ')'"
		)
		| Token.OpenBrace -> parse_scope Code imports s
		| _ -> (
			let (atom, imports) = parse_atom imports s in
			match atom with
			| Some expr -> (
				let (expr, imports) = parse_postfix imports s expr in
				(expr, imports)
			)
			| None -> (
				let start = unwrap_peek_token s None in
				syntax_error start start "expected expression."
			)
		)
	in

	(* Parse infix/postfix operators *)
	let rec loop lhs imports =
		let peek = unwrap_peek_token s None in
		match peek.token with
		| t when Token.binary t ->
			let (l_bp, r_bp) = binary_bp t in
			if l_bp > max_bp then
				(lhs, imports)
			else (
				discard_pop s;
				let (rhs, imports) = parse_expr_bp imports s r_bp in
				let new_lhs = (
					(Shell_ast.start lhs, Shell_ast.last rhs),
					Shell_ast.Binary { left = lhs; op = t; right = Some rhs }
				) in
				loop new_lhs imports
			)
		| t when Token.right t ->  (* postfix unary *)
			let l_bp = right_bp t in
			if l_bp > max_bp then
				(lhs, imports)
			else (
				discard_pop s;
				let new_lhs = (
					(Shell_ast.start lhs, peek),
					Shell_ast.Unary { op = t; arg = Some lhs }
				) in
				loop new_lhs imports
			)
		| _ -> (lhs, imports)
	in
	loop lhs imports

and parse_atom (imports : string list) (s : Lexer.t) : (Shell_ast.fat_node option) * (string list) =
	let start = unwrap_token s None in
	match start.token with
	| Token.Integer _ -> (Some ((start, start), Shell_ast.Integer start), imports)
	| Token.Float _ -> (Some ((start, start), Shell_ast.Float start), imports)
	| Token.Character _ -> (Some ((start, start), Shell_ast.Character start), imports)
	| Token.String _ -> (Some ((start, start), Shell_ast.String start), imports)
	| Token.True -> (Some ((start, start), Shell_ast.True start), imports)
	| Token.False -> (Some ((start, start), Shell_ast.False start), imports)
	| Token.If -> (
		let (res, imports) = parse_if imports s in
		(Some res, imports)
	)
	| Token.Until -> (
		let (res, imports) = parse_until imports s in
		(Some res, imports)
	)
	| Token.For -> (
		let (res, imports) = parse_for imports s in
		(Some res, imports)
	)
	| Token.Match -> (
		let (res, imports) = parse_match imports s in
		(Some res, imports)
	)
	| Token.OpenBracket -> (
		let (size, imports) =
			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.ClosedBracket -> (None, imports)
			| _ -> (
				let (expr, imports) = parse_expr imports s in
				(Some expr, imports)
			)
		in
		let _ =
			simple_expect_token s None Token.ClosedBracket
				"'[...]' array initializer starting must enclose either nothing or an integral expression, followed by ']'."
		in
		let (_, type_) = parse_id s in
		let (args, imports) = parse_scope ArrayInitializer imports s in
		(Some ((start, Shell_ast.last args), Shell_ast.ArrayInitializer { size = size; type_ = type_; args = args }), imports)
	)
	| Token.Id _ | Token.Scope -> (
		Lexer.push start s;
		let (ref_last, ref) = parse_id s in
		(Some ((start, ref_last), Shell_ast.Reference ref), imports)
	)
	| _ -> (None, imports)

and parse_postfix (imports : string list) (s : Lexer.t) (acc : Shell_ast.fat_node) : pres =
	let start = unwrap_peek_token s (Some (Shell_ast.start acc)) in
	match start.token with
	| Token.OpenParen -> (
		(* discard_pop s; *)
		let (args, imports) = parse_scope FunctionArguments imports s in
		let last =
			simple_expect_token s (Some (Shell_ast.start args)) Token.ClosedParen
				"function call expression must be closed off with ')' to match the opening '('."
		in
		parse_postfix imports s (
			(Shell_ast.start acc, last), Shell_ast.Call {
				from = acc;
				args = args;
			}
		)
	)
	| Token.OpenBracket -> (
		(* discard_pop s; *)
		let (args, imports) = parse_scope IndexArguments imports s in
		let last =
			simple_expect_token s (Some (Shell_ast.start args)) Token.ClosedBracket
				"index expression must be closed off with ']' to match the opening '['."
		in
		parse_postfix imports s (
			(Shell_ast.start acc, last), Shell_ast.Index {
				from = acc;
				args = args;
			}
		)
	)
	| Token.OpenBrace -> (
		match acc with
		| (_, Shell_ast.Reference _) -> (
			let initializer_parse_scope (imports : string list) (s : Lexer.t) : Lexer.fat_token * ((string * Shell_ast.fat_node) list) * (string list) =
				let single (imports : string list) (s : Lexer.t) : (string * Shell_ast.fat_node * (string list)) =
					let (member, last) =
						expect_token s (Some start) (function | Token.Id name -> Some name | _ -> None)
							"initializer member assignment must start with the member name, followed by ':', and then a value to assign it."
					in

					let _ =
						simple_expect_token s (Some last) Token.Colon
							"colon needs to delimit the member name and the value in an initializer."
					in

					let (value, imports) = parse_expr imports s in
					(member, value, imports)
				in

				let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : (string * Shell_ast.fat_node) list) : Lexer.fat_token * ((string * Shell_ast.fat_node) list) * (string list) =
					let start = unwrap_peek_token s (Some start) in
					match start.token with
					| Token.ClosedBrace -> (discard_pop s; (start, List.rev acc, imports))
					| Token.Comma -> (
						if comma_found then (
							syntax_error
								start start
								"only one comma (+ one trailing) can delimit argument assignments in an initializer."
						) else (
							(discard_pop s; loop s start true imports acc)
						)
					)
					| _ -> (
						let (thing_tag, thing, imports) = single imports s in
						loop s start false imports ((thing_tag, thing) :: acc)
					)
				in

				let (last, nodes, imports) = loop s start false imports [] in
				(last, nodes, imports)
			in
			discard_pop s;
			let (last, args, imports) = initializer_parse_scope imports s in
			parse_postfix imports s (
				(Shell_ast.start acc, last), Shell_ast.Initializer {
					type_ = acc;
					args = args;
				}
			)
		)
		| _ -> (acc, imports)
	)
	| _ -> (acc, imports)

and parse_type_annotation (imports : string list) (s : Lexer.t) : pres =
	let raw_type_annotation (imports : string list) (s : Lexer.t) =
		let start = unwrap_token s None in
		match start.token with
		| Token.Type -> (
			let (members, imports) = parse_scope Type imports s in
			(
				(start, Shell_ast.last members),
				Shell_ast.Type {
					name = "";
					members = members;
				}
			), imports
		)
		| Token.Enum -> (
			let (members, imports) = parse_scope Enum imports s in
			(
				(start, Shell_ast.last members),
				Shell_ast.Enum {
					name = "";
					members = members;
				}
			), imports
		)
		| Token.OpenParen -> (
			Lexer.push start s;
			let (members, imports) = parse_scope Tuple imports s in
			((start, Shell_ast.last members), Shell_ast.Tuple members), imports
		)
		| Token.OpenBrace -> (
			Lexer.push start s;
			let (members, imports) = parse_scope AnonymousEnum imports s in
			((start, Shell_ast.last members), Shell_ast.AnonymousEnum members), imports
		)
		| _ -> (
			Lexer.push start s;
			let (last, id) = parse_id s in
			((start, last), Shell_ast.Id id), imports
		)
	in

	let start = unwrap_peek_token s None in
	match start.token with
	| t when ((Token.binaryify t) = Token.Multiply) -> (
		let (inner, imports) = raw_type_annotation imports s in
		(((start, Shell_ast.last inner), Pointer inner), imports)
	)
	| _ -> raw_type_annotation imports s

and parse_id (s : Lexer.t) : Lexer.fat_token * Shell_ast.id =
	(* Parse optional leading :: *)
	let start = unwrap_token s None in

	let (root, first_seg) =
		match start.token with
		| Token.Scope -> (
			(* must be followed by an identifier *)
			let tok = unwrap_token s (Some start) in
			match tok.token with
			| Token.Id name -> (true, (tok, name))
			| _ ->
				syntax_error start tok
					"expected identifier after '::'"
		)

		| Token.Id name ->
			(false, (start, name))

		| _ ->
			syntax_error start start
				"identifier must start with '::' or an identifier"
	in

	(* Parse zero or more :: id *)
	let rec loop (last_tok : Lexer.fat_token) (acc : string list) : Lexer.fat_token * (string list) =
		let next = unwrap_peek_token s (Some last_tok) in
		match next.token with
		| Token.Scope -> (
			discard_pop s;
			let tok = unwrap_token s (Some next) in
			match tok.token with
			| Token.Id name ->
				loop tok (name :: acc)
			| _ ->
				syntax_error next tok
					"expected identifier after '::'"
		)
		| _ ->
			(last_tok, List.rev acc)
	in

	let (last, chain) = loop (fst first_seg) [snd first_seg] in
	(last, (root, chain))

and stringify_id (id : Shell_ast.id) : string =
	let (root, chain) = id in
	let rec loop (chain : string list) : string =
		match chain with
		| [] -> ""
		| [h] -> h
		| h::t -> h ^ "::" ^ (loop t)
	in (if root then "::" else "") ^ (loop chain)

and stringify_node (node : Shell_ast.fat_node) : string =
	let (_, node) = node in
	match node with
	| Shell_ast.Program files ->
			"Program [\n" ^
			(String.concat ";\n" (List.map (fun (path, block) ->
				"	 " ^ path ^ " -> " ^ stringify_node block
			) files)) ^
			"\n]"

	| Shell_ast.Pub inner ->
			"pub " ^ stringify_node inner

	| Shell_ast.Priv inner ->
			"priv " ^ stringify_node inner

	| Shell_ast.Stat inner ->
			"stat " ^ stringify_node inner

	| Shell_ast.Id id ->
			stringify_id id

	| Shell_ast.Variable var ->
			"let " ^ var.name ^
			(match var.type_ with
			 | None -> ""
			 | Some t -> " : " ^ stringify_node t) ^
			(match var.value with
			 | None -> ""
			 | Some v -> " = " ^ stringify_node v)

	| Shell_ast.Type layer ->
			if layer.name = "" then
				"type { " ^ stringify_node layer.members ^ " }"
			else
				"type " ^ layer.name ^ " { " ^ stringify_node layer.members ^ " }"

	| Shell_ast.Alias (name, target) ->
			"type " ^ name ^ " = " ^ stringify_node target

	| Shell_ast.Tuple block ->
			"(" ^ stringify_node block ^ ")"

	| Shell_ast.Function fn ->
			"fn " ^ fn.name ^ stringify_node fn.params ^
			(match fn.result with
			 | None -> ""
			 | Some r -> " " ^ stringify_node r) ^
			" " ^ stringify_node fn.code

	| Shell_ast.OperatorOverload { name; params; result; code } ->
			"oper (" ^
			(if List.length params = 1 then
				Token.value name ^ stringify_node (List.hd params)
			else if List.length params = 2 then
				stringify_node (List.nth params 0) ^ " " ^
				Token.value name ^ " " ^
				stringify_node (List.nth params 1)
			else "") ^
			")" ^
			(match result with
			 | None -> ""
			 | Some r -> " " ^ stringify_node r) ^
			" " ^ stringify_node code

	| Shell_ast.EnumVariant { name; type_ } ->
			name ^ " " ^ stringify_node type_

	| Shell_ast.Enum layer ->
			if layer.name = "" then
				"enum { " ^ stringify_node layer.members ^ " }"
			else
				"enum " ^ layer.name ^ " { " ^ stringify_node layer.members ^ " }"

	| Shell_ast.AnonymousEnum block ->
			"{ " ^ stringify_node block ^ " }"

	| Shell_ast.Pointer inner ->
			"*" ^ (stringify_node inner)

	| Shell_ast.Space { name; members } ->
			"space " ^ stringify_id name ^ " { " ^ stringify_node members ^ " }"

	| Shell_ast.Use id ->
			"use " ^ stringify_id id

	| Shell_ast.Import { path } ->
			"import \"" ^ path ^ "\""

	| Shell_ast.If { condition; true_block; false_block } ->
			"if " ^ stringify_node condition ^
			" " ^ stringify_node true_block ^
			(match false_block with
			 | None -> ""
			 | Some fb -> " else " ^ stringify_node fb)

	| Shell_ast.Match { switcher; cases } ->
			"match " ^ stringify_node switcher ^ " { " ^ stringify_node cases ^ " }"

	| Shell_ast.MatchCase { expr; logic } ->
			stringify_node expr ^
			(match logic with
			 | None -> ","
			 | Some l -> " -> " ^ stringify_node l)

	| Shell_ast.For { params; block } ->
			"for " ^ stringify_node params ^ " " ^ stringify_node block

	| Shell_ast.Until { condition; block } ->
			"until " ^ stringify_node condition ^ " " ^ stringify_node block

	| Shell_ast.Block nodes ->
			"{ " ^
			String.concat "; " (List.map stringify_node nodes) ^
			" }"

	| Shell_ast.Break None ->
			"brk"

	| Shell_ast.Break (Some expr) ->
			"brk " ^ stringify_node expr

	| Shell_ast.Continue ->
			"cont"

	| Shell_ast.Binary { left; op; right } ->
			stringify_node left ^ " " ^ Token.value op ^
			(match right with
			 | None -> ""
			 | Some r -> " " ^ stringify_node r)

	| Shell_ast.Unary { op; arg } ->
			(match arg with
			 | None -> Token.value op
			 | Some a ->
					 if Token.left op then
						 Token.value op ^ stringify_node a
					 else
						 stringify_node a ^ Token.value op)

	| Shell_ast.Reference id ->
			stringify_id id

	| Shell_ast.Call { from; args } ->
			stringify_node from ^ stringify_node args

	| Shell_ast.Index { from; args } ->
			stringify_node from ^ "[" ^
			(let (_, block_node) = args in
			 match block_node with
			 | Shell_ast.Block nodes ->
					 String.concat ", " (List.map stringify_node nodes)
			 | _ -> stringify_node args) ^
			"]"

	| Shell_ast.Initializer { type_; args } ->
			stringify_node type_ ^ " { " ^
			String.concat ", " (List.map (fun (name, value) ->
				name ^ ": " ^ stringify_node value
			) args) ^
			" }"

	| Shell_ast.TupleExpr exprs ->
			String.concat ", " (
				List.map
					stringify_node
					exprs
			)

	| Shell_ast.ArrayInitializer { size; type_; args } ->
			"[" ^
			(match size with
			 | None -> ""
			 | Some s -> stringify_node s) ^
			"]" ^ stringify_id type_ ^ " " ^ stringify_node args

	| Shell_ast.Integer tok ->
			(match tok.token with
			 | Token.Integer s -> s
			 | _ -> "<invalid integer>")

	| Shell_ast.Float tok ->
			(match tok.token with
			 | Token.Float s -> s
			 | _ -> "<invalid float>")

	| Shell_ast.Character tok ->
			(match tok.token with
			 | Token.Character s -> "'" ^ s ^ "'"
			 | _ -> "<invalid char>")

	| Shell_ast.String tok ->
			(match tok.token with
			 | Token.String s -> "\"" ^ s ^ "\""
			 | _ -> "<invalid string>")

	| Shell_ast.True _ ->
			"true"

	| Shell_ast.False _ ->
			"false"

	| Shell_ast.Parenthesis inner ->
			"(" ^ stringify_node inner ^ ")"

let parse_entry (allow_root_expr : bool) (input_files : string list) env : Shell_ast.node =
	let with_mutex m f =
		(Eio.Mutex.lock m; Fun.protect ~finally:(fun () -> Eio.Mutex.unlock m) f)
	in

	let sem = Eio.Semaphore.make 8 in

	let rec parse_file ~sw env (state : state) (path : string) : unit =
		let fs = Eio.Stdenv.cwd env in
		let ( / ) = Eio.Path.( / ) in

		with_mutex state.mutex (fun () ->
			if Hashtbl.mem state.pending path then
				false
			else (
				Hashtbl.add state.pending path ();
				true
			)
		)
		|> function
		| false -> ()
		| true -> (
			Eio.Semaphore.acquire sem;
			Fun.protect
				~finally:(fun () -> Eio.Semaphore.release sem)
				(fun () ->
					(* let flow = Eio.Fs.open_in fs path in *)
					Eio.Path.with_open_in (* ~sw *) (fs / path) (* @@ *) (fun flow ->
						let lexer = Lexer.init flow 4096 in
						let (root, imports) = parse allow_root_expr [] lexer in

						with_mutex state.mutex (fun () ->
							state.parsed := StringMap.add path root !(state.parsed));

						let rec loop (imports : string list) : unit =
							match imports with
							| [] -> ()
							| path::t -> (
								Eio.Fiber.fork ~sw (fun () -> parse_file ~sw env state path);
								loop t
							)
						in loop imports
					)
				)
		)
	in

	let state = {
		parsed = ref StringMap.empty;
		pending = Hashtbl.create 32;
		mutex = Eio.Mutex.create ();
	} in

	Eio.Switch.run @@ (fun sw ->
		List.iter
			(fun path -> parse_file ~sw env state path)
			input_files
	);
	Shell_ast.Program (StringMap.fold (fun key value acc -> ((key, value) :: acc)) !(state.parsed) [])
