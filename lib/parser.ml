exception SyntaxError of Parse_node.bounds * string
exception IOError of string

type pres = Parse_node.fat_node * (string list)

module StringMap = Map.Make(String)

type state = {
	parsed : Parse_node.fat_node StringMap.t ref;
	pending : (string, unit) Hashtbl.t;
	mutex : Eio.Mutex.t;
}

type scope =
	| Root
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

let rec parse (imports : string list) (s : Lexer.t) : pres =
	parse_scope Root imports s

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

		let rec loop (s : Lexer.t) (last : Lexer.fat_token) (imports : string list) (acc : Parse_node.fat_node list) : (Parse_node.fat_node list) * (string list) =
			let last = unwrap_peek_token s (Some last) in
			match last.token with
			| Token.Eof -> (acc, imports)
			| _ -> (
				let (result, imports) = single imports s in
				loop s (Parse_node.last result) imports (result :: acc)
			)
		in

		let start = unwrap_peek_token s None in
		let (nodes, imports) = loop s start imports [] in
		let last =
			match nodes with
			| [] -> None
			| h::_ -> Some (Parse_node.last h)
		in
		((start, (
			match last with
			| Some l -> l
			| None -> start
		)), Parse_node.Block (List.rev nodes)), imports
	)
	| Type -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"'type ...' must be followed by '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
			| _ -> (
				let (result, imports) = parse_decl imports s in
				loop s last imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start imports [] in
		((start, last), Parse_node.Block nodes), imports
	)
	| Enum -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"'enum ...' must be followed by '{'."
		in

		let rec loop (s : Lexer.t) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list)=
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
					(start, Parse_node.last annotation),
					Parse_node.EnumVariant {
						name = name;
						type_ = annotation;
					}
				) :: acc)
			)
		in

		let (last, nodes, imports) = loop s imports [] in
		((start, last), Parse_node.Block nodes), imports
	)
	| AnonymousEnum -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"anonymous enum annotation must start with '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
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
		((start, last), Parse_node.Block nodes), imports
	)
	| Tuple -> (
		let start =
			simple_expect_token s None Token.OpenParen
				"tuple annotation must start with '('."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
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
		((start, last), Parse_node.Block nodes), imports
	)
	| Space -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"'space ...' must be followed by '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
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
		((start, last), Parse_node.Block nodes), imports
	)
	| Code -> (
		let start = unwrap_peek_token s None in
		match start.token with
		| Token.OpenBrace -> (
			discard_pop s;
			let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
				let last = unwrap_peek_token s (Some start) in
				match last.token with
				| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
				| Token.SemiColon -> loop s last imports acc
				| _ -> (
					let (any, imports) = parse_scope Code imports s in
					loop s (Parse_node.last any) imports (any :: acc)
				)
			in
			let (last, nodes, imports) = loop s start imports [] in
			((start, last), Parse_node.Block nodes), imports
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
		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
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
							(start, Parse_node.last logic),
							Parse_node.MatchCase {
								expr = expr;
								logic = Some logic;
							}
						), imports
					)
					else (
						(
							(start, last),
							Parse_node.MatchCase {
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
		((start, last), Parse_node.Block nodes), imports
	)
	| FunctionParams -> (
		let start =
			simple_expect_token s None Token.OpenParen
				"function parameter list must start with '('."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
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
		((start, last), Parse_node.Block nodes), imports
	)
	| FunctionArguments -> (
		let start =
			simple_expect_token s None Token.OpenParen
				"function call argument list must start with '('."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
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
				let (result, imports) = parse_expr imports s in
				loop s start false imports (result :: acc)
			)
		in

		let (last, nodes, imports) = loop s start false imports [] in
		((start, last), Parse_node.Block nodes), imports
	)
	| IndexArguments -> (
		let start =
			simple_expect_token s None Token.OpenBracket
				"index argument list must start with '['."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
			let start = unwrap_peek_token s (Some start) in
			match start.token with
			| Token.ClosedBracket -> (discard_pop s; (start, List.rev acc, imports))
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
		((start, last), Parse_node.Block nodes), imports
	)
	| ForParams -> (
		(* a; b; c -> {a}; {b}; {c} *)
		let (init, imports) = parse_scope Code imports s in
		let (condition, imports) = parse_expr imports s in
		let (iter, imports) = parse_scope Code imports s in
		((Parse_node.start init, Parse_node.last iter), Parse_node.Block [init; condition; iter]), imports
	)
	| For -> (
		let start = unwrap_peek_token s None in
		match start.token with
		| Token.OpenBrace -> (
			discard_pop s;
			let rec loop (s : Lexer.t) (start : Lexer.fat_token) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
				let last = unwrap_peek_token s (Some start) in
				match last.token with
				| Token.ClosedBrace -> (discard_pop s; (last, List.rev acc, imports))
				| Token.SemiColon -> loop s last imports acc
				| _ -> (
					let (any, imports) = parse_scope For imports s in
					loop s (Parse_node.last any) imports (any :: acc)
				)
			in
			let (last, nodes, imports) = loop s start imports [] in
			((start, last), Parse_node.Block nodes), imports
		)
		| Token.Break -> (
			discard_pop s;
			let last = unwrap_peek_token s (Some start) in
			match last.token with
			| Token.SemiColon -> ((start, last), Parse_node.Break None), imports
			| _ -> (
				let (expr, imports) = parse_expr imports s in
				((start, Parse_node.last expr), Parse_node.Break (Some expr)), imports
			)
		)
		| Token.Continue -> (
			discard_pop s;
			((start, start), Parse_node.Continue), imports
		)
		| t when is_decl t -> parse_decl imports s
		| _ -> parse_expr imports s
	)
	| ArrayInitializer -> (
		let start =
			simple_expect_token s None Token.OpenBrace
				"array initializer must start with '{'."
		in

		let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : Parse_node.fat_node list) : Lexer.fat_token * (Parse_node.fat_node list) * (string list) =
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
		((start, last), Parse_node.Block nodes), imports
	)

and parse_decl (imports : string list) (s : Lexer.t) : pres =
	let start = unwrap_token s None in
	match start.token with
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
		((start, (Parse_node.last value)), Parse_node.Variable {
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
			((start, (Parse_node.last value)), Parse_node.Variable {
				name = name;
				type_ = Some type_;
				value = Some value;
			}), imports
		| _ -> (
			Lexer.push last s;
			((start, (Parse_node.last type_)), Parse_node.Variable {
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
		| Token.OpenBrace -> (None, imports) (* To annotate an anoynous unnamed variant, surround with parenthesis to make a single entry tuple *)
		| t when (is_type_annotation t) -> (
			let (res, imports) = parse_type_annotation imports s in
			(Some res, imports)
		)
		| _ -> (None, imports)
	) in

	let (code, imports) = parse_scope Function imports s in
	(
		(start, Parse_node.last code),
		Parse_node.Function {
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
			(start, Parse_node.last members),
			Parse_node.Type {
				name = name;
				members = members;
			}
		), imports
	)
	| Token.AssignmentEquals -> (
		let (annotation, imports) = parse_type_annotation imports s in
		(
			(start, (Parse_node.last annotation)),
			Parse_node.Alias (
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
		((start, Parse_node.last d), Parse_node.Pub d), imports
	)
	| Token.Priv -> (
		let (d, imports) = parse_decl imports s in
		((start, Parse_node.last d), Parse_node.Priv d), imports
	)
	| Token.Stat -> (
		let (d, imports) = parse_decl imports s in
		((start, Parse_node.last d), Parse_node.Stat d), imports
	)
	| _ ->
		syntax_error
			start start
			"expected either 'pub' | 'priv' | 'stat'."

and parse_operator_overload (imports : string list) (s : Lexer.t) : pres =
	(* oper (+a type) return_type {} *)
	(* oper (a type+) return_type {} *)
	(* oper (a type + b type) return_type {} *)
	let start = unwrap_token s None in
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
		(start, Parse_node.last code),
		Parse_node.OperatorOverload {
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
		(start, Parse_node.last members),
		Parse_node.Enum {
			name = name;
			members = members;
		}
	), imports

and parse_space (imports : string list) (s : Lexer.t) : pres =
	let start = unwrap_peek_token s None in
	let (_, name) = parse_id s in
	let (members, imports) = parse_scope Space imports s in
	(
		(start, Parse_node.last members),
		Parse_node.Space {
			name = name;
			members = members;
		}
	), imports

and parse_use (imports : string list) (s : Lexer.t) : pres =
	let start = unwrap_peek_token s None in
	let (last, use) = parse_id s in
	((start, last), Parse_node.Use use), imports

and parse_import (imports : string list) (s : Lexer.t) : pres =
	(* import "<path>" *)
	let (path, start) =
		expect_token s None (function | Token.String s -> Some s | _ -> None)
			"'import' must be followed by a filepath."
	in
	(
		(start, start),
		Parse_node.Import {
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
		(start, match false_block with | Some fb -> (Parse_node.last fb) | None -> (Parse_node.last true_block)),
		Parse_node.If {
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
		(Parse_node.start switcher, Parse_node.last cases),
		Parse_node.Match {
			switcher = switcher;
			cases = cases;
		}
	), imports

and parse_for (imports : string list) (s : Lexer.t) : pres =
	let (params, imports) = parse_scope ForParams imports s in
	let (block, imports) = parse_scope For imports s in
	(
		(Parse_node.start params, Parse_node.last block),
		Parse_node.For {
			params = params;
			block = block;
		}
	), imports

and parse_until (imports : string list) (s : Lexer.t) : pres =
	(* until expr { ... } *)
	let (condition, imports) = parse_expr imports s in
	let (block, imports) = parse_scope Code imports s in
	(
		(Parse_node.start condition, Parse_node.last block),
		Parse_node.Until {
			condition = condition;
			block = block;
		}
	), imports

and parse_expr (imports : string list) (s : Lexer.t) : pres =
	parse_expr_bp imports s max_int

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
					(start, Parse_node.last arg),
					Parse_node.Unary { op = t; arg = Some arg }
				),
				imports
			)
		)
		| Token.OpenParen -> (
			discard_pop s;
			let (((_, _), expr), imports) = parse_expr imports s in
			let last =
				simple_expect_token s (Some start) Token.ClosedParen
					"expected ')'"
			in (((start, last), expr), imports)
		)
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
			if l_bp > max_bp then (lhs, imports)
			else (
				discard_pop s;
				let (rhs, imports) = parse_expr_bp imports s r_bp in
				let new_lhs = (
					(Parse_node.start lhs, Parse_node.last rhs),
					Parse_node.Binary { left = lhs; op = t; right = Some rhs }
				) in
				loop new_lhs imports
			)
		| t when Token.right t ->  (* postfix unary *)
			let l_bp = right_bp t in
			if l_bp < max_bp then (lhs, imports)
			else (
				discard_pop s;
				let new_lhs = (
					(Parse_node.start lhs, peek),
					Parse_node.Unary { op = t; arg = Some lhs }
				) in
				loop new_lhs imports
			)
		| _ -> (lhs, imports)
	in
	loop lhs imports

and parse_atom (imports : string list) (s : Lexer.t) : (Parse_node.fat_node option) * (string list) =
	let start = unwrap_token s None in
	match start.token with
	| Token.Integer _ -> (Some ((start, start), Parse_node.Integer start), imports)
	| Token.Float _ -> (Some ((start, start), Parse_node.Float start), imports)
	| Token.Character _ -> (Some ((start, start), Parse_node.Character start), imports)
	| Token.String _ -> (Some ((start, start), Parse_node.String start), imports)
	| Token.True -> (Some ((start, start), Parse_node.True start), imports)
	| Token.False -> (Some ((start, start), Parse_node.False start), imports)
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
		(Some ((start, Parse_node.last args), Parse_node.ArrayInitializer { size = size; type_ = type_; args = args }), imports)
	)
	| Token.Id _ -> (
		Lexer.push start s;
		let (ref_last, ref) = parse_id s in
		(Some ((start, ref_last), Parse_node.Reference ref), imports)
	)
	| _ -> (None, imports)

and parse_postfix (imports : string list) (s : Lexer.t) (acc : Parse_node.fat_node) : pres =
	let start = unwrap_peek_token s (Some (Parse_node.start acc)) in
	match start.token with
	| Token.OpenParen -> (
		discard_pop s;
		let (args, imports) = parse_scope FunctionArguments imports s in
		let last =
			simple_expect_token s (Some (Parse_node.start args)) Token.ClosedParen
				"function call expression must be closed off with ')' to match the opening '('."
		in
		parse_postfix imports s (
			(Parse_node.start acc, last), Parse_node.Call {
				from = acc;
				args = args;
			}
		)
	)
	| Token.OpenBracket -> (
		discard_pop s;
		let (args, imports) = parse_scope IndexArguments imports s in
		let last =
			simple_expect_token s (Some (Parse_node.start args)) Token.ClosedBracket
				"index expression must be closed off with ']' to match the opening '['."
		in
		parse_postfix imports s (
			(Parse_node.start acc, last), Parse_node.Index {
				from = acc;
				args = args;
			}
		)
	)
	| Token.OpenBrace -> (
		let initializer_parse_scope (imports : string list) (s : Lexer.t) : Lexer.fat_token * ((string * Parse_node.fat_node) list) * (string list) =
			let single (imports : string list) (s : Lexer.t) : (string * Parse_node.fat_node * (string list)) =
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

			let rec loop (s : Lexer.t) (start : Lexer.fat_token) (comma_found : bool) (imports : string list) (acc : (string * Parse_node.fat_node) list) : Lexer.fat_token * ((string * Parse_node.fat_node) list) * (string list) =
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
		let (last, args, imports) = initializer_parse_scope imports s in
		parse_postfix imports s (
			(Parse_node.start acc, last), Parse_node.Initializer {
				type_ = acc;
				args = args;
			}
		)
	)
	| _ -> (acc, imports)

(*
and parse_expr (imports : string list) (s : Lexer.t) : pres =
	let prec (f : Lexer.fat_token) : int =
		if f.token = Token.OpenParen then (
			-1
		) else (
			match Token.prec f.token with
			| Some prec -> prec
			| None ->
				syntax_error f f
					"impossible error"
		)
	in

	let apply_oper (stack : Parse_node.fat_node list) (oper : Lexer.fat_token) : Parse_node.fat_node list =
		match oper.token with
		| t when Token.left t -> (
			(
				(oper, oper),
				Parse_node.Unary {
					op = t;
					arg = None;
				}
			) :: stack
		)
		| t when Token.right t -> (
			match stack with
			| [] ->
				syntax_error oper oper
					"a right unary operator must have an expression to its left to act upon."
			| h::tail -> (
				(
					(Parse_node.start h, oper),
					Parse_node.Unary {
						op = t;
						arg = Some h;
					}
				) :: tail
			)
		)
		| t when Token.binary t -> (
			match stack with
			| [] ->
				syntax_error oper oper
					"a binary operator must have an expression to its left and right to act upon."
			| h::tail -> (
				(
					(Parse_node.start h, oper),
					Parse_node.Binary {
						left = h;
						op = t;
						right = None;
					}
				) :: tail
			)
		)
		| _ ->
			syntax_error oper oper
				"expected an operator."
	in

	let apply_expr (stack : Parse_node.fat_node list) (expr : Parse_node.fat_node) : Parse_node.fat_node list =
		match stack with
		| [] -> [expr]
		| h::t -> (
			let ((start, _), node) = h in
			match node with
			| Parse_node.Binary b -> (
				match b.right with
				| Some ((start, last), _) ->
					syntax_error start last
						"binary operator can only act upon 2 expressions."
				| None -> (
					(
						(start, Parse_node.last expr),
						Parse_node.Binary { b with right = Some expr; }
					) :: t
				)
			)
			| Parse_node.Unary u -> (
				(* Right unary operators are immediately applied, so its fine to simply check if the argument exists *)
				match u.arg with
				| Some ((start, last), _) ->
					syntax_error start last
						"unary operator can only act upon 1 expression."
				| None -> (
					(
						(start, Parse_node.last expr),
						Parse_node.Unary { u with arg = Some expr; }
					) :: t
				)
			)
			| _ ->
				syntax_error (Parse_node.start expr) (Parse_node.last expr)
					"expected operator to delimit expressions."
		)
	in

	let rec single (s : Lexer.t) (imports : string list) (stack : Parse_node.fat_node list) (opers : Lexer.fat_token list) : (Parse_node.fat_node list) * (Lexer.fat_token list) * (string list) =
		let rec loop (start : Lexer.fat_token) (stack : Parse_node.fat_node list) (opers : Lexer.fat_token list) : (Parse_node.fat_node list) * (Lexer.fat_token list) =
			match opers with
			| [] -> (stack, [start])
			| h::t -> (
				let start_prec = prec start in
				let h_prec = prec h in
				if h_prec >= start_prec then (
					let stack = apply_oper stack h in
					loop start stack t
				) else (
					if start.token = Token.ClosedParen then
						(stack, t)
					else
						(stack, start :: t)
				)
			)
		in

		let start = unwrap_peek_token s None in
		match start.token with
		| t when (t = Token.OpenParen || Token.oper t) -> (
			discard_pop s;
			let (stack, opers) = loop start stack opers in
			single s imports stack opers
		)
		| _ -> (
			let (atom, imports) = parse_atom imports s in
			match atom with
			| Some expr -> (
				let (expr, imports) = parse_postfix imports s expr in
				let stack = apply_expr stack expr in
				single s imports stack opers
			)
			| None -> (stack, opers, imports)
		)
	in

	let (stack, opers, imports) = single s imports [] [] in
	let rec collapse (stack : Parse_node.fat_node list) (opers : Lexer.fat_token list) : Parse_node.fat_node list =
		match opers with
		| [] -> stack
		| h::t ->
			collapse (apply_oper stack h) t
	in

	match collapse stack opers with
	| [] -> (
		let start = unwrap_peek_token s None in
		syntax_error start start "expected expression."
	)
	| [h] -> (h, imports)
	| h::_ ->
		syntax_error (Parse_node.start h) (Parse_node.last h) "invalid expression."

 *)
and parse_type_annotation (imports : string list) (s : Lexer.t) : pres =
	let start = unwrap_token s None in
	match start.token with
	| Token.Type -> (
		let (members, imports) = parse_scope Type imports s in
		(
			(start, Parse_node.last members),
			Parse_node.Type {
				name = "";
				members = members;
			}
		), imports
	)
	| Token.Enum -> (
		let (members, imports) = parse_scope Enum imports s in
		(
			(start, Parse_node.last members),
			Parse_node.Enum {
				name = "";
				members = members;
			}
		), imports
	)
	| Token.OpenParen -> (
		Lexer.push start s;
		let (members, imports) = parse_scope Tuple imports s in
		((start, Parse_node.last members), Parse_node.Tuple members), imports
	)
	| Token.OpenBrace -> (
		Lexer.push start s;
		let (members, imports) = parse_scope AnonymousEnum imports s in
		((start, Parse_node.last members), Parse_node.AnonymousEnum members), imports
	)
	| _ -> (
		Lexer.push start s;
		let (last, id) = parse_id s in
		((start, last), Parse_node.Id id), imports
	)

and parse_id (s : Lexer.t) : Lexer.fat_token * Parse_node.id =
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

and stringify_id (id : Parse_node.id) : string =
	let (root, chain) = id in
	let rec loop (chain : string list) : string =
		match chain with
		| [] -> ""
		| [h] -> h
		| h::t -> h ^ "::" ^ (loop t)
	in (if root then "::" else "") ^ (loop chain)

let rec stringify_node (node : Parse_node.fat_node) : string =
	let (_, node) = node in
	match node with
	| Parse_node.Program files ->
			"Program [\n" ^
			(String.concat ";\n" (List.map (fun (path, block) ->
				"	 " ^ path ^ " -> " ^ stringify_node block
			) files)) ^
			"\n]"

	| Parse_node.Pub inner ->
			"pub " ^ stringify_node inner

	| Parse_node.Priv inner ->
			"priv " ^ stringify_node inner

	| Parse_node.Stat inner ->
			"stat " ^ stringify_node inner

	| Parse_node.Id id ->
			stringify_id id

	| Parse_node.Variable var ->
			"let " ^ var.name ^
			(match var.type_ with
			 | None -> ""
			 | Some t -> " : " ^ stringify_node t) ^
			(match var.value with
			 | None -> ""
			 | Some v -> " = " ^ stringify_node v)

	| Parse_node.Type layer ->
			if layer.name = "" then
				"type { " ^ stringify_node layer.members ^ " }"
			else
				"type " ^ layer.name ^ " { " ^ stringify_node layer.members ^ " }"

	| Parse_node.Alias (name, target) ->
			"type " ^ name ^ " = " ^ stringify_node target

	| Parse_node.Tuple block ->
			"(" ^ stringify_node block ^ ")"

	| Parse_node.Function fn ->
			"fn " ^ fn.name ^ stringify_node fn.params ^
			(match fn.result with
			 | None -> ""
			 | Some r -> " " ^ stringify_node r) ^
			" " ^ stringify_node fn.code

	| Parse_node.OperatorOverload { name; params; result; code } ->
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

	| Parse_node.EnumVariant { name; type_ } ->
			name ^ " " ^ stringify_node type_

	| Parse_node.Enum layer ->
			if layer.name = "" then
				"enum { " ^ stringify_node layer.members ^ " }"
			else
				"enum " ^ layer.name ^ " { " ^ stringify_node layer.members ^ " }"

	| Parse_node.AnonymousEnum block ->
			"{ " ^ stringify_node block ^ " }"

	| Parse_node.Space { name; members } ->
			"space " ^ stringify_id name ^ " { " ^ stringify_node members ^ " }"

	| Parse_node.Use id ->
			"use " ^ stringify_id id

	| Parse_node.Import { path } ->
			"import \"" ^ path ^ "\""

	| Parse_node.If { condition; true_block; false_block } ->
			"if " ^ stringify_node condition ^
			" " ^ stringify_node true_block ^
			(match false_block with
			 | None -> ""
			 | Some fb -> " else " ^ stringify_node fb)

	| Parse_node.Match { switcher; cases } ->
			"match " ^ stringify_node switcher ^ " { " ^ stringify_node cases ^ " }"

	| Parse_node.MatchCase { expr; logic } ->
			stringify_node expr ^
			(match logic with
			 | None -> ","
			 | Some l -> " -> " ^ stringify_node l)

	| Parse_node.For { params; block } ->
			"for " ^ stringify_node params ^ " " ^ stringify_node block

	| Parse_node.Until { condition; block } ->
			"until " ^ stringify_node condition ^ " " ^ stringify_node block

	| Parse_node.Block nodes ->
			"{ " ^
			String.concat "; " (List.map stringify_node nodes) ^
			" }"

	| Parse_node.Break None ->
			"brk"

	| Parse_node.Break (Some expr) ->
			"brk " ^ stringify_node expr

	| Parse_node.Continue ->
			"cont"

	| Parse_node.Binary { left; op; right } ->
			stringify_node left ^ " " ^ Token.value op ^
			(match right with
			 | None -> ""
			 | Some r -> " " ^ stringify_node r)

	| Parse_node.Unary { op; arg } ->
			(match arg with
			 | None -> Token.value op
			 | Some a ->
					 if Token.left op then
						 Token.value op ^ stringify_node a
					 else
						 stringify_node a ^ Token.value op)

	| Parse_node.Reference id ->
			stringify_id id

	| Parse_node.Call { from; args } ->
			stringify_node from ^ stringify_node args

	| Parse_node.Index { from; args } ->
			stringify_node from ^ "[" ^
			(let (_, block_node) = args in
			 match block_node with
			 | Parse_node.Block nodes ->
					 String.concat ", " (List.map stringify_node nodes)
			 | _ -> stringify_node args) ^
			"]"

	| Parse_node.Initializer { type_; args } ->
			stringify_node type_ ^ " { " ^
			String.concat ", " (List.map (fun (name, value) ->
				name ^ ": " ^ stringify_node value
			) args) ^
			" }"

	| Parse_node.ArrayInitializer { size; type_; args } ->
			"[" ^
			(match size with
			 | None -> ""
			 | Some s -> stringify_node s) ^
			"]" ^ stringify_id type_ ^ " " ^ stringify_node args

	| Parse_node.Integer tok ->
			(match tok.token with
			 | Token.Integer s -> s
			 | _ -> "<invalid integer>")

	| Parse_node.Float tok ->
			(match tok.token with
			 | Token.Float s -> s
			 | _ -> "<invalid float>")

	| Parse_node.Character tok ->
			(match tok.token with
			 | Token.Character s -> "'" ^ s ^ "'"
			 | _ -> "<invalid char>")

	| Parse_node.String tok ->
			(match tok.token with
			 | Token.String s -> "\"" ^ s ^ "\""
			 | _ -> "<invalid string>")

	| Parse_node.True _ ->
			"true"

	| Parse_node.False _ ->
			"false"

	| Parse_node.Parenthesis inner ->
			"(" ^ stringify_node inner ^ ")"

let parse_entry (input_files : string list) env : Parse_node.node =
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
						let (root, imports) = parse [] lexer in

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
	Parse_node.Program (StringMap.fold (fun key value acc -> ((key, value) :: acc)) !(state.parsed) [])
