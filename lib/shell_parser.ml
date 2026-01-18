exception SyntaxError of Shell_ast.bounds * string
exception IOError of string

type pres = Shell_ast.fat_node * (string list)

module StringMap = Map.Make(String)

type state = {
	parsed : Shell_ast.fat_node list StringMap.t ref;
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
	| LambdaParams
	| FunctionTypeParams
	| FunctionArguments
	| IndexArguments
	| StandardLoopBlock
	| BreakingLoopBlock
	| ArrayInitializer

(* TODO: pretty print *)
let syntax_error (first : Lexer.fat_token) (last : Lexer.fat_token) (msg : string) : 'a =
	raise (SyntaxError ((first, last), msg))

let io_error (msg : string) : 'a =
	raise (IOError msg)

let unwrap_token s (start : Lexer.fat_token option) : Lexer.fat_token =
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

let unwrap_peek_token s (start : Lexer.fat_token option) : Lexer.fat_token =
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

let discard_pop s : unit =
	match Lexer.pop s with
	| Error (_, msg) ->
		io_error ("Lexer.pop fatally failed: " ^ msg)
	| _ -> ()

let expect_token s (start : Lexer.fat_token option) (pred : Token.t -> 'b option) (msg : string) : 'b * Lexer.fat_token =
	let last = unwrap_token s start in
	match pred last.token with
	| Some thing -> (thing, last)
	| None ->
		syntax_error
			(match start with | None -> last | Some f -> f) last
			msg

let simple_expect_token s (start : Lexer.fat_token option) (ty : Token.t) (msg : string) : Lexer.fat_token =
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
	| Token.Scope | Token.Id _ | Token.Type | Token.Enum | Token.OpenParen | Token.OpenBrace | Token.OpenBracket | Token.Fn -> true
	| Token.LeftDereference | Token.RightDereference | Token.Multiply -> true (* pointers *)
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

	| Token.For | Token.Until -> true (* this is weird to think about, but breaking for should ONLY be used in an expression *)
	| _ -> false

let parse_delimited
	?(opening=Token.Unknown) ?(closing=Token.Eof)
	?(delimiter=Token.Unknown) ?(delim_optional=(delimiter = Token.Unknown)) ~unit_concept
	?(allow_none=true) ?(open_optional=false)
	?(allow_trailing=false) ~unit_parser imports s =

	let start =
		if opening = Token.Unknown || open_optional then unwrap_peek_token s None
		else simple_expect_token s None opening ("expected '" ^ (Token.value opening) ^ "'.")
	in

	let open_found =
		if open_optional && start.token = opening then
			(discard_pop s; true)
		else
			(not open_optional)
	in
	let single_mode = open_optional && opening <> Token.Unknown && not open_found in

	if single_mode then (
		let (unit, imports) = unit_parser imports s in
		((start, Shell_ast.last unit), [unit], imports)
	) else (
		let rec loop start just_parsed_unit delim_used imports acc =
			let tok = unwrap_peek_token s (Some start) in
			match tok.token with
			| t when t = closing ->
				if not open_found then
					syntax_error tok tok ("'" ^ (Token.value closing) ^ "' found with no matching '" ^ (Token.value opening) ^ "'.")
				else if just_parsed_unit || allow_trailing || (allow_none && (snd acc) = []) then
					let ((start, _), acc) = acc in
					(discard_pop s; ((start, tok), List.rev acc, imports))
				else
					syntax_error tok tok ("expected " ^ unit_concept ^ ".")
			| t when t = delimiter ->
				if delim_used then
					syntax_error tok tok ("multiple consecutive " ^ unit_concept ^ ".")
				else if not just_parsed_unit then
					syntax_error tok tok ("unexpected delimiter '" ^ (Token.value delimiter) ^ "'.")
				else
					(discard_pop s; loop tok false true imports acc)
			| _ -> (
				if not just_parsed_unit || delim_optional then
					let (unit, imports) = unit_parser imports s in
					let ((start, _), acc) = acc in
					loop tok true false imports ((start, Shell_ast.last unit), (unit :: acc))
				else
					syntax_error tok tok ("expected delimiter '" ^ (Token.value delimiter) ^ "'.")
			)
		in

		let (bnds, items, imports) = loop start false false imports ((start, start), []) in
		(bnds, items, imports)
	)

let parse_sequence ~unit_concept ~unit_parser ?(end_tok=Token.Eof) imports s =
	parse_delimited ~closing:end_tok ~unit_concept:unit_concept ~unit_parser:unit_parser imports s

let rec parse allow_root_expr imports s : Shell_ast.bounds * (Shell_ast.fat_node list) * (string list) =
	if allow_root_expr then (
		parse_scope ChillRoot imports s
	) else (
		parse_scope Root imports s
	)

and parse_scope scope imports s : Shell_ast.bounds * (Shell_ast.fat_node list) * (string list) =
	match scope with
	| Root ->
		parse_sequence
			~unit_concept:"root level declaration"
			~unit_parser:(fun imports s ->
				let start = unwrap_peek_token s None in
				match start.token with
				| Token.Stat ->
					syntax_error
						start start
						"'stat' makes no sense in the file's root context."
				| _ -> parse_decl imports s
			) imports s
	| ChillRoot ->
		parse_sequence
			~unit_concept:"root level declaration or expression"
			~unit_parser:(fun imports s ->
				let start = unwrap_peek_token s None in
				match start.token with
				| Token.Stat ->
					syntax_error
						start start
						"'stat' makes no sense in the file's root context."
				| t when is_decl t -> parse_decl imports s
				| _ -> parse_expr imports s
			) imports s
	| Type ->
		parse_delimited
			~opening:Token.OpenBrace ~closing:Token.ClosedBrace
			~unit_concept:"type member"
			~unit_parser:(fun imports s ->
				let start = unwrap_peek_token s None in
				match start.token with
				| Token.Id _ -> parse_variable imports s
				| _ -> parse_decl imports s
			) imports s
	| Enum ->
		parse_delimited
			~opening:Token.OpenBrace ~closing:Token.ClosedBrace ~delimiter:Token.Comma ~delim_optional:true
			~unit_concept:"enum variant" ~allow_trailing:true
			~unit_parser:(fun imports s ->
				let start = unwrap_token s None in
				match start.token with
				| Token.Fn -> parse_function imports s
				| Token.Id name ->
					let (annotation, imports) = parse_type_annotation imports s in
					(((start, Shell_ast.last annotation), Shell_ast.EnumVariant { name = name; type_ = annotation; }), imports)
				| _ ->
					syntax_error start start
						"Enums must only contain methods or variant defintions."
			) imports s
	| AnonymousEnum ->
		parse_delimited
			~opening:Token.OpenBrace ~closing:Token.ClosedBrace ~delimiter:Token.Comma ~delim_optional:true
			~unit_concept:"anonymous enum variant"
			~unit_parser:parse_type_annotation imports s
	| Tuple ->
		parse_delimited
			~opening:Token.OpenParen ~closing:Token.ClosedParen ~delimiter:Token.Comma
			~unit_concept:"tuple member"
			~unit_parser:parse_type_annotation imports s
	| Space ->
		parse_delimited
			~opening:Token.OpenBrace ~closing:Token.ClosedBrace
			~unit_concept:"space member declaration"
			~unit_parser:(fun imports s ->
				let last = unwrap_peek_token s None in
				match last.token with
				| Token.Stat ->
					syntax_error last last
						"'stat' cannot be used shallowly inside a space block (since there is no clear meaning of what that would accomplish)."
				| _ -> parse_decl imports s
			) imports s
	| Code ->
		parse_delimited
			~unit_concept:"code" ~opening:Token.OpenBrace ~closing:Token.ClosedBrace
			~open_optional:true
			~unit_parser:(fun imports s ->
				let start = unwrap_peek_token s None in
				match start.token with
				| Token.OpenBrace -> (
					let (bnds, code, imports) = parse_scope Code imports s in
					((bnds, Shell_ast.Block code), imports)
				)
				| t when is_decl t -> parse_decl imports s
				| _ -> parse_expr imports s
			) imports s
	| Function -> (
		let start = unwrap_peek_token s None in
		match start.token with
		| Token.AssignmentEquals -> (discard_pop s; parse_scope Code imports s)
		| Token.OpenBrace -> parse_scope Code imports s
		| _ ->
			syntax_error start start
				"function implementation must start with '{' | '='."
	)
	| Match ->
		parse_delimited
			~opening:Token.OpenBrace ~closing:Token.ClosedBrace
			~unit_concept:"match case"
			~unit_parser:(fun imports s ->
				let (pattern, imports) = parse_expr imports s in
				let (start, pattern_last) = Shell_ast.bounds pattern in
				let (guard, imports, guard_last) =
					let last = unwrap_peek_token s (Some start) in
					match last.token with
					| Token.If -> (
						discard_pop s;
						let guard, imports = parse_expr imports s in
						Some guard, imports, last
					)
					| _ -> None, imports, pattern_last
				in
				let (arrow, last) =
					let last = unwrap_peek_token s (Some guard_last) in
					match last.token with
					| Token.Arrow -> (
						discard_pop s;
						true, last
					)
					| _ -> false, guard_last
				in
				if arrow then
					let ((logic_start, last), logic, imports) = parse_scope Code imports s in
					((start, last), Shell_ast.MatchCase {
						pattern = pattern;
						guard = guard;
						logic = Some ((logic_start, last), Shell_ast.Block logic);
					}), imports
				else
					((start, last), Shell_ast.MatchCase {
						pattern = pattern;
						guard = guard;
						logic = None;
					}), imports
			) imports s
	| FunctionParams ->
		parse_delimited
			~opening:Token.OpenParen ~closing:ClosedParen ~delimiter:Token.Comma
			~unit_concept:"function parameter" ~allow_trailing:true
			~unit_parser:(parse_param ~optional_name:false ~allow_value:true)
			imports s
	| LambdaParams ->
		parse_delimited
			~opening:Token.OpenParen ~closing:ClosedParen ~delimiter:Token.Comma
			~unit_concept:"lambda parameter" ~allow_trailing:true
			~unit_parser:(parse_param ~optional_name:true ~allow_value:true)
			imports s
	| FunctionTypeParams ->
		parse_delimited
			~opening:Token.OpenParen ~closing:ClosedParen ~delimiter:Token.Comma
			~unit_concept:"lambda parameter type" ~allow_trailing:true
			~unit_parser:(parse_param ~optional_name:true ~allow_value:false)
			imports s
	| FunctionArguments ->
		parse_delimited
			~opening:Token.OpenParen ~closing:ClosedParen ~delimiter:Token.Comma
			~unit_concept:"function argument"
			~unit_parser:parse_expr
			imports s
	| IndexArguments ->
		parse_delimited
			~opening:Token.OpenBracket ~closing:ClosedBracket ~delimiter:Token.Comma
			~unit_concept:"index argument"
			~unit_parser:parse_expr
			imports s
	| StandardLoopBlock ->
		parse_delimited
			~opening:Token.OpenBrace ~closing:Token.ClosedBrace
			~unit_concept:"for block"
			~unit_parser:(fun imports s ->
				let start = unwrap_peek_token s None in
				match start.token with
				| Token.Break -> (
					discard_pop s;
					((start, start), Shell_ast.StandardBreak), imports
				)
				| Token.Continue -> (
					discard_pop s;
					((start, start), Shell_ast.Continue), imports
				)
				| Token.OpenBrace ->
					let (bnds, block, imports) = parse_scope StandardLoopBlock imports s in
					(bnds, Shell_ast.Block block), imports
				| t when is_decl t -> parse_decl imports s
				| _ -> parse_expr imports s
			) imports s
	| BreakingLoopBlock -> (
		parse_delimited
			~opening:Token.OpenBrace ~closing:Token.ClosedBrace
			~unit_concept:"for block"
			~unit_parser:(fun imports s ->
				let start = unwrap_peek_token s None in
				match start.token with
				| Token.Break -> (
					discard_pop s;
					let (expr, imports) = parse_expr imports s in
					((start, Shell_ast.last expr), Shell_ast.YieldingBreak expr), imports
				)
				| Token.Continue -> (
					discard_pop s;
					((start, start), Shell_ast.Continue), imports
				)
				| Token.OpenBrace ->
					let (bnds, block, imports) = parse_scope BreakingLoopBlock imports s in
					(bnds, Shell_ast.Block block), imports
				| t when is_decl t -> parse_decl imports s
				| _ -> parse_expr imports s
			) imports s
	)
	| ArrayInitializer ->
		parse_delimited
			~opening:Token.OpenBrace ~closing:ClosedBrace ~delimiter:Token.Comma
			~unit_concept:"array initializer argument"
			~unit_parser:parse_expr
			imports s

and parse_decl imports s : pres =
	let parse_decl_atom imports s : pres =
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

		| Token.For -> parse_for false imports s (* to allow for standard for/while loops *)
		| Token.Until -> parse_until false imports s
		(* | Token.Import -> parse_import imports s <- offlaoded to aliases with let *)
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

and parse_variable imports s : pres =
	(*
		let name type = value
		let name = value
		let name type
		let name = declaration (functions decay to lambdas (i.e. valid expressions)) <- this is an immutable alias I guess
	*)
	let (name, start) =
		expect_token s None (function | Token.Id name -> Some name | _ -> None) "'let' must be followed by an id." in
	let last = unwrap_token s (Some start) in
	match last.token with
	| Token.AssignmentEquals -> (
		let next = unwrap_peek_token s (Some start) in
		match next.token with
		| Token.Space -> (
			discard_pop s;
			let ((_, member_last), members, imports) = parse_scope Space imports s in
			(
				(start, member_last),
				Shell_ast.Alias {
					name = name;
					annotation = (
						(next, member_last),
						Shell_ast.Space {
							name = (false, []);
							members = members;
						}
					);
				}
			), imports
		)
		| Token.Import -> (
			discard_pop s;
			let (import, imports) = parse_import imports s in
			(
				(start, Shell_ast.last import),
				Shell_ast.Alias {
					name = name;
					annotation = import
				}
			), imports
		)
		| Token.Fn -> (
			let (value, imports) = parse_expr imports s in
			((start, (Shell_ast.last value)), Shell_ast.Variable {
				name = name;
				type_ = None;
				value = Some value;
			}), imports
		)
		| t when is_decl t -> (
			let (annotation, imports) = parse_type_annotation imports s in
			((start, Shell_ast.last annotation), Shell_ast.Alias {
				name = name;
				annotation = annotation;
			}), imports
		)
		| _ -> (
			let (value, imports) = parse_expr imports s in
			((start, (Shell_ast.last value)), Shell_ast.Variable {
				name = name;
				type_ = None;
				value = Some value;
			}), imports
		)
	)
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

and parse_param imports s ~optional_name ~allow_value : pres =
	(*
		name type = value
		name = value
		name type
	*)
	let (name, start) =
		if optional_name then (
			let next = unwrap_peek_token s None in
			match next.token with
			| Token.Id name -> name, next
			| _ -> "", next
		) else
			expect_token s None (function | Token.Id name -> Some name | _ -> None) "parameter must start with an id."
	in
	let last = unwrap_token s (Some start) in
	match last.token with
	| Token.AssignmentEquals ->
		if allow_value then (
			let (value, imports) = parse_expr imports s in
			((start, (Shell_ast.last value)), Shell_ast.Variable {
				name = name;
				type_ = None;
				value = Some value;
			}), imports
		) else (
			syntax_error last last
				(Printf.sprintf "parameter '%s' cannot have an initial value; a type annotation is also needed." name)
		)
	| t when (is_type_annotation t) || t = Token.RightSpread || t = Token.LeftSpread -> (
		Lexer.push last s;
		let (type_, imports) =
			match last.token with
			| Token.RightSpread | Token.LeftSpread -> (
				discard_pop s;
				let (inner, imports) = parse_type_annotation imports s in
				((last, Shell_ast.last inner), Shell_ast.Variadic inner), imports
			)
			| _ -> parse_type_annotation imports s
		in

		let last = unwrap_token s (Some start) in
		match last.token with
		| Token.AssignmentEquals ->
			if allow_value then (
				let (value, imports) = parse_expr imports s in
				((start, (Shell_ast.last value)), Shell_ast.Variable {
					name = name;
					type_ = Some type_;
					value = Some value;
				}), imports
			) else (
				syntax_error last last
					(Printf.sprintf "parameter '%s' cannot have an initial value." name)
			)
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
			(Printf.sprintf "'%s' parameter must be followed by a type annotation or '='." name)

and parse_function imports s : pres =
	(*
		fn name(param type) type {}
		fn name(param type) {}
		fn name() type = value
		fn name() = value
		fn name(constant) = value (acts as case like haskell ykwim) TODO
		fn()... -> lambda
	*)
	let start = unwrap_peek_token s None in
	match start.token with
	| Token.Id name -> (
		discard_pop s;
		let (_, params, imports) = parse_scope FunctionParams imports s in
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

		let ((_, code_last), code, imports) = parse_scope Function imports s in
		(
			(start, code_last),
			Shell_ast.Function {
				name = name;
				params = params;
				result = result;
				code = code;
			}
		), imports
	)
	| _ -> (
		let ((_, last), lambda), imports = parse_lambda imports s in
		((start, last), lambda), imports
	)

and parse_type imports s : pres =
	let (name, start) =
		expect_token s None (function | Token.Id name -> Some name | _ -> None) "'type' must be followed by an unchained id." in
	let (next, last) =
		expect_token s (Some start) (fun (t : Token.t) -> match t with | Token.OpenBrace | Token.AssignmentEquals -> Some t | _ -> None)
			(Printf.sprintf "'type %s' must be followed by '{' or '='" name)
	in
	match next with
	| Token.OpenBrace -> (
		Lexer.push last s;
		let ((_, member_last), members, imports) = parse_scope Type imports s in
		(
			(start, member_last),
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
			Shell_ast.Alias {
				name = name;
				annotation = annotation;
			}
		), imports
	)
	| _ -> syntax_error start last "unreachable"

and parse_access_mod start imports s : pres =
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

and parse_operator_overload imports s : pres =
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
	let ((_, code_last), code, imports) = parse_scope Function imports s in
	(
		(start, code_last),
		Shell_ast.OperatorOverload {
			name = oper.token;
			params = params;
			result = result;
			code = code;
		}
	), imports

and parse_enum imports s : pres =
	let (name, start) =
		expect_token s None (function | Token.Id name -> Some name | _ -> None)
			"'enum' must be followed by an id [a-zA-Z_][a-zA-Z0-9_]*."
	in
	let ((_, members_last), members, imports) = parse_scope Enum imports s in
	(
		(start, members_last),
		Shell_ast.Enum {
			name = name;
			members = members;
		}
	), imports

and parse_space imports s : pres =
	let start = unwrap_peek_token s None in
	let (_, name) = parse_id s in
	let ((_, members_last), members, imports) = parse_scope Space imports s in
	(
		(start, members_last),
		Shell_ast.Space {
			name = name;
			members = members;
		}
	), imports

and parse_use imports s : pres =
	let start = unwrap_peek_token s None in
	let (last, use) = parse_id s in
	((start, last), Shell_ast.Use use), imports

and parse_import imports s : pres =
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

and parse_if imports s : pres =
	(* if expr any || <if> else any *)
	let start = unwrap_peek_token s None in
	let (condition, imports) = parse_expr imports s in
	let ((_, true_last), true_block, imports) = parse_scope Code imports s in
	let ((_, false_last), false_block, imports) =
		let last = unwrap_peek_token s (Some start) in
		match last.token with
		| Token.Else -> (
			discard_pop s;
			let (res_bnds, res, imports) = parse_scope Code imports s in
			(res_bnds, Some res, imports)
		)
		| _ -> ((last, last), None, imports)
	in
	(
		(start, match false_block with | Some _ -> false_last | None -> true_last),
		Shell_ast.If {
			condition = condition;
			true_block = true_block;
			false_block = false_block;
		}
	), imports

and parse_match imports s : pres =
	(*
		match switcher {
			expr, expr2 -> logic
		}
	*)
	let (switcher, imports) = parse_expr imports s in
	let ((_, cases_last), cases, imports) = parse_scope Match imports s in
	(
		(Shell_ast.start switcher, cases_last),
		Shell_ast.Match {
			switcher = switcher;
			cases = cases;
		}
	), imports

and parse_for force_breaking imports s : pres =
	(* a; b; c -> {a}; {b}; {c} *)
	let ((init_start, _), init, imports) = parse_scope Code imports s in
	let ((next_start, next_last), next, imports) = parse_scope Code imports s in

	let peek = unwrap_peek_token s (Some next_start) in
	match peek.token with
	| Token.OpenBrace ->
		let ((_, block_last), block, imports) = parse_scope BreakingLoopBlock imports s in
		((init_start, block_last), Shell_ast.BreakingFor {
			init = init;
			iter = next;
			block = block;
		}), imports
	| _ ->
		if force_breaking then (
			syntax_error peek peek "expected breaking for loop, got standard."
		) else (
			let condition =
				match next with
				| [h] -> h
				| t -> (next_start, next_last), Shell_ast.Block t
			in
			let (_, iter, imports) = parse_scope Code imports s in
			let ((_, block_last), block, imports) = parse_scope StandardLoopBlock imports s in
			((init_start, block_last), Shell_ast.StandardFor {
				init = init;
				condition = condition;
				iter = iter;
				block = block;
			}), imports
		)

and parse_until force_breaking imports s : pres =
	(* until expr { ... } *)
	let peek = unwrap_peek_token s None in
	match peek.token with
	| Token.OpenBrace ->
		let (block_bnds, block, imports) = parse_scope BreakingLoopBlock imports s in
		(
			block_bnds,
			Shell_ast.BreakingUntil block
		), imports
	| _ ->
		if force_breaking then (
			syntax_error peek peek "expected breaking until loop, got standard."
		) else (
			let (condition, imports) = parse_expr imports s in
			let ((_, block_last), block, imports) = parse_scope StandardLoopBlock imports s in
			(
				(Shell_ast.start condition, block_last),
				Shell_ast.StandardUntil {
					condition = condition;
					block = block;
				}
			), imports
		)

and parse_lambda imports s : pres =
	(* fn(type,type,type)returntype {...} || fn(type,...)ret = code *)
	let ((params_start, _), params, imports) =
		parse_delimited
			~opening:Token.OpenParen ~closing:Token.ClosedParen ~delimiter:Token.Comma
			~delim_optional:true ~allow_none:true
			~allow_trailing:true ~unit_concept:"parameter type" ~unit_parser:parse_variable
			imports s
	in

	let (result, imports) =
		let next = unwrap_peek_token s (Some params_start) in
		match next.token with
		| Token.OpenBrace | Token.AssignmentEquals -> (None, imports)
		| t when is_type_annotation t -> (
			let (result, imports) = parse_type_annotation imports s in
			(Some result, imports)
		)
		| _ -> (None, imports)
	in

	let ((_, last), code, imports) = parse_scope Code imports s in
	((params_start, last), Shell_ast.Lambda {
		params = params;
		result = result;
		code = code;
	}), imports

and parse_tuple imports (expr : Shell_ast.fat_node) s : pres =
	let rec parse_tuple imports exprs s : (Lexer.fat_token * (Shell_ast.fat_node list) * (string list)) =
		let (expr, imports) = parse_expr_bp imports s max_int in
		let exprs = expr :: exprs in
		let next = unwrap_peek_token s None in
		match next.token with
		| Token.Comma -> (discard_pop s; parse_tuple imports exprs s)
		| _ -> (Shell_ast.last expr, exprs, imports)
	in
	let (last, exprs, imports) = parse_tuple imports [expr] s in
	(((Shell_ast.start expr, last), Shell_ast.TupleExpr exprs), imports)

and parse_expr imports s : pres =
	let (expr, imports) = parse_expr_bp imports s max_int in
	let next = unwrap_peek_token s None in
	match next.token with
	| Token.SemiColon -> (discard_pop s; (expr, imports))
	| _ -> (expr, imports)

and parse_expr_bp imports s (max_bp : int) : pres =
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
		| Token.OpenBrace -> (
			let (bnds, code, imports) = parse_scope Code imports s in
			(bnds, Shell_ast.Block code), imports
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
			if l_bp >= max_bp then
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

and parse_atom imports s : (Shell_ast.fat_node option) * (string list) =
	let start = unwrap_token s None in
	match start.token with
	| Token.Integer _ -> (Some ((start, start), Shell_ast.Integer start), imports)
	| Token.Float _ -> (Some ((start, start), Shell_ast.Float start), imports)
	| Token.Character _ -> (Some ((start, start), Shell_ast.Character start), imports)
	| Token.String _ -> (Some ((start, start), Shell_ast.String start), imports)
	| Token.True -> (Some ((start, start), Shell_ast.True start), imports)
	| Token.False -> (Some ((start, start), Shell_ast.False start), imports)
	| Token.Fn -> (
		let (((_, last), lambda), imports) = parse_lambda imports s in
		(Some ((start, last), lambda), imports)
	)
	| Token.If -> (
		let (res, imports) = parse_if imports s in
		(Some res, imports)
	)
	| Token.Until -> (
		let (res, imports) = parse_until true imports s in
		(Some res, imports)
	)
	| Token.For -> (
		let (res, imports) = parse_for true imports s in
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
		let (type_, imports) = parse_type_annotation imports s in
		let ((_, args_last), args, imports) = parse_scope ArrayInitializer imports s in
		(Some ((start, args_last), Shell_ast.ArrayInitializer { size = size; type_ = type_; args = args }), imports)
	)
	| Token.Id _ | Token.Scope -> (
		Lexer.push start s;
		let (ref_last, ref) = parse_id s in
		(Some ((start, ref_last), Shell_ast.Reference ref), imports)
	)
	| _ -> (None, imports)

and parse_postfix imports s (acc : Shell_ast.fat_node) : pres =
	let start = unwrap_peek_token s (Some (Shell_ast.start acc)) in
	match start.token with
	| Token.OpenParen -> (
		let ((_, args_last), args, imports) = parse_scope FunctionArguments imports s in
		parse_postfix imports s (
			(Shell_ast.start acc, args_last), Shell_ast.Call {
				from = acc;
				args = args;
			}
		)
	)
	| Token.OpenBracket -> (
		let ((_, args_last), args, imports) = parse_scope IndexArguments imports s in
		parse_postfix imports s (
			(Shell_ast.start acc, args_last), Shell_ast.Index {
				from = acc;
				args = args;
			}
		)
	)
	| Token.OpenBrace -> (
		match acc with
		| (_, Shell_ast.Reference _) -> (
			let initializer_parse_scope imports s : Lexer.fat_token * (Shell_ast.fat_node list) * (string list) =
				let unit_parser imports s =
					let (name, last) =
						expect_token s None
							(function | Token.Id name -> Some name | _ -> None)
							"expected an id to represent the member being bound or assigned."
					in

					match (unwrap_peek_token s (Some last)).token with
					| Token.Colon -> (
						discard_pop s;
						let (value, imports) = parse_expr imports s in
						((last, Shell_ast.last value), Shell_ast.MemberAssignment {
							name = name;
							value = value;
						}), imports
					)
					| _ -> ((last, last), Shell_ast.Reference (false, [name])), imports
				in

				let ((_, last), args, imports) =
					parse_delimited
						~opening:Token.OpenBrace
						~closing:Token.ClosedBrace
						~delimiter:Token.Comma
						~delim_optional:true
						~allow_none:true
						~allow_trailing:true
						~unit_concept:"initializer member assignment"
						~unit_parser
						imports s
				in

				(last, args, imports)
			in
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
	| _ -> acc, imports

and parse_type_annotation imports s : pres =
	let raw_type_annotation imports s =
		let start = unwrap_token s None in
		match start.token with
		| Token.Fn -> (
			(* fn(type,type,type)returntype *)
			let ((params_start, params_last), params, imports) =
				parse_scope FunctionTypeParams imports s
			in

			let (last, result, imports) =
				let next = unwrap_peek_token s (Some params_start) in
				match next.token with
				| t when is_type_annotation t -> (
					let (result, imports) = parse_type_annotation imports s in
					(Shell_ast.last result, Some result, imports)
				)
				| _ -> (params_last, None, imports)
			in
			((start, last), Shell_ast.FunctionType {
				params = params;
				result = result
			}), imports
		)
		| Token.Type -> (
			let ((_, members_last), members, imports) = parse_scope Type imports s in
			(
				(start, members_last),
				Shell_ast.Type {
					name = "";
					members = members;
				}
			), imports
		)
		| Token.Enum -> (
			let ((_, members_last), members, imports) = parse_scope Enum imports s in
			(
				(start, members_last),
				Shell_ast.Enum {
					name = "";
					members = members;
				}
			), imports
		)
		| Token.OpenParen -> (
			Lexer.push start s;
			let ((_, members_last), members, imports) = parse_scope Tuple imports s in
			match members with
			| [member] -> (member, imports) (* single tuple case *)
			| _ -> ((start, members_last), Shell_ast.Tuple members), imports
		)
		| Token.OpenBrace -> (
			Lexer.push start s;
			let ((_, members_last), members, imports) = parse_scope AnonymousEnum imports s in
			((start, members_last), Shell_ast.AnonymousEnum members), imports
		)
		| _ -> (
			Lexer.push start s;
			let (last, id) = parse_id s in
			((start, last), Shell_ast.Reference id), imports
		)
	in

	let start = unwrap_peek_token s None in
	match start.token with
	| t when ((Token.binaryify t) = Token.Multiply) -> (
		discard_pop s;
		let (inner, imports) = parse_type_annotation imports s in
		((start, Shell_ast.last inner), Pointer inner), imports
	)
	| _ -> raw_type_annotation imports s

and parse_id s : Lexer.fat_token * Shell_ast.id =
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

and stringify_id id : string =
	let (root, chain) = id in
	let rec loop (chain : string list) : string =
		match chain with
		| [] -> ""
		| [h] -> h
		| h::t -> h ^ "::" ^ (loop t)
	in (if root then "::" else "") ^ (loop chain)

let rec stringify_node node : string =
	let (_, node) = node in
	match node with
	| Shell_ast.Program files ->
			"Program [\n" ^
			(String.concat ";\n" (List.map (fun (path, block) ->
				"	 " ^ path ^ " -> " ^ stringify_block block
			) files)) ^
			"\n]"

	| Shell_ast.Pub inner ->
			"pub " ^ stringify_node inner

	| Shell_ast.Priv inner ->
			"priv " ^ stringify_node inner

	| Shell_ast.Stat inner ->
			"stat " ^ stringify_node inner

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
				"type " ^ stringify_block layer.members
			else
				"type " ^ layer.name ^ " " ^ stringify_block layer.members

	| Shell_ast.Alias {name;annotation} ->
			"type " ^ name ^ " = " ^ (stringify_node annotation)

	| Shell_ast.Tuple block ->
			stringify_block ~op:"(" ~cl:")" ~sep:"," block

	| Shell_ast.Function fn ->
			"fn " ^ fn.name ^ stringify_block ~op:"(" ~cl:")" ~sep:"," fn.params ^
			(match fn.result with
			 | None -> ""
			 | Some r -> " " ^ stringify_node r) ^
			" " ^ stringify_block fn.code

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
			" " ^ stringify_block code

	| Shell_ast.EnumVariant { name; type_ } ->
			name ^ " " ^ stringify_node type_

	| Shell_ast.Enum layer ->
			if layer.name = "" then
				"enum " ^ stringify_block layer.members
			else
				"enum " ^ layer.name ^ " " ^ stringify_block layer.members

	| Shell_ast.AnonymousEnum block ->
			stringify_block ~sep:"," block

	| Shell_ast.Pointer inner ->
			"*" ^ (stringify_node inner)

	| Shell_ast.FunctionType { params; result } ->
		"fn" ^ (stringify_block ~op:"(" ~cl:")" ~sep:"," params) ^ ((match result with | Some r -> stringify_node r | None -> ""))

	| Shell_ast.Lambda { params; result; code } ->
		"fn" ^ (stringify_block ~op:"(" ~cl:")" ~sep:"," params) ^ (match result with
			| Some ty -> stringify_node ty
			| None -> "") ^ " " ^ (stringify_block code)

	| Shell_ast.Variadic inner ->
		"..." ^ (stringify_node inner)

	| Shell_ast.Space { name; members } ->
			"space " ^ stringify_id name ^ " " ^ stringify_block members

	| Shell_ast.Use id ->
			"use " ^ stringify_id id

	| Shell_ast.Import { path } ->
			"import \"" ^ path ^ "\""

	| Shell_ast.If { condition; true_block; false_block } ->
			"if " ^ stringify_node condition ^
			" " ^ stringify_block true_block ^
			(match false_block with
			 | None -> ""
			 | Some fb -> " else " ^ stringify_block fb)

	| Shell_ast.Match { switcher; cases } ->
			"match " ^ stringify_node switcher ^ " " ^ stringify_block cases

	| Shell_ast.MatchCase { pattern; guard; logic } ->
			stringify_node pattern ^
			(match guard with
			| Some g -> " if " ^ (stringify_node g)
			| None -> " ") ^
			(match logic with
			 | None -> ""
			 | Some l -> " -> " ^ stringify_node l)

	| Shell_ast.StandardFor { init; condition; iter; block } ->
			"for " ^ (stringify_block init) ^ "; " ^ (stringify_node condition) ^ "; " ^ (stringify_block iter) ^ " " ^ (stringify_block block)

	| Shell_ast.BreakingFor { init; iter; block } ->
			"for " ^ (stringify_block init) ^ "; " ^ (stringify_block iter) ^ " " ^ (stringify_block block)

	| Shell_ast.StandardUntil { condition; block } ->
			"until " ^ stringify_node condition ^ " " ^ (stringify_block block)

	| Shell_ast.BreakingUntil block ->
			"until " ^ (stringify_block block)

	| Shell_ast.Block nodes ->
			stringify_block nodes

	| Shell_ast.StandardBreak ->
			"brk"

	| Shell_ast.YieldingBreak expr ->
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
			stringify_node from ^ stringify_block ~op:"(" ~cl:")" ~sep:"," args

	| Shell_ast.Index { from; args } ->
			stringify_node from ^ "[" ^
			(String.concat ", " (List.map stringify_node args)) ^
			"]"

	| Shell_ast.Initializer { type_; args } ->
			stringify_node type_ ^ " { " ^
			(String.concat ", " (List.map stringify_node args)) ^
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
			"]" ^ (stringify_node type_) ^ " " ^ stringify_block ~sep:"," args

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

	| Shell_ast.MemberAssignment {name; value} ->
		name ^ ": " ^ (stringify_node value)

and stringify_block ?(op="{") ?(cl="}") ?(sep=";") things =
	op ^ " " ^ (String.concat (sep ^ " ") (List.map stringify_node things)) ^ " " ^ cl

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
						let (_, root_list, imports) = parse allow_root_expr [] lexer in

						with_mutex state.mutex (fun () ->
							state.parsed := StringMap.add path root_list !(state.parsed));

						let rec loop imports : unit =
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
