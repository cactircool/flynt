(* Comprehensive parser tests using Alcotest *)

open Flynt
open Shell_parser
open Shell_ast

(* Helper to create a test lexer from a string *)
let lexer_from_string (s : string) : Lexer.t =
	(* let flow = Eio_mock.Flow.make s in
	Lexer.init (flow :> _ Eio.Flow.source) 4096 *)
	Lexer.init (Eio.Flow.string_source s) 4096

(* Helper to parse a string and return the node *)
let parse_string (s : string) : fat_node * string list =
	let lexer = lexer_from_string s in
	parse true [] lexer

(* Helper to extract node from fat_node *)
let extract_node (fn : fat_node) : node =
	let (_, n) = fn in n

(* Test cases *)
let test_simple_variable () =
	let (result, _) = parse_string "let x = 5" in
	match extract_node result with
	| Block [((_, _), Variable { name = "x"; type_ = None; value = Some _ })] -> ()
	| _ -> Alcotest.fail ("Expected simple variable declaration. Got: " ^ (stringify_node result))

let test_variable_with_type () =
	let (result, _) = parse_string "let x i32 = 5" in
	match extract_node result with
	| Block [((_, _), Variable { name = "x"; type_ = Some _; value = Some _ })] -> ()
	| _ -> Alcotest.fail "Expected variable with type annotation"

let test_variable_without_value () =
	let (result, _) = parse_string "let x i32" in
	match extract_node result with
	| Block [((_, _), Variable { name = "x"; type_ = Some _; value = None })] -> ()
	| _ -> Alcotest.fail "Expected variable without value"

let test_simple_function () =
	let (result, _) = parse_string "fn test() { }" in
	match extract_node result with
	| Block [((_, _), Function { name = "test"; params = _; result = None; code = _ })] -> ()
	| _ -> Alcotest.fail "Expected simple function"

let test_function_with_params () =
	let (result, _) = parse_string "fn add(a i32, b i32) i32 = a + b" in
	Printf.printf "stringify_node -> %s\n" (stringify_node result);
	match extract_node result with
	| Block [((_, _), Function { name = "add"; params = _; result = Some _; code = _ })] -> ()
	| _ -> Alcotest.fail "Expected function with parameters and return type"

let test_type_declaration () =
	let (result, _) = parse_string "type Point { let x i32 let y i32 }" in
	match extract_node result with
	| Block [((_, _), Type { name = "Point"; members = _ })] -> ()
	| _ -> Alcotest.fail "Expected type declaration"

let test_type_alias () =
	let (result, _) = parse_string "type MyInt = i32" in
	match extract_node result with
	| Block [((_, _), Alias ("MyInt", _))] -> ()
	| _ -> Alcotest.fail "Expected type alias"

let test_enum_declaration () =
	let (result, _) = parse_string "enum Option { Some i32 None unit }" in
	match extract_node result with
	| Block [((_, _), Enum { name = "Option"; members = _ })] -> ()
	| _ -> Alcotest.fail "Expected enum declaration"

let test_space_declaration () =
	let (result, _) = parse_string "space math { fn add(a i32, b i32) i32 = a + b }" in
	match extract_node result with
	| Block [((_, _), Space { name = (false, ["math"]); members = _ })] -> ()
	| _ -> Alcotest.fail "Expected space declaration"

let test_use_statement () =
	let (result, _) = parse_string "use std::io" in
	match extract_node result with
	| Block [((_, _), Use (false, ["std"; "io"]))] -> ()
	| _ -> Alcotest.fail "Expected use statement"

let test_use_statement_rooted () =
	let (result, _) = parse_string "use ::std::io" in
	match extract_node result with
	| Block [((_, _), Use (true, ["std"; "io"]))] -> ()
	| _ -> Alcotest.fail "Expected rooted use statement"

let test_import_statement () =
	let (result, imports) = parse_string "import \"file.lang\"" in
	match extract_node result with
	| Block [((_, _), Import { path = "file.lang" })] when imports = ["file.lang"] -> ()
	| _ -> Alcotest.fail "Expected import statement"

let test_if_statement () =
	let (result, _) = parse_string "if true { 5 }" in
	match extract_node result with
	| Block [((_, _), If { condition = _; true_block = _; false_block = None })] -> ()
	| _ -> (Printf.printf "%s\n" (stringify_node result); Alcotest.fail "Expected if statement")

let test_if_else_statement () =
	let (result, _) = parse_string "if (x) { 5 } else { 10 }" in
	match extract_node result with
	| Block [((_, _), If { condition = _; true_block = _; false_block = Some _ })] -> ()
	| _ -> Alcotest.fail "Expected if-else statement"

let test_match_statement () =
	let (result, _) = parse_string "match (x) { 1 -> 5 2 -> 10 }" in
	match extract_node result with
	| Block [((_, _), Match { switcher = _; cases = _ })] -> ()
	| _ -> Alcotest.fail "Expected match statement"

let test_for_loop () =
	let (result, _) = parse_string "for let i = 0; i < 10; i = i + 1 { }" in
	match extract_node result with
	| Block [((_, _), For { params = _; block = _ })] -> ()
	| _ -> Alcotest.fail "Expected for loop"

let test_until_loop () =
	let (result, _) = parse_string "until (x) { }" in
	match extract_node result with
	| Block [((_, _), Until { condition = _; block = _ })] -> ()
	| _ -> Alcotest.fail "Expected until loop"

let test_binary_expression () =
	let (result, _) = parse_string "x + y" in
	match extract_node result with
	| Block [((_, _), Binary { left = _; op = Token.Add; right = Some _ })] -> ()
	| _ -> Alcotest.fail "Expected binary expression"

let test_unary_left_expression () =
	let (result, _) = parse_string "!x" in
	match extract_node result with
	| Block [((_, _), Unary { op = Token.LeftNot; arg = Some _ })] -> ()
	| _ -> Alcotest.fail "Expected left unary expression"

let test_unary_right_expression () =
	let (result, _) = parse_string "x++" in
	match extract_node result with
	| Block [((_, _), Unary { op = Token.RightIncrement; arg = Some _ })] -> ()
	| _ -> (Printf.printf "got: %s\n" (stringify_node result); Alcotest.fail "Expected right unary expression")

let test_function_call () =
	let (result, _) = parse_string "foo(1, 2, 3)" in
	match extract_node result with
	| Block [((_, _), Call { from = _; args = _ })] -> ()
	| _ -> Alcotest.fail "Expected function call"

let test_index_expression () =
	let (result, _) = parse_string "arr[0]" in
	match extract_node result with
	| Block [((_, _), Index { from = _; args = _ })] -> ()
	| _ -> Alcotest.fail "Expected index expression"

let test_array_initializer () =
	let (result, _) = parse_string "[5]i32 { 1, 2, 3 }" in
	match extract_node result with
	| Block [((_, _), ArrayInitializer { size = Some _; type_ = (false, ["i32"]); args = _ })] -> ()
	| _ -> Alcotest.fail "Expected array initializer"

let test_initializer_expression () =
	let (result, _) = parse_string "Point { x: 1, y: 2 }" in
	match extract_node result with
	| Block [((_, _), Initializer { type_ = _; args = [("x", _); ("y", _)] })] -> ()
	| _ -> Alcotest.fail "Expected initializer expression"

let test_integer_literal () =
	let (result, _) = parse_string "42" in
	match extract_node result with
	| Block [((_, _), Integer _)] -> ()
	| _ -> Alcotest.fail "Expected integer literal"

let test_string_literal () =
	let (result, _) = parse_string "\"hello\"" in
	match extract_node result with
	| Block [((_, _), String _)] -> ()
	| _ -> Alcotest.fail "Expected string literal"

let test_true_literal () =
	let (result, _) = parse_string "true" in
	match extract_node result with
	| Block [((_, _), True _)] -> ()
	| _ -> Alcotest.fail "Expected true literal"

let test_false_literal () =
	let (result, _) = parse_string "false" in
	match extract_node result with
	| Block [((_, _), False _)] -> ()
	| _ -> Alcotest.fail "Expected false literal"

let test_pub_modifier () =
	let (result, _) = parse_string "pub fn test() { }" in
	match extract_node result with
	| Block [((_, _), Pub ((_, _), Function _))] -> ()
	| _ -> Alcotest.fail "Expected pub modifier"

let test_priv_modifier () =
	let (result, _) = parse_string "priv let x = 5" in
	match extract_node result with
	| Block [((_, _), Priv ((_, _), Variable _))] -> ()
	| _ -> Alcotest.fail "Expected priv modifier"

let test_stat_modifier () =
	let (result, _) = parse_string "type hi { stat fn helper() { } }" in
	match extract_node result with
	| Block [((_, _), Type { name = "hi"; members = (_, Block [((_, _), Stat ((_, _), Function _))]) })] -> ()
	| _ -> Alcotest.fail "Expected stat modifier"

let test_operator_overload_binary () =
	let (result, _) = parse_string "oper (a i32 + b i32) i32 { a + b }" in
	match extract_node result with
	| Block [((_, _), OperatorOverload { name = Token.Add; params = [_; _]; result = Some _; code = _ })] -> ()
	| _ -> Alcotest.fail "Expected binary operator overload"

let test_operator_overload_left () =
	let (result, _) = parse_string "oper (!a bool) bool { false }" in
	match extract_node result with
	| Block [((_, _), OperatorOverload { name = Token.LeftNot; params = [_]; result = Some _; code = _ })] -> ()
	| _ -> Alcotest.fail "Expected left unary operator overload"

let test_operator_overload_right () =
	let (result, _) = parse_string "oper (a i32++) i32 { a + 1 }" in
	match extract_node result with
	| Block [((_, _), OperatorOverload { name = Token.RightIncrement; params = [_]; result = Some _; code = _ })] -> ()
	| _ -> Alcotest.fail "Expected right unary operator overload"

let test_tuple_type () =
	let (result, _) = parse_string "let x (i32, i32) = (1, 2)" in
	match extract_node result with
	| Block [((_, _), Variable { type_ = Some ((_, _), Tuple _); _ })] -> ()
	| _ -> Alcotest.fail "Expected tuple type"

let test_anonymous_enum () =
	let (result, _) = parse_string "let x {i32, f32} = 5" in
	match extract_node result with
	| Block [((_, _), Variable { type_ = Some ((_, _), AnonymousEnum _); _ })] -> ()
	| _ -> Alcotest.fail "Expected anonymous enum type"

let test_break_statement () =
	let (result, _) = parse_string "for let i = 0; i < 10; i = i + 1 { brk; }" in
	match extract_node result with
	| Block [((_, _), For { block = ((_, _), Block [((_, _), Break None)]); _ })] -> ()
	| _ -> Alcotest.fail "Expected break statement"

let test_break_with_value () =
	let (result, _) = parse_string "for let i = 0; i < 10; i = i + 1 { brk 5 }" in
	match extract_node result with
	| Block [((_, _), For { block = ((_, _), Block [((_, _), Break (Some _))]); _ })] -> ()
	| _ -> Alcotest.fail "Expected break with value"

let test_continue_statement () =
	let (result, _) = parse_string "for let i = 0; i < 10; i = i + 1 { cont }" in
	match extract_node result with
	| Block [((_, _), For { block = ((_, _), Block [((_, _), Continue)]); _ })] -> ()
	| _ -> Alcotest.fail "Expected continue statement"

let test_nested_expressions () =
	let (result, _) = parse_string "(x + y) * z" in
	match extract_node result with
	| Block [((_, _), Binary { op = Token.Multiply; _ })] -> ()
	| _ -> Alcotest.fail "Expected nested expression with correct precedence"

let test_chained_calls () =
	let (result, _) = parse_string "foo().bar().baz()" in
	match extract_node result with
	| (Shell_ast.Block
		[
			(_, Shell_ast.Binary {
				left = (_, Shell_ast.Call {
					from = (_, (Shell_ast.Reference (false, ["foo"])));
					args = (_, (Shell_ast.Block []))
				});
				op = Token.Dot;
				right = (Some (_, Shell_ast.Binary {
					left = (_, Shell_ast.Call {
						from = (_, (Shell_ast.Reference (false, ["bar"])));
						args = (_, (Shell_ast.Block []))
					});
					op = Token.Dot;
					right = (Some (_, Shell_ast.Call {
						from = (_, (Shell_ast.Reference (false, ["baz"])));
						args = (_, (Shell_ast.Block []))
					}))
				}))
			})
     ]) -> ()
	| _ -> Alcotest.fail "Expected chained function calls"

let test_complex_initializer () =
	let (result, _) = parse_string "Point { x: 1 + 2, y: foo(3) }" in
	match extract_node result with
	| Block [((_, _), Initializer { args = [("x", ((_, _), Binary _)); ("y", ((_, _), Call _))]; _ })] -> ()
	| _ -> Alcotest.fail "Expected complex initializer"

let test_multiple_statements () =
	let (result, _) = parse_string "let x = 5; let y = 10; x + y" in
	match extract_node result with
	| Block [_; _; _] -> ()
	| _ -> Alcotest.fail "Expected multiple statements in block"

let test_nested_blocks () =
	let (result, _) = parse_string "{ { 5 } }" in
	match extract_node result with
	| Block [((_, _), Block [((_, _), Block [_])])] -> ()
	| _ -> Alcotest.fail "Expected nested blocks"

let test_scope_resolution () =
	let (result, _) = parse_string "std::io::println" in
	match extract_node result with
	| Block [((_, _), Reference (false, ["std"; "io"; "println"]))] -> ()
	| _ -> Alcotest.fail "Expected scope resolution"

let test_rooted_scope_resolution () =
	let (result, _) = parse_string "::std::io" in
	match extract_node result with
	| Block [((_, _), Reference (true, ["std"; "io"]))] -> ()
	| _ -> Alcotest.fail "Expected rooted scope resolution"

let test_hello_world () =
	let (result, _) = parse_string "import \"io\"\nfn main(argc i32, argv *char) {\n\tio.Println(\"Hello world!\")\n}\n" in
	match extract_node result with
	| Block [((_, _), Reference (true, ["std"; "io"]))] -> ()
	| _ -> Alcotest.fail "Expected rooted scope resolution"

(* Collect all tests *)
let tests = [
	Alcotest.test_case "simple variable" `Quick test_simple_variable;
	Alcotest.test_case "variable with type" `Quick test_variable_with_type;
	Alcotest.test_case "variable without value" `Quick test_variable_without_value;
	Alcotest.test_case "simple function" `Quick test_simple_function;
	Alcotest.test_case "function with params" `Quick test_function_with_params;
	Alcotest.test_case "type declaration" `Quick test_type_declaration;
	Alcotest.test_case "type alias" `Quick test_type_alias;
	Alcotest.test_case "enum declaration" `Quick test_enum_declaration;
	Alcotest.test_case "space declaration" `Quick test_space_declaration;
	Alcotest.test_case "use statement" `Quick test_use_statement;
	Alcotest.test_case "use statement rooted" `Quick test_use_statement_rooted;
	Alcotest.test_case "import statement" `Quick test_import_statement;
	Alcotest.test_case "if statement" `Quick test_if_statement;
	Alcotest.test_case "if-else statement" `Quick test_if_else_statement;
	Alcotest.test_case "match statement" `Quick test_match_statement;
	Alcotest.test_case "for loop" `Quick test_for_loop;
	Alcotest.test_case "until loop" `Quick test_until_loop;
	Alcotest.test_case "binary expression" `Quick test_binary_expression;
	Alcotest.test_case "unary left expression" `Quick test_unary_left_expression;
	Alcotest.test_case "unary right expression" `Quick test_unary_right_expression;
	Alcotest.test_case "function call" `Quick test_function_call;
	Alcotest.test_case "index expression" `Quick test_index_expression;
	Alcotest.test_case "array initializer" `Quick test_array_initializer;
	Alcotest.test_case "initializer expression" `Quick test_initializer_expression;
	Alcotest.test_case "integer literal" `Quick test_integer_literal;
	Alcotest.test_case "string literal" `Quick test_string_literal;
	Alcotest.test_case "true literal" `Quick test_true_literal;
	Alcotest.test_case "false literal" `Quick test_false_literal;
	Alcotest.test_case "pub modifier" `Quick test_pub_modifier;
	Alcotest.test_case "priv modifier" `Quick test_priv_modifier;
	Alcotest.test_case "stat modifier" `Quick test_stat_modifier;
	Alcotest.test_case "operator overload binary" `Quick test_operator_overload_binary;
	Alcotest.test_case "operator overload left" `Quick test_operator_overload_left;
	Alcotest.test_case "operator overload right" `Quick test_operator_overload_right;
	Alcotest.test_case "tuple type" `Quick test_tuple_type;
	Alcotest.test_case "anonymous enum" `Quick test_anonymous_enum;
	Alcotest.test_case "break statement" `Quick test_break_statement;
	Alcotest.test_case "break with value" `Quick test_break_with_value;
	Alcotest.test_case "continue statement" `Quick test_continue_statement;
	Alcotest.test_case "nested expressions" `Quick test_nested_expressions;
	Alcotest.test_case "chained calls" `Quick test_chained_calls;
	Alcotest.test_case "complex initializer" `Quick test_complex_initializer;
	Alcotest.test_case "multiple statements" `Quick test_multiple_statements;
	Alcotest.test_case "nested blocks" `Quick test_nested_blocks;
	Alcotest.test_case "scope resolution" `Quick test_scope_resolution;
	Alcotest.test_case "rooted scope resolution" `Quick test_rooted_scope_resolution;
]
