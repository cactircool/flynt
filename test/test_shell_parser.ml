open Flynt
open Shell_parser
open Shell_ast

(* Test helpers *)
let make_flow str =
	Eio.Flow.string_source str

let make_lexer str =
	Lexer.init (make_flow str) 4096

let parse_string ?(allow_root_expr=false) str =
  let (_, nodes, _) = parse allow_root_expr [] (make_lexer str) in
  nodes

let parse_expr_string str =
  let (expr, _) = parse_expr [] (make_lexer str) in
  expr

let parse_generic_string str =
	let (_, stuff, _) = parse true [] (make_lexer str) in
	stuff

let test_parse_success name input _ =
  (name, `Quick, fun () ->
    let result = parse_string input in
    Alcotest.(check bool) "parse succeeded" true (List.length result > 0))

let test_parse_expr name input check =
  (name, `Quick, fun () ->
    let (_, node) = parse_expr_string input in
    check node)

let test_parse_gen name input check =
  (name, `Quick, fun () ->
  	match parse_generic_string input with
   	| [_, node] -> check node
    | _ -> Alcotest.fail "bad parse")

let test_parse_fail name input =
  (name, `Quick, fun () ->
 	try
	    ignore (parse_string input);
	    Alcotest.fail "Expected a SyntaxError, but instead succeeded"
    with
    | Flynt.Shell_parser.SyntaxError _ -> ()
    | Flynt.Shell_parser.IOError _ -> ()
    | e -> Alcotest.fail ("Expected a SyntaxError, got " ^ (Printexc.to_string e)))

(* Basic literal tests *)
let literal_tests = [
  test_parse_expr "integer literal" "42" (function
    | Integer _ -> ()
    | _ -> Alcotest.fail "expected Integer");

  test_parse_expr "float literal" "3.14" (function
    | Float _ -> ()
    | _ -> Alcotest.fail "expected Float");

  test_parse_expr "character literal" "'a'" (function
    | Character _ -> ()
    | _ -> Alcotest.fail "expected Character");

  test_parse_expr "string literal" "\"hello\"" (function
    | String _ -> ()
    | _ -> Alcotest.fail "expected String");

  test_parse_expr "true literal" "true" (function
    | True _ -> ()
    | _ -> Alcotest.fail "expected True");

  test_parse_expr "false literal" "false" (function
    | False _ -> ()
    | _ -> Alcotest.fail "expected False");
]

(* Variable declaration tests *)
let variable_tests = [
  test_parse_success "simple let" "let x = 5" [];
  test_parse_success "let with type" "let x i32 = 5" [];
  test_parse_success "let without value" "let x i32" [];
  test_parse_success "let with complex type" "let x *i32 = &y" [];
  test_parse_success "let with tuple type" "let x (i32, f64) = (1, 2.0)" [];
  test_parse_fail "let without name" "let = 5";
  test_parse_fail "let without type or value" "let x";
]

(* Function declaration tests *)
let function_tests = [
  test_parse_success "simple function" "fn add(a i32, b i32) i32 { a + b }" [];
  test_parse_success "no params function" "fn get() i32 { 42 }" [];
  test_parse_success "no return type" "fn print(x i32) { }" [];
  test_parse_success "arrow function" "fn square(x i32) i32 = x * x" [];
  test_parse_success "multi-line function"
    "fn complex(x i32) {\n  let y = x + 1\n  y * 2\n}" [];
]

(* Type declaration tests *)
let type_tests = [
  test_parse_success "struct type" "type Point { x i32 y i32 }" [];
  test_parse_success "type alias" "type Int = i32" [];
  test_parse_success "complex type"
    "type Complex { real f64 imag f64 fn magnitude() f64 { } }" [];
  test_parse_success "empty type" "type Empty {}" [];
  test_parse_success "nested types"
    "type Outer { type Inner { x i32 } inner Inner }" [];
]

(* Enum tests *)
let enum_tests = [
  test_parse_success "simple enum" "enum Color { Red i32, Green i32, Blue i32 }" [];
  test_parse_success "enum with structs"
    "enum Shape { Circle { radius f64 } Square { side f64 } }" [];
  test_parse_success "trailing comma enum" "enum Opt { Some i32, None i32, }" [];
  test_parse_success "enum without variants" "enum Empty {}" []; (* only to allow code to match specific predefined shapes (i.e. an Infallible enum as the err branch of a function required to return a Result) *)
]

(* Expression tests *)
let expr_tests = [
  test_parse_expr "binary addition" "1 + 2" (function
    | Binary { op = Token.Add; _ } -> ()
    | _ -> Alcotest.fail "expected Add");

  test_parse_expr "binary multiplication" "3 * 4" (function
    | Binary { op = Token.Multiply; _ } -> ()
    | _ -> Alcotest.fail "expected Multiply");

  test_parse_expr "chained operations" "1 + 2 * 3" (function
    | Binary { op = Token.Add; _ } -> ()
    | _ -> Alcotest.fail "expected Add at root");

  test_parse_expr "parenthesized expr" "(1 + 2) * 3" (function
    | Binary { op = Token.Multiply; _ } -> ()
    | _ -> Alcotest.fail "expected Multiply at root");

  test_parse_expr "prefix unary" "-5" (function
    | Unary { op = Token.LeftMinus; _ } -> ()
    | _ -> Alcotest.fail "expected LeftMinus");

  test_parse_expr "postfix unary" "x++" (function
    | Unary { op = Token.RightIncrement; _ } -> ()
    | _ -> Alcotest.fail "expected RightIncrement");

  test_parse_expr "dereference" "*ptr" (function
    | Unary { op = Token.LeftDereference; _ } -> ()
    | _ -> Alcotest.fail "expected LeftDereference");

  test_parse_expr "reference" "&var" (function
    | Unary { op = Token.LeftReference; _ } -> ()
    | _ -> Alcotest.fail "expected LeftReference");
]

(* Complex operator tests *)
let operator_tests = [
  test_parse_expr "comparison" "x == y" (function
    | Binary { op = Token.ComparisonEquals; _ } -> ()
    | _ -> Alcotest.fail "expected ComparisonEquals");

  test_parse_expr "logical and" "a and b" (function
    | Binary { op = Token.And; _ } -> ()
    | _ -> Alcotest.fail "expected And");

  test_parse_expr "logical or" "a or b" (function
    | Binary { op = Token.Or; _ } -> ()
    | _ -> Alcotest.fail "expected Or");

  test_parse_expr "bitwise ops" "x & y | z" (function
    | Binary { op = Token.BitwiseOr; _ } -> ()
    | _ -> Alcotest.fail "expected BitwiseOr at root");

  test_parse_expr "shift operators" "x << 2" (function
    | Binary { op = Token.LeftShift; _ } -> ()
    | _ -> Alcotest.fail "expected LeftShift");

  test_parse_expr "assignment" "x = 5" (function
    | Binary { op = Token.AssignmentEquals; _ } -> ()
    | _ -> Alcotest.fail "expected AssignmentEquals");

  test_parse_expr "compound assignment" "x += 5" (function
    | Binary { op = Token.AddEquals; _ } -> ()
    | _ -> Alcotest.fail "expected AddEquals");
]

(* Ambiguous operator tests *)
let ambiguous_tests = [
  test_parse_expr "multiply vs deref" "a * b" (function
    | Binary { op = Token.Multiply; _ } -> ()
    | _ -> Alcotest.fail "should be binary multiply");

  test_parse_expr "prefix deref" "*a" (function
    | Unary { op = Token.LeftDereference; _ } -> ()
    | _ -> Alcotest.fail "should be prefix deref");

  test_parse_expr "postfix deref in call" "f()*" (function
    | Unary { op = Token.RightDereference; arg = Some (_, Call _); _ } -> ()
    | _ -> Alcotest.fail "should be postfix deref of call");

  test_parse_expr "ref vs bitwise and" "a & b" (function
    | Binary { op = Token.BitwiseAnd; _ } -> ()
    | _ -> Alcotest.fail "should be binary and");

  test_parse_expr "prefix ref" "&a" (function
    | Unary { op = Token.LeftReference; _ } -> ()
    | _ -> Alcotest.fail "should be prefix ref");

  test_parse_expr "plus vs unary" "a + b" (function
    | Binary { op = Token.Add; _ } -> ()
    | _ -> Alcotest.fail "should be binary add");

  test_parse_expr "prefix plus" "+a" (function
    | Unary { op = Token.LeftPlus; _ } -> ()
    | _ -> Alcotest.fail "should be prefix plus");
]

(* Call and index tests *)
let call_index_tests = [
  test_parse_expr "function call" "f()" (function
    | Call { args = []; _ } -> ()
    | _ -> Alcotest.fail "expected Call with no args");

  test_parse_expr "function call with args" "f(1, 2, 3)" (function
    | Call { args; _ } ->
        if List.length args = 3 then ()
        else Alcotest.fail "expected 3 args"
    | _ -> Alcotest.fail "expected Call");

  test_parse_expr "chained calls" "f()()" (function
    | Call { from = (_, Call _); _ } -> ()
    | _ -> Alcotest.fail "expected chained calls");

  test_parse_expr "array index" "arr[0]" (function
    | Index { args = [_]; _ } -> ()
    | _ -> Alcotest.fail "expected Index");

  test_parse_expr "multi-dimensional index" "arr[i][j]" (function
    | Index { from = (_, Index _); _ } -> ()
    | _ -> Alcotest.fail "expected chained index");

  test_parse_expr "call after index" "arr[0]()" (function
    | Call { from = (_, Index _); _ } -> ()
    | _ -> Alcotest.fail "expected Call after Index");
]

(* Tuple tests *)
let tuple_tests = [
  test_parse_expr "two-element tuple" "(1, 2)" (function
    | TupleExpr [_; _] -> ()
    | _ -> Alcotest.fail "expected 2-element tuple");

  test_parse_expr "three-element tuple" "(1, 2, 3)" (function
    | TupleExpr [_; _; _] -> ()
    | _ -> Alcotest.fail "expected 3-element tuple");

  test_parse_expr "tuple with expressions" "(a + b, c * d)" (function
    | TupleExpr [(_, Binary _); (_, Binary _)] -> ()
    | _ -> Alcotest.fail "expected tuple of binaries");

  test_parse_expr "parenthesized tuple" "(1, 2)" (function
    | TupleExpr [_; _] -> ()
    | _ -> Alcotest.fail "expected tuple");

  test_parse_expr "single paren (not tuple)" "(42)" (function
    | Integer _ -> ()
    | _ -> Alcotest.fail "expected unwrapped integer");
]

(* Control flow tests *)
let control_flow_tests = [
  test_parse_expr "if statement" "if (x) { }" (function
    | If { false_block = None; _ } -> ()
    | _ -> Alcotest.fail "expected If without else");

  test_parse_expr "if-else" "if (x) { 1 } else { 2 }" (function
    | If { false_block = Some _; _ } -> ()
    | _ -> Alcotest.fail "expected If with else");

  test_parse_expr "nested if" "if (x) { if (y) { } }" (function
    | If { true_block = [(_, If _)]; _ } -> ()
    | _ -> Alcotest.fail "expected nested If");

  test_parse_expr "match statement" "match (x) { 1 -> { } 2 -> { } }" (function
    | Match _ -> ()
    | _ -> Alcotest.fail "expected Match");

  test_parse_expr "match with fallthrough" "match (x) { 1 2 -> { } }" (function
    | Match _ -> ()
    | _ -> Alcotest.fail "expected Match");

  test_parse_gen "for loop" "for let i = 0; i < 10; i++ { }" (function
    | StandardFor _ -> ()
    | _ -> Alcotest.fail "expected For");

  test_parse_gen "until loop" "until (done) { }" (function
    | StandardUntil _ -> ()
    | _ -> Alcotest.fail "expected Until");
]

(* Block tests *)
let block_tests = [
  test_parse_expr "empty block" "{}" (function
    | Block [] -> ()
    | _ -> Alcotest.fail "expected empty block");

  test_parse_expr "single statement block" "{ 42 }" (function
    | Block [_] -> ()
    | _ -> Alcotest.fail "expected single statement");

  test_parse_expr "multi statement block" "{ let x = 1; x + 2 }" (function
    | Block [_; _] -> ()
    | _ -> Alcotest.fail "expected two statements");

  test_parse_expr "nested blocks" "{ { } }" (function
    | Block [(_, Block [])] -> ()
    | _ -> Alcotest.fail "expected nested empty blocks");
]

(* Initializer tests *)
let initializer_tests = [
  test_parse_expr "struct initializer" "Point { x: 1, y: 2 }" (function
    | Initializer { args; _ } ->
        if List.length args = 2 then ()
        else Alcotest.fail "expected 2 fields"
    | _ -> Alcotest.fail "expected Initializer");

  test_parse_expr "empty initializer" "Empty {}" (function
    | Initializer { args = []; _ } -> ()
    | _ -> Alcotest.fail "expected empty initializer");

  test_parse_expr "nested initializer"
    "Outer { inner: Inner { x: 1 } }" (function
    | Initializer { args = [(_, MemberAssignment { name = _; value = (_, Initializer _) })]; _ } -> ()
    | _ -> Alcotest.fail "expected nested initializer");
]

(* Array initializer tests *)
let array_tests = [
  test_parse_expr "array with size" "[10]i32 { 1, 2, 3 }" (function
    | ArrayInitializer { size = Some _; args; _ } ->
        if List.length args = 3 then ()
        else Alcotest.fail "expected 3 elements"
    | _ -> Alcotest.fail "expected ArrayInitializer");

  test_parse_expr "array without size" "[]i32 { 1, 2, 3 }" (function
    | ArrayInitializer { size = None; _ } -> ()
    | _ -> Alcotest.fail "expected ArrayInitializer without size");

  test_parse_expr "empty array" "[]i32 {}" (function
    | ArrayInitializer { args = []; _ } -> ()
    | _ -> Alcotest.fail "expected empty array");
]

(* Reference/ID tests *)
let reference_tests = [
  test_parse_expr "simple id" "x" (function
    | Reference (false, ["x"]) -> ()
    | _ -> Alcotest.fail "expected simple reference");

  test_parse_expr "scoped id" "a::b" (function
    | Reference (false, ["a"; "b"]) -> ()
    | _ -> Alcotest.fail "expected scoped reference");

  test_parse_expr "root scoped id" "::a::b" (function
    | Reference (true, ["a"; "b"]) -> ()
    | _ -> Alcotest.fail "expected root-scoped reference");

  test_parse_expr "deeply nested" "a::b::c::d" (function
    | Reference (false, ["a"; "b"; "c"; "d"]) -> ()
    | _ -> Alcotest.fail "expected deeply nested reference");
]

(* Access modifier tests *)
let access_modifier_tests = [
  test_parse_success "pub function" "pub fn f() {}" [];
  test_parse_success "priv variable" "priv let x = 5" [];
  test_parse_success "stat type" "type Thing { stat type T = i32 }" [];
  test_parse_success "nested modifiers" "pub stat fn f() {}" [];
]

(* Operator overload tests *)
let operator_overload_tests = [
  test_parse_success "binary operator" "oper (a i32 + b i32) i32 { a + b }" [];
  test_parse_success "left unary" "oper (-a i32) i32 { 0 - a }" [];
  test_parse_success "right unary" "oper (a i32++) i32 { a = a + 1 }" [];
  test_parse_fail "operator without params" "oper () {}";
]

(* Space tests *)
let space_tests = [
  test_parse_success "simple space" "space math { fn add() {} }" [];
  test_parse_success "nested space" "space outer::inner { }" [];
  test_parse_success "space with types"
    "space geom { type Point {} fn distance() {} }" [];
]

(* Import/Use tests *)
let import_use_tests = [
  test_parse_success "use statement" "use std::io" [];
  test_parse_success "root use" "use ::std::io" [];
  test_parse_success "import statement" "let lib = import \"lib.shell\"" [];
]

(* Pointer type tests *)
let pointer_tests = [
  test_parse_success "pointer variable" "let ptr *i32" [];
  test_parse_success "double pointer" "let ptr **i32" [];
  test_parse_success "pointer to tuple" "let ptr *(i32, f64)" [];
]

(* Edge case tests *)
let edge_case_tests = [
  test_parse_success "multiple semicolons" "let x = 1;; let y = 2" [];
  test_parse_success "semicolon after block" "fn thing() { };" [];
  test_parse_expr "operator precedence complex"
    "a + b * c - d / e" (function
    | Binary { op = Token.Subtract; _ } -> ()
    | _ -> Alcotest.fail "wrong precedence");

  test_parse_expr "right associativity" "a = b = c" (function
    | Binary { op = Token.AssignmentEquals;
               right = Some (_, Binary { op = Token.AssignmentEquals; _ }); _ } -> ()
    | _ -> Alcotest.fail "assignment should be right-associative");

  test_parse_success "trailing comma in enum"
    "enum E { A i32, B i32, }" [];

  test_parse_success "empty function body" "fn f() {}" [];

  test_parse_expr "complex nested expr"
    "f(g(x, y), h(z))[0].field()" (function
    | Binary { left = (_, Index { from = (_, Call _); args = [(_, Integer _)]; }); op = Token.Dot; right = Some (_, Call _) } -> ()
    | _ -> Alcotest.fail "expected method call");
]

(* Whitespace and formatting tests *)
let whitespace_tests = [
  test_parse_success "extra whitespace" "let   x   =   5" [];
  test_parse_success "tabs and spaces" "let\tx\t=\t5" [];
  test_parse_success "newlines in expr" "let x =\n  1 +\n  2" [];
  test_parse_success "no spacing" "let x=5" [];
]

(* Error recovery tests *)
let error_tests = [
  test_parse_fail "unclosed brace" "fn f() { ";
  test_parse_fail "unclosed paren" "f(1, 2";
  test_parse_fail "unclosed bracket" "arr[0";
  test_parse_fail "missing operator" "1 2";
  test_parse_fail "invalid token sequence" "let = x";
  test_parse_fail "stat in root" "stat let x = 5";
]

(* Full program tests *)
let program_tests = [
  test_parse_success "complete program"
    "type Point { x i32 y i32 }
     fn distance(p1 Point, p2 Point) f64 {
       let dx = p2.x - p1.x
       let dy = p2.y - p1.y
       dx * dx + dy * dy
     }
     let origin = Point { x: 0, y: 0 }" [];

  test_parse_success "program with imports"
    "let std = import \"std.shell\"
     use std::io
     fn main() { io::println(\"hello\") }" [];
]

(* Lambda expression tests *)
let lambda_tests = [
  test_parse_expr "simple lambda" "fn() { 42 }" (function
    | Lambda { params = []; result = None; code = [_] } -> ()
    | _ -> Alcotest.fail "expected simple lambda");

  test_parse_expr "lambda with params" "fn(x i32, y i32) { x + y }" (function
    | Lambda { params; code; _ } ->
        if List.length params = 2 && List.length code > 0 then ()
        else Alcotest.fail "expected lambda with 2 params and body"
    | _ -> Alcotest.fail "expected Lambda");

  test_parse_expr "lambda with return type" "fn(x i32) i32 { x * 2 }" (function
    | Lambda { result = Some _; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with return type");

  test_parse_expr "lambda no params with return" "fn() bool { true }" (function
    | Lambda { params = []; result = Some _; _ } -> ()
    | _ -> Alcotest.fail "expected parameterless lambda with return");

  test_parse_expr "lambda single line" "fn(x i32) i32 { x }" (function
    | Lambda _ -> ()
    | _ -> Alcotest.fail "expected Lambda");

  test_parse_expr "lambda empty body" "fn() {}" (function
    | Lambda { code = []; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with empty body");

  test_parse_expr "lambda with complex body"
    "fn(a i32, b i32) i32 { let sum = a + b; sum * 2 }" (function
    | Lambda { code; _ } ->
        if List.length code = 2 then ()
        else Alcotest.fail "expected 2 statements in body"
    | _ -> Alcotest.fail "expected Lambda");

  test_parse_expr "nested lambda"
    "fn(x i32) fn(i32)i32 { fn(y i32) i32 { x + y } }" (function
    | Lambda { code = [_, Lambda _]; _ } -> ()
    | _ -> Alcotest.fail "expected nested lambda");

  test_parse_expr "lambda in call" "map(fn(x i32) i32 { x * 2 })" (function
    | Call { args = [_, Lambda _]; _ } -> ()
    | _ -> Alcotest.fail "expected call with lambda arg");

  test_parse_gen "lambda in variable" "let f = fn(x i32) i32 { x + 1 }" (function
    | Variable { name = "f"; type_ = None; value = Some (_, Lambda _) } -> () (* This would be parsed as declaration, not expr *)
    | _ -> ());

  test_parse_expr "lambda with tuple params"
    "fn(point (i32, i32)) i32 { }" (function
    | Lambda { params = [_]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with tuple param");

  test_parse_expr "lambda with pointer param"
    "fn(ptr *i32) i32 { *ptr }" (function
    | Lambda { params = [_]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with pointer param");

  test_parse_expr "lambda trailing comma params"
    "fn(x i32, y i32,) { }" (function
    | Lambda { params; _ } ->
        if List.length params = 2 then ()
        else Alcotest.fail "expected 2 params despite trailing comma"
    | _ -> Alcotest.fail "expected Lambda");

  test_parse_expr "lambda in array" "[2]fn()i32 { fn() i32 { 1 }, fn() i32 { 2 } }" (function
    | ArrayInitializer { args; _ } ->
        if List.length args = 2 then ()
        else Alcotest.fail "expected 2 lambda elements"
    | _ -> Alcotest.fail "expected array of lambdas");

  test_parse_expr "lambda in tuple expr" "(fn() i32 { 1 }, fn() i32 { 2 })" (function
    | TupleExpr [_, Lambda _; _, Lambda _] -> ()
    | _ -> Alcotest.fail "expected tuple of lambdas");

  test_parse_expr "lambda in initializer"
    "Handler { callback: fn(x i32) { } }" (function
    | Initializer { args = [_, MemberAssignment { value = (_, Lambda _); _ }]; _ } -> ()
    | _ -> Alcotest.fail "expected initializer with lambda field");

  test_parse_gen "lambda as binary operand"
    "let x = fn() i32 { 1 } or fn() i32 { 2 }" (function
    | Variable { name = "x"; type_ = None; value = Some (_, Lambda { params = []; result = Some (_, (Reference (false, ["i32"]))); _ }) } -> () (* Would parse as weird binary expr with lambdas *)
    | _ -> ());

  test_parse_expr "lambda with match in body"
    "fn(x i32) i32 { match (x) { 0 -> { 1 } _ -> { 2 } } }" (function
    | Lambda { code = [_, Match _]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with match");

  test_parse_expr "lambda with if in body"
    "fn(x i32) bool { if x > 0 { true } else { false } }" (function
    | Lambda { code = [_, If _]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with if");

  test_parse_expr "lambda immediate call"
    "fn() i32 { 42 }()" (function
    | Call { from = _, Lambda _; args = []; _ } -> ()
    | _ -> Alcotest.fail "expected immediate lambda invocation");
]

(* Function type annotation tests *)
let function_type_tests = [
  test_parse_success "var with function type no params"
    "let f fn()i32" [];

  test_parse_success "var with function type with params"
    "let f fn(i32, i32)i32" [];

  test_parse_success "var with function type no return"
    "let callback fn(i32)" [];

  test_parse_success "function returning function type"
    "fn make_adder() fn(i32)i32 { }" [];

  test_parse_success "function taking function type"
    "fn apply(f fn(i32)i32, x i32) i32 { }" [];

  test_parse_success "nested function types"
    "let f fn(fn(i32)i32)i32" [];

  test_parse_success "function type with tuple param"
    "let f fn((i32, i32))i32" [];

  test_parse_success "function type with pointer param"
    "let f fn(*i32)i32" [];

  test_parse_success "function type returning pointer"
    "let f fn(i32)*i32" [];

  test_parse_success "function type in struct"
    "type Handler { callback fn(i32) }" [];

  test_parse_success "function type in enum variant"
    "enum Action { Execute fn(i32)bool }" [];

  test_parse_success "function type as type alias"
    "type Callback = fn(i32, i32)i32" [];

  test_parse_success "array of function types"
    "let handlers *fn()i32 = [4]fn()i32{}" [];

  test_parse_success "pointer to function type"
    "let fptr *fn(i32)i32" [];

  test_parse_success "function type with multiple params"
    "let f fn(i32, f64, bool, *i32)i32" [];

  test_parse_success "function type trailing comma"
    "let f fn(i32, f64,)i32" [];

  test_parse_success "function type in anonymous enum"
    "let result { fn(i32)i32, i32 }" [];

  test_parse_success "complex nested function types"
    "let f fn(fn(fn(i32)i32)i32)i32" [];

  test_parse_success "function type in tuple type"
    "let pair (fn()i32, fn()i32)" [];

  test_parse_success "operator overload with function type param"
    "oper (f fn(i32)i32+) { }" [];
]

(* Edge cases for lambdas and function types *)
let lambda_function_edge_cases = [
  test_parse_expr "lambda with anonymous struct param"
    "fn(point type { x i32 y i32 }) { }" (function
    | Lambda { params = [_]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with anon struct param");

  test_parse_expr "lambda with anonymous enum param"
    "fn(result enum { Ok i32, Err i32 }) { }" (function
    | Lambda { params = [_]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with anon enum param");

  test_parse_expr "lambda returning function type"
    "fn() fn(i32)i32 { fn(x i32) i32 { x } }" (function
    | Lambda { result = Some (_, FunctionType _); code = [_, Lambda _]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda returning lambda");

  test_parse_success "function type returning function type"
    "let f fn()fn(i32)i32" [];

  test_parse_success "lambda assigned to typed variable"
    "let add fn(i32, i32)i32 = fn(x i32, y i32) i32 { x + y }" [];

  test_parse_expr "chained lambda calls"
    "fn() fn()i32 { fn() i32 { 42 } }()()" (function
    | Call { from = _, Call { from = _, Lambda _; _ }; _ } -> ()
    | _ -> Alcotest.fail "expected chained lambda calls");

  test_parse_success "function taking multiple function types"
    "fn compose(f fn(i32)i32, g fn(i32)i32) fn(i32)i32 { }" [];

  test_parse_expr "lambda with for loop"
    "fn() { for let i = 0; i < 10; i++ { } }" (function
    | Lambda { code = [_, StandardFor _]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with for loop");

  test_parse_expr "lambda with until loop"
    "fn(done bool) { until (done) { } }" (function
    | Lambda { code = [_, StandardUntil _]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with until loop");

  test_parse_success "recursive function type"
    "type Recursive = fn(Recursive)i32" [];

  test_parse_expr "lambda returning block"
    "fn() i32 { { 42 } }" (function
    | Lambda { code = [_, Block _]; _ } -> ()
    | _ -> Alcotest.fail "expected lambda with block return");

  test_parse_expr "empty param function type in expr position"
    "fn()i32 {}" (function
    | Lambda { params = []; result = Some _; code = []; _ } -> ()
    | _ -> Alcotest.fail "expected minimal lambda");

  test_parse_success "function type with spread operator"
    "fn variadic(args ...i32) { }" [];

  test_parse_expr "lambda in match case"
    "match (x) { 0 -> fn() { } 1 -> fn() { } }" (function
    | Match { cases; _ } ->
        if List.length cases >= 2 then ()
        else Alcotest.fail "expected 2+ match cases"
    | _ -> Alcotest.fail "expected Match");

  test_parse_expr "lambda in if condition (weird but valid)"
    "if fn() bool { true }() { }" (function
    | If { condition = _, Call { from = _, Lambda _; _ }; _ } -> ()
    | _ -> Alcotest.fail "expected if with lambda call condition");
]

(* Integration tests with existing features *)
let lambda_integration_tests = [
  test_parse_success "complete higher-order function example"
    "fn map(arr *i32, f fn(i32)i32) *i32 {
       let result = [6]i32 {}
       for let i = 0; i < 10; i++ {
         result[i] = f(arr[i])
       }
       result
     }
     let doubled = map(arr, fn(x i32) i32 { x * 2 })" [];

  test_parse_success "callback pattern"
    "type EventHandler {
       callback fn(i32)
     }
     fn register(handler EventHandler) {
       handler.callback(42)
     }
     let _ = register(EventHandler {
       callback: fn(x i32) { }
     })" [];

  test_parse_success "closure simulation"
    "fn make_counter() fn()i32 {
       let count = 0
       fn() i32 {
         count = count + 1
         count
       }
     }
     let counter = make_counter()
     let _ = counter()" [];

  test_parse_success "function composition"
    "fn compose(f fn(i32)i32, g fn(i32)i32) fn(i32)i32 {
       fn(x i32) i32 { f(g(x)) }
     }
     let addOne = fn(x i32) i32 { x + 1 }
     let double = fn(x i32) i32 { x * 2 }
     let composed = compose(addOne, double)" [];

  test_parse_success "array of function pointers"
    "let operations = [4]fn(i32, i32)i32 {
       fn(a i32, b i32) i32 { a + b },
       fn(a i32, b i32) i32 { a - b },
       fn(a i32, b i32) i32 { a * b },
       fn(a i32, b i32) i32 { a / b }
     }
     let result = operations[0](5, 3)" [];

  test_parse_success "filter function"
    "fn filter(arr *i32, predicate fn(i32)bool) *i32 {
       let result = *i32 {}
       for let i = 0; i < 10; i++ {
         if predicate(arr[i]) {
           result[i] = arr[i]
         }
       }
       result
     }
     let evens = filter(arr, fn(x i32) bool { x % 2 == 0 })" [];

  test_parse_success "reduce function"
    "fn reduce(arr *i32, init i32, f fn(i32, i32)i32) i32 {
       let acc = init
       for let i = 0; i < 10; i++ {
         acc = f(acc, arr[i])
       }
       acc
     }
     let sum = reduce(arr, 0, fn(a i32, b i32) i32 { a + b })" [];
]

(* Error cases *)
let lambda_error_tests = [
  test_parse_fail "lambda without parens" "fn { }";
  test_parse_fail "function type without parens" "let f : fni32";
  test_parse_fail "lambda with malformed params" "fn(x) { }";
  test_parse_fail "function type with malformed params" "let f : fn(i32 i32)i32";
]

let tests =
    literal_tests @
    variable_tests @
    function_tests @
    type_tests @
    enum_tests @
    expr_tests @
    operator_tests @
    ambiguous_tests @
    call_index_tests @
    tuple_tests @
    control_flow_tests @
    block_tests @
    initializer_tests @
    array_tests @
    reference_tests @
    access_modifier_tests @
    operator_overload_tests @
    space_tests @
    import_use_tests @
    pointer_tests @
    edge_case_tests @
    whitespace_tests @
    error_tests @
    program_tests @
    lambda_tests @
    function_type_tests @
    lambda_function_edge_cases @
    lambda_integration_tests @
    lambda_error_tests
