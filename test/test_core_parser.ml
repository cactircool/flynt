(* core_parser_test.ml - Comprehensive test suite for core_parser *)

open Flynt
open Alcotest

(* Helper to create fat_tokens for testing *)
let make_fat_token token pos line col =
  { Lexer.token; pos; line; col }

let eof_token = make_fat_token Token.Eof (-1) (-1) (-1)

(* Helper to create bounds *)
let make_bounds start_tok end_tok = (start_tok, end_tok)

(* Helper to create simple identifier *)
let make_id name =
  (* let tok = make_fat_token (Token.Id name) 0 0 0 in *)
  (false, [name])

(* Helper to create simple reference node *)
let make_ref_node name =
  let tok = make_fat_token (Token.Id name) 0 0 0 in
  let bounds = make_bounds tok tok in
  (bounds, Shell_ast.Reference (make_id name))

(* Helper to create integer literal node *)
let make_int_node value =
  let tok = make_fat_token (Token.Integer value) 0 0 0 in
  let bounds = make_bounds tok tok in
  (bounds, Shell_ast.Integer tok)

(* Helper to create string literal node *)
let make_string_node value =
  let tok = make_fat_token (Token.String value) 0 0 0 in
  let bounds = make_bounds tok tok in
  (bounds, Shell_ast.String tok)

(* Helper to create boolean literal nodes *)
let make_true_node () =
  let tok = make_fat_token Token.True 0 0 0 in
  let bounds = make_bounds tok tok in
  (bounds, Shell_ast.True tok)

let make_false_node () =
  let tok = make_fat_token Token.False 0 0 0 in
  let bounds = make_bounds tok tok in
  (bounds, Shell_ast.False tok)

(* Test: Literal parsing *)
let test_parse_literals () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  (* Test integer literal *)
  let int_node = make_int_node "42" in
  let result = Core_parser.parse_expr file state int_node in
  match result with
  | Core_ast.ELiteral (Core_ast.LInteger "42") -> ()
  | _ -> Alcotest.fail "Integer literal not parsed correctly"

(* Test: Variable declaration parsing *)
let test_parse_variable_with_type_and_value () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let name_tok = make_fat_token (Token.Id "x") 0 0 0 in
  let type_node = make_ref_node "int" in
  let value_node = make_int_node "42" in

  let var_node = (
    make_bounds name_tok name_tok,
    Shell_ast.Variable {
      name = "x";
      type_ = Some type_node;
      value = Some value_node;
    }
  ) in

  let result = Core_parser.parse_decl file state var_node in
  match result with
  | Core_ast.DVariable { name = "x"; type_ref = Some _; value = Some _ } -> ()
  | _ -> Alcotest.fail "Variable with type and value not parsed correctly"

let test_parse_variable_type_only () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let name_tok = make_fat_token (Token.Id "x") 0 0 0 in
  let type_node = make_ref_node "int" in

  let var_node = (
    make_bounds name_tok name_tok,
    Shell_ast.Variable {
      name = "x";
      type_ = Some type_node;
      value = None;
    }
  ) in

  let result = Core_parser.parse_decl file state var_node in
  match result with
  | Core_ast.DVariable { name = "x"; type_ref = Some _; value = None } -> ()
  | _ -> Alcotest.fail "Variable with type only not parsed correctly"

let test_parse_variable_value_only () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let name_tok = make_fat_token (Token.Id "x") 0 0 0 in
  let value_node = make_int_node "42" in

  let var_node = (
    make_bounds name_tok name_tok,
    Shell_ast.Variable {
      name = "x";
      type_ = None;
      value = Some value_node;
    }
  ) in

  let result = Core_parser.parse_decl file state var_node in
  match result with
  | Core_ast.DVariable { name = "x"; type_ref = None; value = Some _ } -> ()
  | _ -> Alcotest.fail "Variable with value only not parsed correctly"

let test_parse_variable_neither_fails () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let name_tok = make_fat_token (Token.Id "x") 0 0 0 in

  let var_node = (
    make_bounds name_tok name_tok,
    Shell_ast.Variable {
      name = "x";
      type_ = None;
      value = None;
    }
  ) in

  try
    let _ = Core_parser.parse_decl file state var_node in
    Alcotest.fail "Should have raised FatalError for variable without type or value"
  with
  | Core_parser.FatalError _ -> ()
  | _ -> Alcotest.fail "Wrong exception raised"

(* Test: Binary operator parsing *)
let test_parse_binary_add () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let left = make_int_node "1" in
  let right = make_int_node "2" in

  let binary_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Binary {
      left = left;
      op = Token.Add;
      right = Some right;
    }
  ) in

  let result = Core_parser.parse_expr file state binary_node in
  match result with
  | Core_ast.EBinary { op = Core_ast.Add; _ } -> ()
  | _ -> Alcotest.fail "Binary addition not parsed correctly"

let test_parse_binary_multiply () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let left = make_ref_node "x" in
  let right = make_int_node "2" in

  let binary_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Binary {
      left = left;
      op = Token.Multiply;
      right = Some right;
    }
  ) in

  let result = Core_parser.parse_expr file state binary_node in
  match result with
  | Core_ast.EBinary { op = Core_ast.Multiply; _ } -> ()
  | _ -> Alcotest.fail "Binary multiplication not parsed correctly"

(* Test: Unary operator parsing *)
let test_parse_unary_minus () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let arg = make_int_node "42" in

  let unary_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Unary {
      op = Token.LeftMinus;
      arg = Some arg;
    }
  ) in

  let result = Core_parser.parse_expr file state unary_node in
  match result with
  | Core_ast.EUnary { op = Core_ast.LeftMinus; _ } -> ()
  | _ -> Alcotest.fail "Unary minus not parsed correctly"

let test_parse_unary_not () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let arg = make_true_node () in

  let unary_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Unary {
      op = Token.LeftNot;
      arg = Some arg;
    }
  ) in

  let result = Core_parser.parse_expr file state unary_node in
  match result with
  | Core_ast.EUnary { op = Core_ast.LeftNot; _ } -> ()
  | _ -> Alcotest.fail "Unary not not parsed correctly"

(* Test: Function parsing *)
let test_parse_simple_function () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let param_tok = make_fat_token (Token.Id "x") 0 0 0 in
  let param = (
    make_bounds param_tok param_tok,
    Shell_ast.Variable {
      name = "x";
      type_ = Some (make_ref_node "int");
      value = None;
    }
  ) in

  let body = make_int_node "42" in

  let func_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Function {
      name = "add_one";
      params = [param];
      result = Some (make_ref_node "int");
      code = [body];
    }
  ) in

  let result = Core_parser.parse_decl file state func_node in
  match result with
  | Core_ast.DFunction { name = "add_one"; params = [_]; return_type = Some _; _ } -> ()
  | _ -> Alcotest.fail "Simple function not parsed correctly"

let test_parse_function_no_params () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let body = make_int_node "42" in

  let func_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Function {
      name = "get_value";
      params = [];
      result = Some (make_ref_node "int");
      code = [body];
    }
  ) in

  let result = Core_parser.parse_decl file state func_node in
  match result with
  | Core_ast.DFunction { name = "get_value"; params = []; _ } -> ()
  | _ -> Alcotest.fail "Function with no params not parsed correctly"

(* Test: If expression parsing *)
let test_parse_if_with_else () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let condition = make_true_node () in
  let true_branch = [make_int_node "1"] in
  let false_branch = [make_int_node "2"] in

  let if_node = (
    make_bounds eof_token eof_token,
    Shell_ast.If {
      condition = condition;
      true_block = true_branch;
      false_block = Some false_branch;
    }
  ) in

  let result = Core_parser.parse_expr file state if_node in
  match result with
  | Core_ast.EIf { condition = _; true_block = [_]; false_block = [_] } -> ()
  | _ -> Alcotest.fail "If with else not parsed correctly"

let test_parse_if_without_else () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let condition = make_true_node () in
  let true_branch = [make_int_node "1"] in

  let if_node = (
    make_bounds eof_token eof_token,
    Shell_ast.If {
      condition = condition;
      true_block = true_branch;
      false_block = None;
    }
  ) in

  let result = Core_parser.parse_expr file state if_node in
  match result with
  | Core_ast.EIf { condition = _; true_block = [_]; false_block = [] } -> ()
  | _ -> Alcotest.fail "If without else not parsed correctly"

(* Test: Lambda parsing *)
let test_parse_lambda () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let param = (
    make_bounds eof_token eof_token,
    Shell_ast.Variable {
      name = "x";
      type_ = Some (make_ref_node "int");
      value = None;
    }
  ) in

  let body = make_ref_node "x" in

  let lambda_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Lambda {
      params = [param];
      result = Some (make_ref_node "int");
      code = [body];
    }
  ) in

  let result = Core_parser.parse_expr file state lambda_node in
  match result with
  | Core_ast.ELambda { params = [_]; return_type = Some _; _ } -> ()
  | _ -> Alcotest.fail "Lambda not parsed correctly"

(* Test: Type declaration parsing *)
let test_parse_type_declaration () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let field = (
    make_bounds eof_token eof_token,
    Shell_ast.Variable {
      name = "x";
      type_ = Some (make_ref_node "int");
      value = None;
    }
  ) in

  let type_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Type {
      name = "Point";
      members = [field];
    }
  ) in

  let result = Core_parser.parse_decl file state type_node in
  match result with
  | Core_ast.DType { name = "Point"; members = [_] } -> ()
  | _ -> Alcotest.fail "Type declaration not parsed correctly"

(* Test: Enum parsing *)
let test_parse_enum () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let variant = (
    make_bounds eof_token eof_token,
    Shell_ast.EnumVariant {
      name = "Red";
      type_ = make_ref_node "unit";
    }
  ) in

  let enum_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Enum {
      name = "Color";
      members = [variant];
    }
  ) in

  let result = Core_parser.parse_decl file state enum_node in
  match result with
  | Core_ast.DEnum { name = "Color"; members = [Core_ast.EMVariant _] } -> ()
  | _ -> Alcotest.fail "Enum not parsed correctly"

(* Test: Match expression parsing *)
let test_parse_match () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let switcher = make_ref_node "x" in

  let pattern = make_int_node "1" in
  let branch_body = make_true_node () in

  let case = (
    make_bounds eof_token eof_token,
    Shell_ast.MatchCase {
      pattern = pattern;
      guard = None;
      logic = Some branch_body;
    }
  ) in

  let match_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Match {
      switcher = switcher;
      cases = [case];
    }
  ) in

  let result = Core_parser.parse_expr file state match_node in
  match result with
  | Core_ast.EMatch { switcher = _; logic = [_] } -> ()
  | _ -> Alcotest.fail "Match not parsed correctly"

(* Test: Loop parsing *)
let test_parse_standard_for () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let init = [make_ref_node "x"] in
  let condition = make_true_node () in
  let iter = [make_ref_node "y"] in
  let body = [make_int_node "1"] in

  let for_node = (
    make_bounds eof_token eof_token,
    Shell_ast.StandardFor {
      init = init;
      condition = condition;
      iter = iter;
      block = body;
    }
  ) in

  let result = Core_parser.parse_stmt file state for_node in
  match result with
  | Core_ast.SFor { init = [_]; condition = _; iter = [_]; code = [_] } -> ()
  | _ -> Alcotest.fail "Standard for loop not parsed correctly"

let test_parse_breaking_for () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let init = [make_ref_node "x"] in
  let iter = [make_ref_node "y"] in
  let body = [make_int_node "1"] in

  let for_node = (
    make_bounds eof_token eof_token,
    Shell_ast.BreakingFor {
      init = init;
      iter = iter;
      block = body;
    }
  ) in

  let result = Core_parser.parse_expr file state for_node in
  match result with
  | Core_ast.EFor { init = [_]; iter = [_]; code = [_] } -> ()
  | _ -> Alcotest.fail "Breaking for loop not parsed correctly"

(* Test: Yielding break parsing *)
let test_parse_yielding_break_in_breaking_loop () =
  let file = "test.lang" in
  let state = Core_ast.push_ctx Core_ast.CBFor Core_ast.default_state in

  let break_value = make_int_node "42" in

  let break_node = (
    make_bounds eof_token eof_token,
    Shell_ast.YieldingBreak break_value
  ) in

  let result = Core_parser.parse_stmt file state break_node in
  match result with
  | Core_ast.SYieldingBreak _ -> ()
  | _ -> Alcotest.fail "Yielding break not parsed correctly"

let test_parse_yielding_break_outside_breaking_loop_fails () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let break_value = make_int_node "42" in

  let break_node = (
    make_bounds eof_token eof_token,
    Shell_ast.YieldingBreak break_value
  ) in

  try
    let _ = Core_parser.parse_stmt file state break_node in
    Alcotest.fail "Should have raised FatalError for yielding break outside breaking loop"
  with
  | Core_parser.FatalError _ -> ()
  | _ -> Alcotest.fail "Wrong exception raised"

(* Test: Call and Index parsing *)
let test_parse_call () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let func = make_ref_node "foo" in
  let arg1 = make_int_node "1" in
  let arg2 = make_int_node "2" in

  let call_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Call {
      from = func;
      args = [arg1; arg2];
    }
  ) in

  let result = Core_parser.parse_expr file state call_node in
  match result with
  | Core_ast.ECall { from = _; args = [_; _] } -> ()
  | _ -> Alcotest.fail "Call not parsed correctly"

let test_parse_index () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let array = make_ref_node "arr" in
  let index = make_int_node "0" in

  let index_node = (
    make_bounds eof_token eof_token,
    Shell_ast.Index {
      from = array;
      args = [index];
    }
  ) in

  let result = Core_parser.parse_expr file state index_node in
  match result with
  | Core_ast.EIndex { from = _; args = [_] } -> ()
  | _ -> Alcotest.fail "Index not parsed correctly"

(* Test: Tuple parsing *)
let test_parse_tuple_expr () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let elem1 = make_int_node "1" in
  let elem2 = make_int_node "2" in
  let elem3 = make_int_node "3" in

  let tuple_node = (
    make_bounds eof_token eof_token,
    Shell_ast.TupleExpr [elem1; elem2; elem3]
  ) in

  let result = Core_parser.parse_expr file state tuple_node in
  match result with
  | Core_ast.ETuple [_; _; _] -> ()
  | _ -> Alcotest.fail "Tuple expression not parsed correctly"

(* Test: Pattern parsing *)
let test_parse_pattern_wildcard () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let pattern = make_ref_node "_" in

  let result = Core_parser.parse_pattern file state pattern None in
  match result with
  | Core_ast.PStandard Core_ast.PWildcard -> ()
  | _ -> Alcotest.fail "Wildcard pattern not parsed correctly"

let test_parse_pattern_bind () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let pattern = make_ref_node "x" in

  let result = Core_parser.parse_pattern file state pattern None in
  match result with
  | Core_ast.PStandard (Core_ast.PBind "x") -> ()
  | _ -> Alcotest.fail "Bind pattern not parsed correctly"

let test_parse_pattern_literal () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let pattern = make_int_node "42" in

  let result = Core_parser.parse_pattern file state pattern None in
  match result with
  | Core_ast.PStandard (Core_ast.PLiteral (Core_ast.LInteger "42")) -> ()
  | _ -> Alcotest.fail "Literal pattern not parsed correctly"

let test_parse_pattern_with_guard () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let pattern = make_ref_node "x" in
  let guard = make_true_node () in

  let result = Core_parser.parse_pattern file state pattern (Some guard) in
  match result with
  | Core_ast.PGuard (Core_ast.PBind "x", _) -> ()
  | _ -> Alcotest.fail "Pattern with guard not parsed correctly"

(* Test: Operator overload parsing *)
let test_parse_unary_operator_overload () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let param = (
    make_bounds eof_token eof_token,
    Shell_ast.Variable {
      name = "self";
      type_ = Some (make_ref_node "T");
      value = None;
    }
  ) in

  let body = make_int_node "1" in

  let oper_node = (
    make_bounds eof_token eof_token,
    Shell_ast.OperatorOverload {
      name = Token.LeftMinus;
      params = [param];
      result = Some (make_ref_node "T");
      code = [body];
    }
  ) in

  let result = Core_parser.parse_decl file state oper_node in
  match result with
  | Core_ast.DOperOverload { op = Core_ast.Unary Core_ast.LeftMinus; params = [_]; _ } -> ()
  | _ -> Alcotest.fail "Unary operator overload not parsed correctly"

let test_parse_binary_operator_overload () =
  let file = "test.lang" in
  let state = Core_ast.default_state in

  let param1 = (
    make_bounds eof_token eof_token,
    Shell_ast.Variable {
      name = "a";
      type_ = Some (make_ref_node "T");
      value = None;
    }
  ) in

  let param2 = (
    make_bounds eof_token eof_token,
    Shell_ast.Variable {
      name = "b";
      type_ = Some (make_ref_node "T");
      value = None;
    }
  ) in

  let body = make_int_node "1" in

  let oper_node = (
    make_bounds eof_token eof_token,
    Shell_ast.OperatorOverload {
      name = Token.Add;
      params = [param1; param2];
      result = Some (make_ref_node "T");
      code = [body];
    }
  ) in

  let result = Core_parser.parse_decl file state oper_node in
  match result with
  | Core_ast.DOperOverload { op = Core_ast.Binary Core_ast.Add; params = [_; _]; _ } -> ()
  | _ -> Alcotest.fail "Binary operator overload not parsed correctly"

(* Test: Context state management *)
let test_context_push_pop () =
  let state = Core_ast.default_state in
  check bool "Initial state has empty stack" true (state.stack = []);

  let state1 = Core_ast.push_ctx Core_ast.CFunction state in
  check bool "After push, stack is non-empty" true (state1.stack <> []);
  check bool "Top of stack is CFunction" true
    (match Core_ast.peek_ctx state1 with Some Core_ast.CFunction -> true | _ -> false);

  let state2 = Core_ast.pop_ctx state1 in
  check bool "After pop, stack is empty again" true (state2.stack = []);
  check bool "State is back to default" true (state2 = state)

let test_context_in_ctx () =
  let state = Core_ast.default_state in
  check bool "Not in CFunction initially" false (Core_ast.in_ctx Core_ast.CFunction state);

  let state1 = Core_ast.push_ctx Core_ast.CFunction state in
  check bool "In CFunction after push" true (Core_ast.in_ctx Core_ast.CFunction state1);
  check bool "Not in CType" false (Core_ast.in_ctx Core_ast.CType state1);

  let state2 = Core_ast.push_ctx Core_ast.CType state1 in
  check bool "In both CFunction and CType" true
    (Core_ast.in_ctx Core_ast.CFunction state2 && Core_ast.in_ctx Core_ast.CType state2)

(* Test suite definitions *)
let literal_tests = [
  "parse integer literal", `Quick, test_parse_literals;
]

let variable_tests = [
  "parse variable with type and value", `Quick, test_parse_variable_with_type_and_value;
  "parse variable with type only", `Quick, test_parse_variable_type_only;
  "parse variable with value only", `Quick, test_parse_variable_value_only;
  "parse variable with neither fails", `Quick, test_parse_variable_neither_fails;
]

let operator_tests = [
  "parse binary addition", `Quick, test_parse_binary_add;
  "parse binary multiplication", `Quick, test_parse_binary_multiply;
  "parse unary minus", `Quick, test_parse_unary_minus;
  "parse unary not", `Quick, test_parse_unary_not;
  "parse unary operator overload", `Quick, test_parse_unary_operator_overload;
  "parse binary operator overload", `Quick, test_parse_binary_operator_overload;
]

let function_tests = [
  "parse simple function", `Quick, test_parse_simple_function;
  "parse function with no params", `Quick, test_parse_function_no_params;
  "parse lambda", `Quick, test_parse_lambda;
]

let control_flow_tests = [
  "parse if with else", `Quick, test_parse_if_with_else;
  "parse if without else", `Quick, test_parse_if_without_else;
  "parse match", `Quick, test_parse_match;
  "parse standard for loop", `Quick, test_parse_standard_for;
  "parse breaking for loop", `Quick, test_parse_breaking_for;
  "parse yielding break in breaking loop", `Quick, test_parse_yielding_break_in_breaking_loop;
  "parse yielding break outside breaking loop fails", `Quick, test_parse_yielding_break_outside_breaking_loop_fails;
]

let type_tests = [
  "parse type declaration", `Quick, test_parse_type_declaration;
  "parse enum", `Quick, test_parse_enum;
]

let expression_tests = [
  "parse call", `Quick, test_parse_call;
  "parse index", `Quick, test_parse_index;
  "parse tuple expression", `Quick, test_parse_tuple_expr;
]

let pattern_tests = [
  "parse wildcard pattern", `Quick, test_parse_pattern_wildcard;
  "parse bind pattern", `Quick, test_parse_pattern_bind;
  "parse literal pattern", `Quick, test_parse_pattern_literal;
  "parse pattern with guard", `Quick, test_parse_pattern_with_guard;
]

let context_tests = [
  "context push and pop", `Quick, test_context_push_pop;
  "context in_ctx check", `Quick, test_context_in_ctx;
]

(* Main test runner *)
let tests =
	literal_tests @
	variable_tests @
	operator_tests @
	function_tests @
	control_flow_tests @
	type_tests @
	expression_tests @
	pattern_tests @
	context_tests
