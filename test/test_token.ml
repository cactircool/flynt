open Alcotest
open Flynt

let tokenize reader =
	Reader.skip_whitespace reader;
	Token.tokenize reader

(* --- Helper: Create a Reader from a string --- *)
let reader_of_string s =
	Reader.init (Eio.Flow.string_source s) 4096

(* --- Helper: tokenize entire input to a list --- *)
let rec collect reader =
	match tokenize reader with
	| Ok Token.Eof -> []
	| Ok tok -> tok :: (collect reader)
	| Error e -> failwith ("Unexpected tokenize error: " ^ e)

(* --- TOKENIZE TESTS ------------------------------------------------------- *)

let test_integer () =
	let r = reader_of_string "1234" in
	match tokenize r with
	| Ok (Token.Integer "1234") -> ()
	| Ok tok -> failf "Expected Integer 1234, got %s" (Token.show tok)
	| Error e -> failf "Expected Integer 1234, errored with %s" e

let test_float () =
	let r = reader_of_string "12.34" in
	match tokenize r with
	| Ok (Token.Float "12.34") -> ()
	| Ok tok -> failf "Expected Float 12.34, got %s" (Token.show tok)
	| Error e -> failf "Expected Float 12.34, errored with %s" e

let test_dot_not_float () =
	let r = reader_of_string "." in
	match tokenize r with
	| Ok Token.Dot -> ()
	| Ok tok -> failf "Expected Dot but got %s" (Token.show tok)
	| Error e -> failf "Expected Dot, errored with %s" e

let test_keyword () =
	let r = reader_of_string "let" in
	match tokenize r with
	| Ok Token.Let -> ()
	| Ok tok -> failf "Expected keyword Let, got %s" (Token.show tok)
	| Error e -> failf "Expected keyword Let, errored with %s" e

let test_identifier () =
	let r = reader_of_string "foo123" in
	match tokenize r with
	| Ok (Token.Id "foo123") -> ()
	| Ok tok -> failf "Expected Id foo123, got %s" (Token.show tok)
	| Error e -> failf "Expected Id foo123, errored with %s" e

let test_string_literal () =
	let r = reader_of_string "\"hello\"" in
	match tokenize r with
	| Ok (Token.String "hello") -> ()
	| Ok tok -> failf "Expected String \"hello\", got %s" (Token.show tok)
	| Error e -> failf "Expected String \"hello\", errored with %s" e

let test_char_literal () =
	let r = reader_of_string "'x'" in
	match tokenize r with
	| Ok (Token.Character "x") -> ()
	| Ok tok -> failf "Expected Character 'x', got %s" (Token.show tok)
	| Error e -> failf "Expected Character 'x', errored with %s" e

let test_escape_sequence () =
	let r = reader_of_string "\"a\\\"b\"" in
	match tokenize r with
	| Ok (Token.String "a\\\"b") -> ()
	| Ok tok -> failf "Expected escaped string, got %s" (Token.show tok)
	| Error e -> failf "Expected escaped string, errored with %s" e

let test_symbol () =
	let r = reader_of_string "->" in
	match tokenize r with
	| Ok Token.Arrow -> ()
	| Ok tok -> failf "Expected Arrow, got %s" (Token.show tok)
	| Error e -> failf "Expected Arrow, errored with %s" e

let test_sequence () =
	let r = reader_of_string "let x = 3 % 4" in
	let toks = collect r in
	let expected = [
		Token.Let;
		Token.Id "x";
		Token.AssignmentEquals;
		Token.Integer "3";
		Token.Modulus;
		Token.Integer "4"
	] in
	check (list string)
		"token sequence"
		(List.map Token.show expected)
		(List.map Token.show toks)

(* --- CLASSIFICATION TESTS ------------------------------------------------ *)

let test_literal () =
	check bool "integer literal" true (Token.literal (Token.Integer "1"));
	check bool "float literal" true (Token.literal (Token.Float "1.2"));
	check bool "true literal" true (Token.literal Token.True);
	check bool "false literal" true (Token.literal Token.False);
	check bool "not literal" false (Token.literal Token.Add)

let test_id_pred () =
	check bool "id true" true (Token.id (Token.Id "xyz"));
	check bool "id false" false (Token.id Token.Let)

let test_symbol_pred () =
	check bool "paren" true (Token.symbol Token.OpenParen);
	check bool "not symbol" false (Token.symbol Token.Add)

let test_keyword_pred () =
	check bool "kw true" true (Token.keyword Token.For);
	check bool "kw false" false (Token.keyword (Token.Id "for"))

let test_unknown_pred () =
	check bool "unknown true" true (Token.unknown Token.Unknown);
	check bool "unknown false" false (Token.unknown Token.Add)

(* --- LEFT/RIGHT/BINARY TESTS --------------------------------------------- *)

let test_left_right_binary () =
	check bool "left operator" true (Token.left Token.LeftPlus);
	check bool "left false" false (Token.left Token.RightPlus);

	check bool "right operator" true (Token.right Token.RightPlus);
	check bool "right false" false (Token.right Token.LeftPlus);

	check bool "binary operator" true (Token.binary Token.Add);
	check bool "not binary" false (Token.binary Token.RightPlus)

(* --- PRECEDENCE TESTS ---------------------------------------------------- *)

let test_precedence () =
	check (option int) "prec(Add)=4" (Some 4) (Token.prec Token.Add);
	check (option int) "prec(Multiply)=3" (Some 3) (Token.prec Token.Multiply);
	check (option int) "prec(Or)=13" (Some 13) (Token.prec Token.Or);
	check (option int) "prec(Unknown)=-1" None (Token.prec Token.Unknown)

(* --- LEFTIFY / RIGHTIFY -------------------------------------------------- *)

let test_leftify () =
	check string "leftify RightPlus" "Token.LeftPlus"
		(Token.show (Token.leftify Token.RightPlus));

	check string "leftify Add" "Token.LeftPlus"
		(Token.show (Token.leftify Token.Add))

let test_rightify () =
	check string "rightify LeftPlus" "Token.RightPlus"
		(Token.show (Token.rightify Token.LeftPlus));

	check string "rightify Subtract" "Token.RightMinus"
		(Token.show (Token.rightify Token.Subtract))

(* --- VALUE & TYPE -------------------------------------------------------- *)

let test_value () =
	check string "value(Add)" "+" (Token.value Token.Add);
	check string "value(Id foo)" "foo" (Token.value (Token.Id "foo"));
	check string "value(String)" "hello" (Token.value (Token.String "hello"));
	check string "value(ExclusiveRange)" ".." (Token.value Token.ExclusiveRange)

let test_token_type () =
	check string "token_type(Add)" "Token.Add"
		(Token.show Token.Add);

	check string "token_type(Id)" "(Token.Id \"xyz\")"
		(Token.show (Token.Id "xyz"))

let test_has_right () =
	check bool "has_right(Add)" true
		(Token.has_right Token.Add);

	check bool "has_right(RightPlus)" true
		(Token.has_right Token.RightPlus);

	check bool "has_right(Modulus)" false
		(Token.has_right Token.Modulus)

let test_has_left () =
	check bool "has_left(Add)" true
		(Token.has_left Token.Add);

	check bool "has_left(LeftPlus)" true
		(Token.has_left Token.LeftPlus);

	check bool "has_left(Modulus)" false
		(Token.has_left Token.Modulus)

let test_has_binary () =
	check bool "has_binary(LeftPlus)" true
		(Token.has_binary Token.LeftPlus);

	check bool "has_binary(LeftNot)" false
		(Token.has_binary Token.LeftNot);

	check bool "has_binary(Modulus)" true
		(Token.has_binary Token.Modulus)

(* ------------------------------------------------------------------------- *)

let tests = [
	test_case "integer" `Quick test_integer;
	test_case "float" `Quick test_float;
	test_case "dot" `Quick test_dot_not_float;
	test_case "keyword" `Quick test_keyword;
	test_case "identifier" `Quick test_identifier;
	test_case "string literal" `Quick test_string_literal;
	test_case "char literal" `Quick test_char_literal;
	test_case "escape sequence" `Quick test_escape_sequence;
	test_case "symbol" `Quick test_symbol;
	test_case "sequence" `Quick test_sequence;

	test_case "literal predicate" `Quick test_literal;
	test_case "id predicate" `Quick test_id_pred;
	test_case "symbol predicate" `Quick test_symbol_pred;
	test_case "keyword predicate" `Quick test_keyword_pred;
	test_case "unknown predicate" `Quick test_unknown_pred;

	test_case "left/right/binary" `Quick test_left_right_binary;
	test_case "precedence" `Quick test_precedence;
	test_case "leftify" `Quick test_leftify;
	test_case "rightify" `Quick test_rightify;
	test_case "value" `Quick test_value;
	test_case "token type" `Quick test_token_type;

	test_case "has right" `Quick test_has_right;
	test_case "has left" `Quick test_has_left;
	test_case "has binary" `Quick test_has_binary;
]
