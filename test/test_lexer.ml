open Flynt
open Alcotest

(* Helpers *)
let lexer_of_string s =
	Lexer.init (Eio.Flow.string_source s) 4096

let strip_fat t =
	fun (f : Lexer.fat_token) -> (t f.token)

let transform_fat t =
	fun (f : Lexer.fat_token) -> { f with token = (t f.token) }

let fat_token_pp f (x : Lexer.fat_token) = Format.fprintf f "{ token = %s; pos = %d; line = %d; col = %d }" (Token.show x.token) x.pos x.line x.col

let fat_token_eq (x : Lexer.fat_token) (y : Lexer.fat_token) = x.token = y.token && x.pos = y.pos && x.line = y.line && x.col = y.col

let testable_fat_token = testable fat_token_pp fat_token_eq

let explode_error (fat_opt, msg) =
	failwith ("Unexpected tokenize error: " ^ msg ^ (
		match fat_opt with
		| Some fat -> " @ " ^ (
			let buf = (Buffer.create 16) in
			let formatter = (Format.formatter_of_buffer buf) in
			fat_token_pp formatter fat;
			Format.pp_print_flush formatter ();
			Buffer.contents buf
		)
		| None -> ""
	))

let rec collect l =
	match Lexer.pop l with
	| Ok { token = Token.Eof; _ } -> []
	| Ok fat -> fat :: (collect l)
	| Error e -> explode_error e

(* -------------------------- *)
(* Tests                      *)
(* -------------------------- *)

let test_basic_popping () =
	let toks = collect (lexer_of_string "x x x") in
	let expected = [
		Token.Id "x";
		Token.Id "x";
		Token.Id "x";
	] in
	check (list string)
		"id sequence"
		(List.map Token.show expected)
		(List.map (strip_fat Token.show) toks);

	let toks = collect (lexer_of_string "1.0 10 \"hello\" 'hello' true false") in
	let expected = [
		Token.Float "1.0";
		Token.Integer "10";
		Token.String "hello";
		Token.Character "hello";
		Token.True;
		Token.False;
	] in
	check (list string)
		"literal sequence"
		(List.map Token.show expected)
		(List.map (strip_fat Token.show) toks)


let test_fat_token_production () =
	let toks = collect (lexer_of_string "x y z\nx y z") in
	let expected : Lexer.fat_token list = [
		{ token = Token.Id "x"; pos = 0; line = 0; col = 0; };
		{ token = Token.Id "y"; pos = 2; line = 0; col = 2; };
		{ token = Token.Id "z"; pos = 4; line = 0; col = 4; };

		{ token = Token.Id "x"; pos = 6; line = 1; col = 0; };
		{ token = Token.Id "y"; pos = 8; line = 1; col = 2; };
		{ token = Token.Id "z"; pos = 10; line = 1; col = 4; };
	] in
	check (list testable_fat_token)
		"fat token sequence"
		expected
		toks

let test_peeking () =
	let l = lexer_of_string "x y z" in
	let token = Result.get_ok (Lexer.peek l) in
	check testable_fat_token
		"token = {x; 0; 0; 0}"
		{ token = Token.Id "x"; pos = 0; line = 0; col = 0 }
		token;

	let token = Result.get_ok (Lexer.peek l) in
	check testable_fat_token
		"token = {x; 0; 0; 0}"
		{ token = Token.Id "x"; pos = 0; line = 0; col = 0 }
		token;

	let token = Result.get_ok (Lexer.pop l) in
	check testable_fat_token
		"token = {x; 0; 0; 0}"
		{ token = Token.Id "x"; pos = 0; line = 0; col = 0 }
		token;

	let token = Result.get_ok (Lexer.peek l) in
	check testable_fat_token
		"token = {y; 2; 0; 2}"
		{ token = Token.Id "y"; pos = 2; line = 0; col = 2 }
		token;

	let token = Result.get_ok (Lexer.pop l) in
	check testable_fat_token
		"token = {y; 2; 0; 2}"
		{ token = Token.Id "y"; pos = 2; line = 0; col = 2 }
		token

let test_pushing_and_popping () =
	let l = lexer_of_string "a" in
	for i = 0 to 100 do
		match Lexer.pop l with
		| Ok fat -> (
			Lexer.push fat l;
			check testable_fat_token
				("Iteration " ^ (string_of_int i) ^ (": fat = {a; 0; 0; 0}"))
				{ token = Token.Id "a"; pos = 0; line = 0; col = 0 }
				fat;
		)
		| Error e -> explode_error e
	done;
	match Lexer.pop l with
	| Ok fat -> (
		check testable_fat_token
			"fat = {a; 0; 0; 0}"
			{ token = Token.Id "a"; pos = 0; line = 0; col = 0 }
			fat;
		match Lexer.pop l with
		| Ok fat ->
			check testable_fat_token
				"fat = {EOF; -1; -1; -1}"
				{ token = Token.Eof; pos = -1; line = -1; col = -1 }
				fat
		| Error e -> explode_error e
	)
	| Error e -> explode_error e

let test_resolution () =
	let toks = collect (lexer_of_string "++a + +b+") in
	let expected = [
		Token.LeftIncrement;
		Token.Id "a";
		Token.Add;
		Token.LeftPlus;
		Token.Id "b";
		Token.RightPlus;
	] in
	check (list string)
		"ambiguous sequence 1"
		(List.map Token.show expected)
		(List.map (strip_fat Token.show) toks);

	let toks = collect (lexer_of_string "&x & (&&&x)&") in
	let expected = [
		Token.LeftReference;
		Token.Id "x";
		Token.BitwiseAnd;
		Token.OpenParen;
		Token.LeftReference;
		Token.LeftReference;
		Token.LeftReference;
		Token.Id "x";
		Token.ClosedParen;
		Token.RightReference;
	] in
	check (list string)
		"ambiguous sequence 2"
		(List.map Token.show expected)
		(List.map (strip_fat Token.show) toks);

	let toks = collect (lexer_of_string "= a + b") in
	let expected = [
		Token.AssignmentEquals;
		Token.Id "a";
		Token.Add;
		Token.Id "b";
	] in
	check (list string)
		"sequence 3"
		(List.map Token.show expected)
		(List.map (strip_fat Token.show) toks)

let test_c_hello_world () =
	let toks = collect (lexer_of_string "int main(int argc, char *argv[]) {\nprintf(\"Hello world!\\n\");\nreturn 0;\n}\n") in
	let expected = [
		Token.Id "int";
		Token.Id "main";
		Token.OpenParen;
		Token.Id "int";
		Token.Id "argc";
		Token.Comma;
		Token.Id "char";
		Token.Multiply; (* Not regulated but whatever *)
		Token.Id "argv";
		Token.OpenBracket;
		Token.ClosedBracket;
		Token.ClosedParen;
		Token.OpenBrace;
		Token.Id "printf";
		Token.OpenParen;
		Token.String "Hello world!\\n";
		Token.ClosedParen;
		Token.SemiColon;
		Token.Id "return";
		Token.Integer "0";
		Token.SemiColon;
		Token.ClosedBrace;
	] in
	check (list string)
		"basic c hello world"
		(List.map Token.show expected)
		(List.map (strip_fat Token.show) toks)

let tests = [
	Alcotest.test_case "basic popping" `Quick test_basic_popping;
	Alcotest.test_case "fat token production" `Quick test_fat_token_production;
	Alcotest.test_case "peeking" `Quick test_peeking;
	Alcotest.test_case "pushing and popping" `Quick test_pushing_and_popping;
	Alcotest.test_case "resolution" `Quick test_resolution;
	Alcotest.test_case "c hello world" `Quick test_c_hello_world;
]
