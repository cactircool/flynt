open Flynt

(* Create a helper that builds a reader from a string using Eio.Flow.string_source *)
let with_reader_from_string str f =
	Eio_main.run @@ fun _ ->
		let flow = Eio.Flow.string_source str in
		let r = Reader.init flow 4096 in
		f r

(* -------------------------- *)
(* Tests                      *)
(* -------------------------- *)

let test_init () =
	with_reader_from_string "" (fun r ->
		Alcotest.(check int) "initial pos = 0" 0 (Reader.pos r);
		Alcotest.(check int) "initial line = 0" 0 (Reader.line r);
		Alcotest.(check int) "initial col = 0" 0 (Reader.col r)
	)

let test_peek_and_get () =
	with_reader_from_string "abc" (fun r ->
		Alcotest.(check (option char)) "peek sees 'a'" (Some 'a') (Reader.peek r);

		let c = Reader.get r in
		Alcotest.(check char) "get returns 'a'" 'a' c;

		Alcotest.(check int) "pos updated" 1 (Reader.pos r);
		Alcotest.(check int) "line unchanged" 0 (Reader.line r);
		Alcotest.(check int) "col updated" 1 (Reader.col r);

		Alcotest.(check (option char)) "peek sees 'b'" (Some 'b') (Reader.peek r)
    )

let test_skip_regular_char () =
	with_reader_from_string "xyz" (fun r ->
		Reader.skip r;
		Alcotest.(check int) "pos = 1" 1 (Reader.pos r);
		Alcotest.(check int) "line = 0" 0 (Reader.line r);
		Alcotest.(check int) "col = 1" 1 (Reader.col r);

		Reader.skip r;
		Alcotest.(check int) "pos = 2" 2 (Reader.pos r);
		Alcotest.(check int) "col = 2" 2 (Reader.col r)
    )

let test_skip_newline () =
	with_reader_from_string "a\nb" (fun r ->
		Reader.skip r; (* skip 'a' *)
		Alcotest.(check int) "line still 0" 0 (Reader.line r);
		Alcotest.(check int) "col = 1" 1 (Reader.col r);

		Reader.skip r; (* skip '\n' *)
		Alcotest.(check int) "line increments" 1 (Reader.line r);
		Alcotest.(check int) "col reset" 0 (Reader.col r)
	)

let test_skip_whitespace () =
	with_reader_from_string "   \n  abc" (fun r ->
		Reader.skip_whitespace r;
		Alcotest.(check int) "line = 1 after skipping newline" 1 (Reader.line r);
		Alcotest.(check int) "col = 2" 2 (Reader.col r);
		Alcotest.(check (option char)) "first non-space is 'a'" (Some 'a') (Reader.peek r)
	)

let test_eof_peek_get () =
	with_reader_from_string "" (fun r ->
		Alcotest.(check (option char)) "peek at eof = None" None (Reader.peek r);

		(* get raises an exception at EOF *)
		Alcotest.check_raises "get at eof -> End_of_file"
			End_of_file
			(fun () -> ignore (Reader.get r))
	)

let test_mixed_sequence () =
	with_reader_from_string "a \n  bc" (fun r ->
		(* 'a' *)
		Alcotest.(check char) "get 'a'" 'a' (Reader.get r);
		Alcotest.(check int) "pos=1" 1 (Reader.pos r);
		Alcotest.(check int) "line=0" 0 (Reader.line r);
		Alcotest.(check int) "col=1" 1 (Reader.col r);

		(* skip whitespace ' ' *)
		Reader.skip_whitespace r;
		Alcotest.(check int) "pos=5" 5 (Reader.pos r);
		Alcotest.(check int) "line=1" 1 (Reader.line r);
		Alcotest.(check int) "col=2" 2 (Reader.col r);

		(* skip whitespace "  " *)
		Reader.skip_whitespace r;
		Alcotest.(check int) "pos=5" 5 (Reader.pos r);
		Alcotest.(check int) "line=1" 1 (Reader.line r);
		Alcotest.(check int) "col=2" 2 (Reader.col r);

		Alcotest.(check (option char)) "peek 'b'" (Some 'b') (Reader.peek r)
    )

(* ----------------------------------------- *)
(* List of test cases, exactly as requested. *)
(* ----------------------------------------- *)

let tests = [
	Alcotest.test_case "init" `Quick test_init;
	Alcotest.test_case "peek and get" `Quick test_peek_and_get;
	Alcotest.test_case "skip regular char" `Quick test_skip_regular_char;
	Alcotest.test_case "skip newline" `Quick test_skip_newline;
	Alcotest.test_case "skip whitespace" `Quick test_skip_whitespace;
	Alcotest.test_case "eof behavior" `Quick test_eof_peek_get;
	Alcotest.test_case "mixed sequence" `Quick test_mixed_sequence;
]
