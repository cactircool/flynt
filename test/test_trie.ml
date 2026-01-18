open Flynt
open Trie

let test_init () =
  let trie = init () in
  Alcotest.(check bool) "root has no termination" true (trie.root.terminate = None);
  Alcotest.(check int) "root has no children" 0 (List.length trie.root.next)

let test_insert_single () =
  let trie = init () in
  insert "test" 42 trie;
  match find_exact "test" trie with
  | Some v -> Alcotest.(check int) "finds inserted value" 42 v
  | None -> Alcotest.fail "should find 'test'"

let test_insert_multiple () =
  let trie = init () in
  insert "hello" 1 trie;
  insert "world" 2 trie;
  insert "hi" 3 trie;

  Alcotest.(check (option int)) "finds 'hello'" (Some 1) (find_exact "hello" trie);
  Alcotest.(check (option int)) "finds 'world'" (Some 2) (find_exact "world" trie);
  Alcotest.(check (option int)) "finds 'hi'" (Some 3) (find_exact "hi" trie)

let test_insert_overlapping () =
  let trie = init () in
  insert "test" 1 trie;
  insert "testing" 2 trie;
  insert "te" 3 trie;

  Alcotest.(check (option int)) "finds 'te'" (Some 3) (find_exact "te" trie);
  Alcotest.(check (option int)) "finds 'test'" (Some 1) (find_exact "test" trie);
  Alcotest.(check (option int)) "finds 'testing'" (Some 2) (find_exact "testing" trie)

let test_insert_overwrite () =
  let trie = init () in
  insert "key" 1 trie;
  insert "key" 2 trie;

  Alcotest.(check (option int)) "overwrites value" (Some 2) (find_exact "key" trie)

let test_find_exact_missing () =
  let trie = init () in
  insert "hello" 1 trie;

  Alcotest.(check (option int)) "doesn't find 'world'" None (find_exact "world" trie);
  Alcotest.(check (option int)) "doesn't find prefix 'hel'" None (find_exact "hel" trie);
  Alcotest.(check (option int)) "doesn't find extension 'helloworld'" None (find_exact "helloworld" trie)

let test_find_exact_empty () =
  let trie = init () in
  insert "" 42 trie;

  Alcotest.(check (option int)) "finds empty string" (Some 42) (find_exact "" trie)

let test_find_greedy_exact_match () =
  let trie = init () in
  insert "test" 1 trie;

  let chars = ref ['t'; 'e'; 's'; 't'] in
  let peek_char () = match !chars with | [] -> None | h::_ -> Some h in
  let skip_char () = match !chars with | [] -> () | _::t -> chars := t in

  Alcotest.(check (option int)) "greedy finds 'test'" (Some 1) (find_greedy peek_char skip_char trie);
  Alcotest.(check int) "consumed all chars" 0 (List.length !chars)

let test_find_greedy_longest_match () =
  let trie = init () in
  insert "te" 1 trie;
  insert "test" 2 trie;
  insert "testing" 3 trie;

  let chars = ref ['t'; 'e'; 's'; 't'; 'i'; 'n'; 'g'; 'x'] in
  let peek_char () = match !chars with | [] -> None | h::_ -> Some h in
  let skip_char () = match !chars with | [] -> () | _::t -> chars := t in

  Alcotest.(check (option int)) "greedy finds longest 'testing'" (Some 3) (find_greedy peek_char skip_char trie);
  Alcotest.(check (list char)) "leaves 'x' unconsumed" ['x'] !chars

let test_find_greedy_partial_match () =
  let trie = init () in
  insert "test" 1 trie;

  let chars = ref ['t'; 'e'; 's'; 'x'] in
  let peek_char () = match !chars with | [] -> None | h::_ -> Some h in
  let skip_char () = match !chars with | [] -> () | _::t -> chars := t in

  Alcotest.(check (option int)) "no match for partial" None (find_greedy peek_char skip_char trie);
  Alcotest.(check (list char)) "leaves 'x' unconsumed" ['x'] !chars

let test_find_greedy_no_match () =
  let trie = init () in
  insert "hello" 1 trie;

  let chars = ref ['w'; 'o'; 'r'; 'l'; 'd'] in
  let peek_char () = match !chars with | [] -> None | h::_ -> Some h in
  let skip_char () = match !chars with | [] -> () | _::t -> chars := t in

  Alcotest.(check (option int)) "no match" None (find_greedy peek_char skip_char trie);
  Alcotest.(check (list char)) "no chars consumed" ['w'; 'o'; 'r'; 'l'; 'd'] !chars

let test_find_greedy_empty_input () =
  let trie = init () in
  insert "test" 1 trie;

  (* let chars = ref [] in *)
  let peek_char () = None in
  let skip_char () = () in

  Alcotest.(check (option int)) "no match on empty" None (find_greedy peek_char skip_char trie)

let test_special_chars () =
  let trie = init () in
  insert "a+b" 1 trie;
  insert "x*y" 2 trie;
  insert "m::n" 3 trie;

  Alcotest.(check (option int)) "finds 'a+b'" (Some 1) (find_exact "a+b" trie);
  Alcotest.(check (option int)) "finds 'x*y'" (Some 2) (find_exact "x*y" trie);
  Alcotest.(check (option int)) "finds 'm::n'" (Some 3) (find_exact "m::n" trie)

let test_unicode () =
  let trie = init () in
  insert "café" 1 trie;
  insert "日本" 2 trie;

  Alcotest.(check (option int)) "finds 'café'" (Some 1) (find_exact "café" trie);
  Alcotest.(check (option int)) "finds '日本'" (Some 2) (find_exact "日本" trie)

let tests =
  let open Alcotest in
	[test_case "creates empty trie" `Quick test_init;
	test_case "single insertion" `Quick test_insert_single;
	test_case "multiple insertions" `Quick test_insert_multiple;
	test_case "overlapping keys" `Quick test_insert_overlapping;
	test_case "overwrite existing" `Quick test_insert_overwrite;
	test_case "empty string" `Quick test_find_exact_empty;
	test_case "special characters" `Quick test_special_chars;
	test_case "unicode characters" `Quick test_unicode;
	test_case "missing keys" `Quick test_find_exact_missing;
	test_case "empty string key" `Quick test_find_exact_empty;
	test_case "exact match" `Quick test_find_greedy_exact_match;
	test_case "longest match" `Quick test_find_greedy_longest_match;
	test_case "partial match" `Quick test_find_greedy_partial_match;
	test_case "no match" `Quick test_find_greedy_no_match;
	test_case "empty input" `Quick test_find_greedy_empty_input]
