open Flynt

(* === Helper test functions === *)

let test_empty_trie () =
	let trie = Trie.init () in
	Alcotest.(check (option int)) "empty find_exact"
		None (Trie.find_exact "a" trie)

let test_single_insert_and_find () =
	let trie = Trie.init () in
	Trie.insert "a" 1 trie;
	Alcotest.(check (option int)) "find_exact 'a'"
		(Some 1) (Trie.find_exact "a" trie);
	Alcotest.(check (option int)) "find_exact nonexistent"
		None (Trie.find_exact "b" trie)

let test_multiple_inserts () =
	let trie = Trie.init () in
	Trie.insert "a" 1 trie;
	Trie.insert "ab" 2 trie;
	Trie.insert "abc" 3 trie;
	Trie.insert "abd" 4 trie;

	Alcotest.(check (option int)) "find_exact 'a'"   (Some 1) (Trie.find_exact "a" trie);
	Alcotest.(check (option int)) "find_exact 'ab'"  (Some 2) (Trie.find_exact "ab" trie);
	Alcotest.(check (option int)) "find_exact 'abc'" (Some 3) (Trie.find_exact "abc" trie);
	Alcotest.(check (option int)) "find_exact 'abd'" (Some 4) (Trie.find_exact "abd" trie)

let test_overlapping_prefixes () =
	let trie = Trie.init () in
	Trie.insert "car" 1 trie;
	Trie.insert "cat" 2 trie;
	Trie.insert "cart" 3 trie;

	Alcotest.(check (option int)) "find_exact 'car'"  (Some 1) (Trie.find_exact "car" trie);
	Alcotest.(check (option int)) "find_exact 'cat'"  (Some 2) (Trie.find_exact "cat" trie);
	Alcotest.(check (option int)) "find_exact 'cart'" (Some 3) (Trie.find_exact "cart" trie);
	Alcotest.(check (option int)) "find_exact 'carx'" None (Trie.find_exact "carx" trie)

let test_reinserting_updates_value () =
	let trie = Trie.init () in
	Trie.insert "abc" 1 trie;
	Trie.insert "abc" 99 trie; (* overwrite *)

	Alcotest.(check (option int)) "overwrite existing key"
		(Some 99) (Trie.find_exact "abc" trie)

(* --------- Helper stream for greedy test --------- *)
let make_stream s =
	let idx = ref 0 in
	let peek () = if !idx >= String.length s then None else Some s.[!idx] in
	let skip () = if !idx < String.length s then incr idx; in
	(peek, skip)

let test_find_greedy () =
	let trie = Trie.init () in
	Trie.insert "a" 1 trie;
	Trie.insert "ab" 2 trie;
	Trie.insert "abc" 3 trie;
	Trie.insert "abd" 4 trie;

	let peek, skip = make_stream "abcdzzz" in

	Alcotest.(check (option int)) "greedy matches longest prefix (abc)"
		(Some 3) (Trie.find_greedy peek skip trie)

let test_find_greedy_shortest () =
	let trie = Trie.init () in
	Trie.insert "x" 10 trie;
	Trie.insert "xy" 20 trie;

	let peek, skip = make_stream "xZ" in
	(* The second char doesn't match 'y', so only "x" matches *)
	Alcotest.(check (option int)) "greedy stops on mismatch"
		(Some 10) (Trie.find_greedy peek skip trie)

let test_find_greedy_no_match () =
	let trie = Trie.init () in
	Trie.insert "abc" 1 trie;

	let peek, _ = make_stream "zzz" in
	Alcotest.(check (option int)) "greedy no match"
		None (Trie.find_greedy peek (fun () -> ()) trie)

(* === Test list === *)

let tests = [
	Alcotest.test_case "empty trie" `Quick test_empty_trie;
	Alcotest.test_case "single insert and find" `Quick test_single_insert_and_find;
	Alcotest.test_case "multiple inserts" `Quick test_multiple_inserts;
	Alcotest.test_case "overlapping prefixes" `Quick test_overlapping_prefixes;
	Alcotest.test_case "reinserting updates value" `Quick test_reinserting_updates_value;
	Alcotest.test_case "find greedy" `Quick test_find_greedy;
	Alcotest.test_case "find greedy shortest" `Quick test_find_greedy_shortest;
	Alcotest.test_case "find greedy no match" `Quick test_find_greedy_no_match;
]
