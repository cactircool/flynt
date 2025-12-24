open Alcotest
open Flynt

(* Helpers *)
let int_vector_eq =
	testable
		(Fmt.(using Array.to_list (list int)))
		(=)

let test_init () =
	let v = Vector.init () in
	check int "size should be 0" 0 (Vector.size v);
	check int "capacity should be 0" 0 (Vector.capacity v)

let test_known () =
	let v = Vector.from_array [|1;2;3|] in
	check int "size should be 3" 3 (Vector.size v);
	check int "capacity should be 3" 3 (Vector.capacity v);
	check int "get 0" 1 (Vector.get 0 v);
	check int "get 1" 2 (Vector.get 1 v);
	check int "get 2" 3 (Vector.get 2 v)

let test_capacity_constructor () =
	let v = Vector.from_capacity 5 42 in
	check int "initial size" 0 (Vector.size v);
	check int "capacity 5" 5 (Vector.capacity v);
	(* pushing values fills up the array *)
	Vector.push 1 v;
	Vector.push 2 v;
	check int "size after push" 2 (Vector.size v)

let test_push_resize () =
	let v = Vector.from_capacity 1 0 in
	Vector.push 10 v;	(* capacity 1, size 1 *)
	Vector.push 20 v;	(* triggers resize to capacity 2 *)
	check int "size after resize" 2 (Vector.size v);
	check int "capacity doubled to 2" 2 (Vector.capacity v);
	Vector.push 30 v;	(* triggers resize to capacity 4 *)
	check int "size after 3 pushes" 3 (Vector.size v);
	check int "capacity doubled to 4" 4 (Vector.capacity v);
	check int "first element" 10 (Vector.get 0 v);
	check int "last element" 30 (Vector.back v)

let test_get_set () =
	let v = Vector.from_array [|5;10;15|] in
	check int "initial get" 10 (Vector.get 1 v);
	Vector.set 99 1 v;
	check int "after set" 99 (Vector.get 1 v)

let test_transform_idx () =
	let v = Vector.from_array [|1;2;3|] in
	Vector.transform_idx (fun x -> x * 10) 1 v;
	check int "transform index 1" 20 (Vector.get 1 v)

let test_pop () =
	let v = Vector.from_array [|4;5;6|] in
	let x = Vector.pop v in
	check int "popped value" 6 x;
	check int "new size" 2 (Vector.size v);
	check int "new last" 5 (Vector.back v)

let test_front_back () =
	let v = Vector.from_array [|9;8;7|] in
	check int "front" 9 (Vector.front v);
	check int "back" 7 (Vector.back v)

let test_to_list () =
	let v = Vector.from_array [|1;2;3|] in
	let l = Vector.to_list v in
	check (list int) "to_list" [1;2;3] l

let test_clear () =
	let v = Vector.from_array [|10;20;30|] in
	Vector.clear v;
	check int "size after clear" 0 (Vector.size v)

(* Full test suite list *)
let tests = [
	test_case "init" `Quick test_init;
	test_case "known" `Quick test_known;
	test_case "capacity constructor" `Quick test_capacity_constructor;
	test_case "push + resize" `Quick test_push_resize;
	test_case "get/set" `Quick test_get_set;
	test_case "transform_idx" `Quick test_transform_idx;
	test_case "pop" `Quick test_pop;
	test_case "front/back" `Quick test_front_back;
	test_case "to_list" `Quick test_to_list;
	test_case "clear" `Quick test_clear;
]
