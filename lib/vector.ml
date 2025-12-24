type 'a t = {
	mutable data : 'a array;
	mutable size : int;
} [@@deriving show]

let init () : 'a t = {
	data = [||];
	size = 0;
}

let from_array (arr : 'a array) : 'a t = {
	data = Array.map (fun x -> x) arr;
	size = Array.length arr;
}

let from_capacity (size : int) (default : 'a) : 'a t = {
	data = Array.make size default;
	size = 0;
}

let from_size (size : int) (default : 'a) : 'a t = {
	data = Array.make size default;
	size = size;
}

let data (vec : 'a t) : 'a array =
	vec.data

let size (vec : 'a t) : int =
	vec.size

let capacity (vec : 'a t) : int =
	Array.length vec.data

let get (i : int) (vec : 'a t) : 'a =
	if i < 0 || i >= vec.size then raise (Failure ("index " ^ (string_of_int i) ^ " out of bounds."))
	else vec.data.(i)

let set (value : 'a) (i : int) (vec : 'a t) : unit =
	(vec.data.(i) <- value; ())

let transform_idx (transformer : 'a -> 'a) (i : int) (vec : 'a t) : unit =
	(vec.data.(i) <- (transformer vec.data.(i)); ())

let free_slots (vec : 'a t) : int =
	(capacity vec) - vec.size

let resize (size : int) (default : 'a) (vec : 'a t) : unit  =
	let new_arr = Array.make size default in
	Array.blit vec.data 0 new_arr 0 (min vec.size size);
	vec.data <- new_arr;
	vec.size <- size

let overwrite (size : int) (default : 'a) (vec : 'a t) : unit =
	vec.data <- Array.make size default;
	vec.size <- size

let reserve (size : int) (default : 'a) (vec : 'a t) : unit =
	let needed = size - vec.size in
	if needed > 0 then (
		let new_arr = Array.make size default in
		Array.blit vec.data 0 new_arr 0 (min vec.size size);
		vec.data <- new_arr;
	)

let push (value : 'a) (vec : 'a t) : unit =
	let size = (size vec) in
	if capacity vec = size then (
		reserve (if size = 0 then 1 else 2 * size) value vec;
		(* vec.data.(vec.size) <- value; *) (* Redundant since value is used to fill the array anyway *)
		vec.size <- vec.size + 1
	) else (
		vec.data.(vec.size) <- value;
		vec.size <- vec.size + 1
	)

let pop (vec : 'a t) : 'a =
	let idx = vec.size - 1 in
	let value = vec.data.(idx) in
	vec.size <- idx;
	value

let front (vec : 'a t) : 'a =
	vec.data.(0)

let set_front (value : 'a) (vec : 'a t) : unit =
	(vec.data.(0) <- value; ())

let back (vec : 'a t) : 'a =
	vec.data.(vec.size - 1)

let set_back (value : 'a) (vec : 'a t) : unit =
	(vec.data.(vec.size - 1) <- value; ())

let to_list (vec : 'a t) : 'a list =
	let rec to_list i arr acc =
		if i < 0 then acc else to_list (i - 1) arr (arr.(i)::acc)
	in to_list (vec.size - 1) vec.data []

let clear (vec : 'a t) : unit =
	(vec.size <- 0; ())

let shrink (vec : 'a t) : unit =
	(vec.data <- Array.sub vec.data 0 vec.size; ())

let stringify (f : 'a -> string) (vec : 'a t) : string =
	let rec stringify (i : int) (acc : string) : string =
		if i = 0 then (
			stringify (i + 1) (acc ^ (f vec.data.(0)))
		) else if i < vec.size then (
			stringify (i + 1) (acc ^ "; " ^ (f vec.data.(i)))
		) else (
			acc
		)
	in
	"[" ^ (stringify 0 "") ^ "]"
