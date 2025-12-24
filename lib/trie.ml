type 'a node = {
	payload : char;
	mutable next : 'a node list;
	mutable terminate : 'a option;
}

type 'a t = {
	root : 'a node;
}

let init () : 'a t =
	{ root = { payload = '\000'; next = []; terminate = None } }

let insert (s : string) (value : 'a) (trie : 'a t) : unit =
	let rec go (i : int) (node : 'a node) : unit =
		if i = String.length s then (
			node.terminate <- Some value;
			()
		) else (
			let c = s.[i] in
			match List.find_opt (fun n -> n.payload = c) node.next with
			| Some child -> go (i + 1) child
			| None -> (
				let child = { payload = c; next = []; terminate = None } in
				node.next <- child :: node.next;
				go (i + 1) child
			)
		)
	in
	go 0 trie.root

let find_exact (s : string) (trie : 'a t) : 'a option =
	let rec go (i : int) (node : 'a node) : 'a option =
		if i = String.length s then
			node.terminate
		else (
			match List.find_opt (fun n -> n.payload = s.[i]) node.next with
			| Some child -> go (i + 1) child
			| None -> None
		)
	in
	go 0 trie.root

let find_greedy (peek_char : unit -> char option) (skip_char : unit -> unit) (trie : 'a t) : 'a option =
	let rec go (node : 'a node) (last : 'a option) : 'a option =
		match peek_char () with
		| None -> last
		| Some c -> (
			match List.find_opt (fun n -> n.payload = c) node.next with
			| None -> last
			| Some child -> (
				skip_char ();
				let last = match child.terminate with
					| Some term -> Some term
					| None -> last
				in
				go child last
			)
		)
	in
	go trie.root None
