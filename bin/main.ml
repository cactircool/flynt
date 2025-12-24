open Flynt

let output = ref ""
let input_files = ref []
let help = [
	("-o", Arg.Set_string output, " Output file");
]

let () =
	Arg.parse help (fun arg -> input_files := arg :: (!input_files))
		"Usage: flynt [options] files...";

	Eio_main.run @@ fun env -> (
		(* Preprocess *)

		(* Parse *)
		let _ = Parser.parse_entry !input_files env in

		(* Analyze *)

		(* Optimize *)

		(* ASM *)

		(* Optimize *)

		()
	)
