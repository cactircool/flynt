open Flynt

let output = ref ""
let chill = ref false
let input_files = ref []
let help = [
	("-o", Arg.Set_string output, " Output file");
	("--chill", Arg.Set chill, " Allow root level expressions (run top down in order of least dependency before main)")
]

let () =
	Arg.parse help (fun arg -> input_files := arg :: (!input_files))
		"Usage: flynt [options] files...";

	Eio_main.run @@ fun env -> (
		(* Preprocess *)

		(* Parse *)
		let _ = Shell_parser.parse_entry !chill !input_files env in

		(* Analyze *)

		(* Optimize *)

		(* ASM *)

		(* Optimize *)

		()
	)
