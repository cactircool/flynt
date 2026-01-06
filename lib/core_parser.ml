(* Set of functions that take a shell ast and validate + convert it into a core ast with stricter invariants *)

exception SemanticError of Shell_ast.bounds * string
