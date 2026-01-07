(* TODO: Refine the shell ast as much as possible here into types *)

(* TODO: pattern matching in match statement *)
(* Since patterns are valid dumb expressions anyway, represent patterns in this core ast *)

type expr =
	| ELiteral

type decl =
	| DVariable
	| DType
	| DTuple
	| DFunction
	| DOperOverload
	| DEnum
	| DSpace

type annotation =
	| AAnonymousEnum
	| ANamelessEnum
	| ANamelessType
	| AFunction
	| ATuple
	| AReference
