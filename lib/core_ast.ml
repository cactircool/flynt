(* TODO: Refine the shell ast as much as possible here into types *)

(* TODO: pattern matching in match statement *)
(* Since patterns are valid dumb expressions anyway, represent patterns in this core ast *)

type context =
	| CType
	| CFunctionParams
	| CFunction
	| CEnum
	| CAnonymousEnum
	| CSpace
	| CIf
	| CElse
	| CMatch
	| CBFor
	| CSFor
	| CBUntil
	| CSUntil
	| CBlock
	| CInitializer
	| CArrayInitializer
	| CMatchBranch
	[@@deriving show, eq]

let max_ctx_count_len = 17

let ctx_index = function
	| CType -> 0
	| CFunction -> 1
	| CEnum -> 2
	| CAnonymousEnum -> 16
	| CSpace -> 3
	| CFunctionParams -> 4
	| CIf -> 5
	| CElse -> 15
	| CMatch -> 6
	| CBFor -> 7
	| CSFor -> 8
	| CBUntil -> 9
	| CSUntil -> 10
	| CBlock -> 11
	| CInitializer -> 12
	| CArrayInitializer -> 13
	| CMatchBranch -> 14

type context_state = {
	stack : context list;
	counts : int array;
} [@@deriving show, eq]

let default_state = {
	stack = [];
	counts = Array.make max_ctx_count_len 0;
}

let push_ctx ctx state =
	let counts = Array.copy state.counts in
	counts.(ctx_index ctx) <- counts.(ctx_index ctx) + 1;
	{ stack = ctx :: state.stack; counts = counts }

let pop_ctx state =
	match state.stack with
	| [] -> state
	| h::t ->
		let counts = Array.copy state.counts in
		counts.(ctx_index h) <- counts.(ctx_index h) - 1;
		{ stack = t; counts = counts }

let peek_ctx state =
	match state.stack with
	| [] -> None
	| h::_ -> Some h

let in_ctx ctx state =
	state.counts.(ctx_index ctx) > 0

type expr =
	| ELambda of lambda
	| EIf of if_
	| EMatch of match_
	| EFor of breaking_for
	| EUntil of code list
	| EBlock of code list
	| EBinary of binary
	| EUnary of unary
	| EUnresolvedReference of Shell_ast.id (* TODO: indicates an invalid state *)
	| EReference of string list (* full resolved path *)
	| ECall of expr call
	| EIndex of expr call
	| EInitializer of initializer_
	| EArrayInitializer of array_initializer
	| ETuple of expr list

	| ELiteral of literal
	[@@deriving show]

and pattern =
	| PGuard of pattern_expr * expr
	| PStandard of pattern_expr

and pattern_expr =
	| PWildcard
	| PBind of string
	| PLiteral of literal
	| PTuple of pattern_expr list
	| PUnresolvedConstructor of Shell_ast.id * constructor_args
	| PResolvedConstructor of (string list) * constructor_args
	| PExclusiveRange of expr * expr
	| PInclusiveRange of expr * expr

and constructor_args =
	| CTuple of pattern_expr list
	| CInitializer of initializer_pattern_field list

and initializer_pattern_field =
	| IPBind of string
	| IPExpect of string * pattern_expr

and decl =
	| DVariable of variable
	| DAlias of alias
	| DType of type_
	| DFunction of named_function
	| DOperOverload of oper_overload
	| DEnum of enum
	| DSpace of space
	[@@deriving show]

and enum_member =
	| EMVariant of (string * annotation)
	| EMFunction of named_function access_restricted

and anonymous_enum_member =
	| AEMVariant of annotation
	| AEMFunction of named_function access_restricted

and alias_value =
	| AAnnotation of annotation
	| ASpace of decl access_restricted list
	| AImport of string

and annotation =
	| AAnonymousEnum of anonymous_enum_member list
	| ANamelessEnum of enum_member list
	| ANamelessType of decl access_restricted list
	| AFunction of function_type
	| ATuple of annotation list
	| AUnresolvedReference of Shell_ast.id
	| AResolvedReference of string list

	| AVariadic of annotation
	| APointer of annotation
	[@@deriving show]

and variable = {
	name : string;
	type_ref : annotation option;
	value : expr option;
}

and code =
	| UDecl of decl
	| UExpr of expr
	| UStmt of stmt

and stmt =
	| SUnresolvedUse of Shell_ast.id
	| SResolvedUse of string list
	| SStandardBreak
	| SYieldingBreak of expr
	| SContinue
	| SFor of standard_for
	| SUntil of standard_until

and 'a access_restricted =
	| ARPub of 'a
	| ARPriv of 'a
	| ARStat of 'a

and match_branch = {
	pattern : pattern;
	branch : code list;
}

and named_function = {
	name : string;
	params : variable list;
	return_type : annotation option;
	code : code list;
}

and 'a call = {
	from : 'a;
	args : 'a list;
}

and literal =
	| LInteger of string
	| LFloat of string
	| LCharacter of string
	| LString of string
	| LTrue
	| LFalse

and bin_op =
	| Scope
	| Dot
	| Multiply
	| Divide
	| Modulus
	| Add
	| Subtract
	| LeftShift
	| RightShift
	| LessThan
	| GreaterThan
	| LessThanEqualTo
	| GreaterThanEqualTo
	| In
	| Is
	| As
	| ExclusiveRange
	| InclusiveRange
	| ComparisonEquals
	| ComparisonNotEquals
	| BitwiseAnd
	| BitwiseXor
	| BitwiseOr
	| And
	| Or
	| AssignmentEquals
	| AddEquals
	| SubtractEquals
	| MultiplyEquals
	| DivideEquals
	| ModulusEquals
	| LeftShiftEquals
	| RightShiftEquals
	| BitwiseAndEquals
	| BitwiseXorEquals
	| BitwiseOrEquals

and un_op =
	| RightIncrement
	| RightDecrement
	| RightPlus
	| RightMinus
	| RightNot
	| RightQuestion
	| RightBitwiseNot
	| RightDereference
	| RightReference
	| RightDollar
	| LeftIncrement
	| LeftDecrement
	| LeftPlus
	| LeftMinus
	| LeftNot
	| LeftQuestion
	| LeftBitwiseNot
	| LeftDereference
	| LeftReference
	| LeftDollar
	| RightSpread
	| LeftSpread

and op =
	| Binary of bin_op
	| Unary of un_op
	| Bracket
	| Paren

and lambda = {
	params : variable list;
	return_type : annotation option;
	code : code list;
}

and if_ = {
	condition : expr;
	true_block : code list;
	false_block : code list;
}

and match_ = {
	switcher : expr;
	logic : match_branch list;
}

and standard_for = {
	init : code list;
	condition : expr;
	iter : code list;
	code : code list;
}

and breaking_for = {
	init : code list;
	iter : code list;
	code : code list;
}

and standard_until = {
	condition : expr;
	code : code list;
}

and binary = {
	left : expr;
	op : bin_op;
	right : expr;
}

and unary = {
	arg : expr;
	op : un_op;
}

and initializer_ = {
	type_ref : annotation;
	args : (string * expr) list;
}

and array_initializer = {
	size : expr option;
	type_ref : annotation;
	elements : expr list;
}

and alias = {
	name : string;
	value : alias_value;
}

and type_ = {
	name : string;
	members : decl access_restricted list;
}

and oper_overload = {
	op : op;
	params : variable list;
	return_type : annotation option;
	code : code list;
}

and enum = {
	name : string;
	members : enum_member list;
}

and space = {
	name : Shell_ast.id;
	members : decl access_restricted list;
}

and function_type = {
	params : annotation list;
	return_type : annotation option;
}

type prog = code list
