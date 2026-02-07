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

type 'a fat = Shell_ast.bounds * 'a [@@deriving show, eq]

type bin_op =
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
	[@@deriving show, eq]

type un_op =
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
	[@@deriving show, eq]

type op =
	| Binary of bin_op
	| Unary of un_op
	| Bracket
	| Paren
	[@@deriving show, eq]

type expr =
	| ELambda of lambda
	| EIf of if_
	| EMatch of match_
	| EFor of breaking_for
	| EUntil of code list fat (* TODO refactor parser *)
	| EBlock of code list fat (* TODO refactor parser *)
	| EBinary of binary
	| EUnary of unary
	| EUnresolvedReference of Shell_ast.id fat (* TODO: indicates an invalid state *)
	| ECall of expr call
	| EIndex of expr call
	| EInitializer of initializer_
	| EArrayInitializer of array_initializer
	| ETuple of expr list

	| ELiteral of literal
	[@@deriving show, eq]

and pattern =
	| PGuard of (pattern_expr * expr) fat
	| PStandard of pattern_expr

and pattern_expr =
	| PWildcard of Shell_ast.bounds
	| PBind of string fat
	| PLiteral of literal
	| PTuple of pattern_expr list
	| PUnresolvedConstructor of (Shell_ast.id * constructor_args) fat
	| PExclusiveRange of (expr * expr) fat
	| PInclusiveRange of (expr * expr) fat

and constructor_args =
	| CTuple of pattern_expr list
	| CInitializer of initializer_pattern_field list

and initializer_pattern_field =
	| IPBind of string fat
	| IPExpect of (string * pattern_expr) fat

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
	| EMVariant of (string * annotation) fat
	| EMFunction of named_function access_restricted

and anonymous_enum_member =
	| AEMVariant of annotation
	| AEMFunction of named_function access_restricted

and alias_value =
	| AAnnotation of annotation
	| ASpace of decl access_restricted list
	| AImport of string fat

and annotation =
	| AAnonymousEnum of anonymous_enum_member list
	| ANamelessEnum of enum_member list
	| ANamelessType of decl access_restricted list
	| AFunction of function_type
	| ATuple of annotation list
	| AUnresolvedReference of Shell_ast.id fat

	| AVariadic of annotation
	| APointer of annotation
	[@@deriving show]

and variable = {
	bounds : Shell_ast.bounds;
	name : string;
	type_ref : annotation option;
	value : expr option;
}

and code =
	| UDecl of decl
	| UExpr of expr
	| UStmt of stmt

and stmt =
	| SUnresolvedUse of Shell_ast.id fat
	| SStandardBreak of Shell_ast.bounds
	| SYieldingBreak of expr fat
	| SContinue of Shell_ast.bounds
	| SFor of standard_for
	| SUntil of standard_until

and 'a access_restricted =
	| ARPub of 'a fat
	| ARPriv of 'a fat
	| ARStat of 'a fat

and match_branch = {
	bounds : Shell_ast.bounds;
	pattern : pattern;
	branch : code list;
}

and named_function = {
	bounds : Shell_ast.bounds;
	name : string;
	params : variable list;
	return_type : annotation option;
	code : code list;
}

and 'a call = {
	bounds : Shell_ast.bounds;
	from : 'a;
	args : 'a list;
}

and literal =
	| LInteger of string fat
	| LFloat of string fat
	| LCharacter of string fat
	| LString of string fat
	| LTrue of Shell_ast.bounds
	| LFalse of Shell_ast.bounds

and lambda = {
	bounds : Shell_ast.bounds;
	params : variable list;
	return_type : annotation option;
	code : code list;
}

and if_ = {
	bounds : Shell_ast.bounds;
	condition : expr;
	true_block : code list;
	false_block : code list;
}

and match_ = {
	bounds : Shell_ast.bounds;
	switcher : expr;
	logic : match_branch list;
}

and standard_for = {
	bounds : Shell_ast.bounds;
	init : code list;
	condition : expr;
	iter : code list;
	code : code list;
}

and breaking_for = {
	bounds : Shell_ast.bounds;
	init : code list;
	iter : code list;
	code : code list;
}

and standard_until = {
	bounds : Shell_ast.bounds;
	condition : expr;
	code : code list;
}

and binary = {
	bounds : Shell_ast.bounds;
	left : expr;
	op : bin_op;
	right : expr;
}

and unary = {
	bounds : Shell_ast.bounds;
	arg : expr;
	op : un_op;
}

and initializer_ = {
	bounds : Shell_ast.bounds;
	type_ref : annotation;
	args : (string * expr) list;
}

and array_initializer = {
	bounds : Shell_ast.bounds;
	size : expr option;
	type_ref : annotation;
	elements : expr list;
}

and alias = {
	bounds : Shell_ast.bounds;
	name : string;
	value : alias_value;
}

and type_ = {
	bounds : Shell_ast.bounds;
	name : string;
	members : decl access_restricted list;
}

and oper_overload = {
	bounds : Shell_ast.bounds;
	op : op;
	params : variable list;
	return_type : annotation option;
	code : code list;
}

and enum = {
	bounds : Shell_ast.bounds;
	name : string;
	members : enum_member list;
}

and space = {
	bounds : Shell_ast.bounds;
	name : Shell_ast.id;
	members : decl access_restricted list;
}

and function_type = {
	bounds : Shell_ast.bounds;
	params : annotation list;
	return_type : annotation option;
}

type prog = code list

let binop_value = function
	| Scope -> "::"
	| Dot -> "."
	| Multiply -> "*"
	| Divide -> "/"
	| Modulus -> "%"
	| Add -> "+"
	| Subtract -> "-"
	| LeftShift -> "<<"
	| RightShift -> ">>"
	| LessThan -> "<"
	| GreaterThan -> ">"
	| LessThanEqualTo -> "<="
	| GreaterThanEqualTo -> ">="
	| In -> "in"
	| Is -> "is"
	| As -> "as"
	| ExclusiveRange -> ".."
	| InclusiveRange -> "..="
	| ComparisonEquals -> "=="
	| ComparisonNotEquals -> "!="
	| BitwiseAnd -> "&"
	| BitwiseXor -> "^"
	| BitwiseOr -> "|"
	| And -> "and"
	| Or -> "or"
	| AssignmentEquals -> "="
	| AddEquals -> "+="
	| SubtractEquals -> "-="
	| MultiplyEquals -> "*="
	| DivideEquals -> "/="
	| ModulusEquals -> "%="
	| LeftShiftEquals -> "<<="
	| RightShiftEquals -> ">>="
	| BitwiseAndEquals -> "&="
	| BitwiseXorEquals -> "^="
	| BitwiseOrEquals -> "|="

let unop_left = function
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
	| RightSpread -> true
	| _ -> false

let unop_value = function
	| RightIncrement -> "++"
	| RightDecrement -> "--"
	| RightPlus -> "+"
	| RightMinus -> "-"
	| RightNot -> "!"
	| RightQuestion -> "?"
	| RightBitwiseNot -> "~"
	| RightDereference -> "*"
	| RightReference -> "&"
	| RightDollar -> "$"
	| LeftIncrement -> "++"
	| LeftDecrement -> "--"
	| LeftPlus -> "+"
	| LeftMinus -> "-"
	| LeftNot -> "!"
	| LeftQuestion -> "?"
	| LeftBitwiseNot -> "~"
	| LeftDereference -> "*"
	| LeftReference -> "&"
	| LeftDollar -> "$"
	| RightSpread -> "..."
	| LeftSpread -> "..."
