type 'a fat = Shell_ast.bounds * 'a [@@deriving show, eq]

type type_ref =
	| TReference of int (* types/enums/alias/anonymous_enum *)
	| TFunction of (type_ref list) * type_ref
	| TTuple of type_ref list
	| TVariadic of type_ref
	| TPointer of type_ref
	[@@deriving show, eq]

type expr =
	| ELambda of lambda * type_ref
	| EIf of if_ * type_ref
	| EMatch of match_ * type_ref
	| EFor of breaking_for * type_ref
	| EUntil of code list fat * type_ref
	| EBlock of code list fat * type_ref
	| EBinary of binary * type_ref
	| EUnary of unary * type_ref
	| EResolvedReference of int fat * type_ref
	| ECall of expr call * type_ref
	| EIndex of expr call * type_ref
	| EInitializer of initializer_ * type_ref
	| EArrayInitializer of array_initializer * type_ref
	| ETuple of expr list * type_ref

	| ELiteral of literal * type_ref
	[@@deriving show, eq]

and pattern =
	| PGuard of (pattern_expr * expr) fat
	| PStandard of pattern_expr

and pattern_expr =
	| PWildcard of Shell_ast.bounds
	| PBind of int fat
	| PLiteral of (literal * type_ref)
	| PTuple of pattern_expr list
	| PConstructor of (int * constructor_args) fat
	| PExclusiveRange of (expr * expr) fat
	| PInclusiveRange of (expr * expr) fat

and constructor_args =
	| CTuple of pattern_expr list
	| CInitializer of initializer_pattern_field list

and initializer_pattern_field =
	| IPBind of int fat
	| IPExpect of (int * pattern_expr) fat

and decl =
	| DUnassignedVariable of unassigned_variable
	| DAssignedVariable of assigned_variable
	| DAlias of alias
	| DType of type_
	| DFunction of named_function
	| DOperOverload of oper_overload
	| DEnum of enum
	| DSpace of space
	[@@deriving show]

and enum_member =
	| EMVariant of (int * type_ref) fat
	| EMFunction of named_function access_restricted

and anonymous_enum_member =
	| AEMVariant of (int * type_ref)
	| AEMFunction of named_function access_restricted

and alias_value =
	| AAnnotation of type_ref
	| ASpace of decl access_restricted list
	| AImport of string fat

and unassigned_variable = {
	bounds : Shell_ast.bounds;
	id : int;
	type_ref : type_ref;
}

and assigned_variable = {
	bounds : Shell_ast.bounds;
	id : int;
	type_ref : type_ref;
	value : expr;
}

and code =
	| UDecl of decl
	| UExpr of expr
	| UStmt of stmt

and stmt =
	| SUse of int fat
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
	id : int;
	required_params : unassigned_variable list;
	default_params : assigned_variable list;
	return_type : type_ref;
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
	required_params : unassigned_variable list;
	default_params : assigned_variable list;
	return_type : type_ref;
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
	op : Core_ast.bin_op;
	right : expr;
}

and unary = {
	bounds : Shell_ast.bounds;
	arg : expr;
	op : Core_ast.un_op;
}

and initializer_ = {
	bounds : Shell_ast.bounds;
	type_ref : type_ref;
	args : (int * expr) list;
}

and array_initializer = {
	bounds : Shell_ast.bounds;
	size : expr;
	type_ref : type_ref;
	elements : expr list;
}

and alias = {
	bounds : Shell_ast.bounds;
	id : int;
	value : alias_value;
}

and type_ = {
	bounds : Shell_ast.bounds;
	id : int;
	members : decl access_restricted list;
}

and oper_overload = {
	id : int;
	bounds : Shell_ast.bounds;
	required_params : unassigned_variable list;
	default_params : assigned_variable list;
	return_type : type_ref;
	code : code list;
}

and enum = {
	bounds : Shell_ast.bounds;
	id : int;
	members : enum_member list;
}

and space = {
	bounds : Shell_ast.bounds;
	id : int;
	members : decl access_restricted list;
}

type prog = code list
