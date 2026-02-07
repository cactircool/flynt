type 'a fat = Shell_ast.bounds * 'a [@@deriving show, eq]

type expr =
	| ELambda of lambda
	| EIf of if_
	| EMatch of match_
	| EFor of breaking_for
	| EUntil of code list fat
	| EBlock of code list fat
	| EBinary of binary
	| EUnary of unary
	| EUnresolvedReference of Shell_ast.id fat
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
	| PBind of int fat
	| PLiteral of literal
	| PTuple of pattern_expr list
	| PUnresolvedConstructor of (Shell_ast.id * constructor_args) fat
	| PExclusiveRange of (expr * expr) fat
	| PInclusiveRange of (expr * expr) fat

and constructor_args =
	| CTuple of pattern_expr list
	| CInitializer of initializer_pattern_field list

and initializer_pattern_field =
	| IPBind of int fat
	| IPExpect of (int * pattern_expr) fat

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
	| EMVariant of (int * annotation) fat
	| EMFunction of named_function access_restricted

and anonymous_enum_member =
	| AEMVariant of (int * annotation)
	| AEMFunction of named_function access_restricted

and alias_value =
	| AAnnotation of annotation
	| ASpace of decl access_restricted list
	| AImport of string fat

and annotation =
	| AAnonymousEnum of int * anonymous_enum_member list
	| ANamelessEnum of int * enum_member list
	| ANamelessType of int * decl access_restricted list
	| AFunction of function_type
	| ATuple of annotation list
	| AUnresolvedReference of Shell_ast.id fat

	| AVariadic of annotation
	| APointer of annotation
	[@@deriving show]

and variable = {
	bounds : Shell_ast.bounds;
	id : int;
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
	id : int;
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
	type_ref : annotation;
	args : (int * expr) list;
}

and array_initializer = {
	bounds : Shell_ast.bounds;
	size : expr option;
	type_ref : annotation;
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
	params : variable list;
	return_type : annotation option;
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

and function_type = {
	bounds : Shell_ast.bounds;
	params : annotation list;
	return_type : annotation option;
}

type prog = code list
