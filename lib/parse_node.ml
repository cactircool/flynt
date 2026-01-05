(* (bounds, from_root, id chain) *)
type bounds = Lexer.fat_token * Lexer.fat_token [@@deriving show]
type id = bool * string list [@@deriving show]

type node =
	| Program of (string * block) list

	| Pub of fat_node
	| Priv of fat_node
	| Stat of fat_node

	| Id of id

	| Variable of variable
	| Type of layer (* name = "" for anonymous *)
	| Alias of string * fat_node
	| Tuple of block
	| Function of (string function_t)
	| OperatorOverload of {
		name : Token.t;
		params : fat_node list;
		result : fat_node option;
		code : block;
	}
	| EnumVariant of enum_variant
	| Enum of layer (* name = "" for anonymous *)
	| AnonymousEnum of block
	| Space of {
		name : id;
		members : block;
	}

	| Use of id
	| Import of {
		path : string;
	}

	| If of {
		condition : fat_node; (* enforce bool *)
		true_block : fat_node;
		false_block : fat_node option;
	}
	| Match of {
		switcher : fat_node;
		cases : block;
	}
	| MatchCase of {
		expr : fat_node;
		logic : fat_node option;
	}
	| For of {
		params: block;
		block : block;
	}
	| Until of {
		condition : fat_node;
		block : block;
	}

	| Block of fat_node list
	| Break of fat_node option
	| Continue
	| Binary of {
		left : fat_node;
		op : Token.t;
		right : fat_node option;
	}
	| Unary of {
		op : Token.t;
		arg : fat_node option;
	}
	| Reference of id (* used for any reference since :: counts as an operator in expressions *)
	| Call of {
		from : fat_node;
		args : block;
	}
	| Index of {
		from : fat_node;
		args : block;
	}
	| Initializer of {
		type_ : fat_node;
		args : (string * fat_node) list;
	}
	| ArrayInitializer of {
		size : fat_node option;
		type_ : id;
		args : block;
	}
	| Integer of Lexer.fat_token
	| Float of Lexer.fat_token
	| Character of Lexer.fat_token
	| String of Lexer.fat_token
	| True of Lexer.fat_token
	| False of Lexer.fat_token

	| Parenthesis of fat_node
	[@@deriving show]

and block = fat_node (* specifically the Block variant *) [@@deriving show]

and enum_variant = {
	name : string;
	type_ : fat_node;
} [@@deriving show]

and variable = {
	name : string;
	type_ : fat_node option;
	value : fat_node option;
} [@@deriving show]

and 'a function_t = {
	name : 'a;
	params : block;
	result : fat_node option;
	code : block;
} [@@deriving show]

and layer = {
	name : string;
	members : block;
} [@@deriving show]

and fat_node = bounds * node [@@deriving show]

let last (f : fat_node) = let ((_, last), _) = f in last

let start (f : fat_node) = let ((start, _), _) = f in start

let bounds (f : fat_node) = let (bnds, _) = f in bnds

let node (f : fat_node) = let (_, node) = f in node
