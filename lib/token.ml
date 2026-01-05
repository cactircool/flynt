type t =
	| Unknown
	(* Identifiers/Literals *)
	| Id of string
	| Integer of string
	| Float of string
	| Character of string
	| String of string
	| True
	| False
	(* Symbols *)
	| OpenParen
	| ClosedParen
	| OpenBrace
	| ClosedBrace
	| OpenBracket
	| ClosedBracket
	| SemiColon
	| Colon
	| Arrow
	| DoubleArrow
	| Comma
	| Eof
	(* Keywords *)
	| Let
	| Fn
	| Type
	| Priv
	| Pub
	| Stat
	| Oper
	| If
	| Else
	| For
	| Until
	| Match
	| Enum
	| Space
	| Use
	| Asm
	| Import
	| Break
	| Continue
	(* Operators *)
	(* Precedence=0 *)
	| Scope
	(* Precedence=1 *)
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
	| Dot
	(* Precedence=2 *)
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
	| Alloc
	| Clean
	(* Precedence=3 *)
	| Multiply
	| Divide
	| Modulus
	(* Precedence=4 *)
	| Add
	| Subtract
	(* Precedence=5 *)
	| LeftShift
	| RightShift
	(* Precedence=6 *)
	| LessThan
	| GreaterThan
	| LessThanEqualTo
	| GreaterThanEqualTo
	| In
	| Is
	| As
	| RightSpread
	(* Precedence=7 *)
	| ExclusiveRange
	| InclusiveRange
	(* Precedence=8 *)
	| ComparisonEquals
	| ComparisonNotEquals
	(* Precedence=9 *)
	| BitwiseAnd
	(* Precedence=10 *)
	| BitwiseXor
	(* Precedence=11 *)
	| BitwiseOr
	(* Precedence=12 *)
	| And
	(* Precedence=13 *)
	| Or
	(* Precedence=14 *)
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
	| LeftSpread
[@@deriving show, eq]

module KeywordTrie = struct
	let trie = Trie.init ()
	let () =
		List.iter (fun (p, t) -> Trie.insert p t trie) [
			("true", True);
			("false", False);
			("let", Let);
			("fn", Fn);
			("type", Type);
			("priv", Priv);
			("pub", Pub);
			("stat", Stat);
			("oper", Oper);
			("if", If);
			("else", Else);
			("for", For);
			("until", Until);
			("match", Match);
			("enum", Enum);
			("space", Space);
			("use", Use);
			("asm", Asm);
			("import", Import);
			("alloc", Alloc);
			("clean", Clean);
			("in", In);
			("is", Is);
			("as", As);
			("and", And);
			("or", Or);
			("brk", Break);
			("cont", Continue);
		];
end

module SymbolTrie = struct
	let trie = Trie.init ()
	let () =
		List.iter (fun (p, t) -> Trie.insert p t trie) [
			("(", OpenParen);
			(")", ClosedParen);
			("{", OpenBrace);
			("}", ClosedBrace);
			("[", OpenBracket);
			("]", ClosedBracket);
			(";", SemiColon);
			(":", Colon);
			("->", Arrow);
			("=>", DoubleArrow);
			(",", Comma);
			("::", Scope);
			("++", RightIncrement);
			("--", RightDecrement);
			("+", Add);
			("-", Subtract);
			("!", RightNot);
			("?", RightQuestion);
			("~", RightBitwiseNot);
			("*", Multiply);
			("&", BitwiseAnd);
			("$", RightDollar);
			(".", Dot);
			("/", Divide);
			("%", Modulus);
			("<<", LeftShift);
			(">>", RightShift);
			("<", LessThan);
			(">", GreaterThan);
			("<=", LessThanEqualTo);
			(">=", GreaterThanEqualTo);
			("...", RightSpread);
			("..", ExclusiveRange);
			("..=", InclusiveRange);
			("==", ComparisonEquals);
			("!=", ComparisonNotEquals);
			("^", BitwiseXor);
			("|", BitwiseOr);
			(*
				These are annoying to parse with ambiguity resolution for the Reference tokens
				("&&", And);
				("||", Or);
			*)
			("=", AssignmentEquals);
			("+=", AddEquals);
			("-=", SubtractEquals);
			("*=", MultiplyEquals);
			("/=", DivideEquals);
			("%=", ModulusEquals);
			("<<=", LeftShiftEquals);
			(">>=", RightShiftEquals);
			("&=", BitwiseAndEquals);
			("^=", BitwiseXorEquals);
			("|=", BitwiseOrEquals);
		];
end

let tokenize (reader : Reader.t) : (t, string) result =
	let tokenize_number_or_dot (reader : Reader.t) : (t, string) result =
		let buf = Buffer.create 16 in
		let rec loop (dot_found : bool) : (t, string) result =
			match Reader.peek reader with
			| Some c ->
				if Char.Ascii.is_digit c then (
					Reader.skip reader;
					Buffer.add_char buf c;
					loop dot_found
				)
				else if c = '.' then (
					Reader.skip reader;
					Buffer.add_char buf c;
					loop true
				)
				else if dot_found then
					Ok (Float (Buffer.contents buf))
				else
			 		Ok (Integer (Buffer.contents buf))
			| None -> (
				if dot_found then
					Ok (Float (Buffer.contents buf))
				else
					Ok (Integer (Buffer.contents buf))
			) in
		match loop false with
		| Ok (Float ".") -> Ok Dot
		| _ as res -> res
	in

	let tokenize_id_or_keyword (reader : Reader.t) : (t, string) result =
		let buf = Buffer.create 16 in
		let rec loop () : (t, string) result =
			match Reader.peek reader with
			| Some c when Char.Ascii.is_letter c || Char.Ascii.is_digit c || c = '_' -> (
				Reader.skip reader;
				Buffer.add_char buf c;
				loop ()
			)
			| _ -> (
				let s = Buffer.contents buf in
				match Trie.find_exact s KeywordTrie.trie with
				| Some kw -> Ok kw
				| None -> Ok (Id s)
			)
		in
		loop ()
	in

	let tokenize_char_or_string (reader : Reader.t) : (t, string) result =
		let start = Reader.get reader in
		let resolve s = if start = '"' then String s else Character s in
		let buf = Buffer.create 16 in
		let rec loop (slash : bool) : (t, string) result =
			match Reader.peek reader with
			| Some c -> (
				Reader.skip reader;
				if c = start && not slash then (
					Ok (resolve (Buffer.contents buf))
				) else (
					Buffer.add_char buf c;
					loop ((not slash) && c = '\\')
				)
			)
			| None -> Error "unterminated string"
		in
		loop false
	in

	let tokenize_symbol (reader : Reader.t) : (t, string) result =
		match Trie.find_greedy (fun () -> Reader.peek reader) (fun () -> Reader.skip reader) SymbolTrie.trie with
		| Some t -> Ok t
		| None -> Error "no symbol found"
	in

	match Reader.peek reader with
	| Some c -> (
		if Char.Ascii.is_digit c || c = '.' then
			tokenize_number_or_dot reader
		else if Char.Ascii.is_letter c || c = '_' then
			tokenize_id_or_keyword reader
		else if (c = '\'') || (c = '"') then
			tokenize_char_or_string reader
		else
			tokenize_symbol reader
	)
	| None -> Ok Eof

let ambiguous (tok : t) : bool =
	match tok with
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
	| Multiply
	| Add
	| Subtract
	| LeftShift
	| RightShift
	| RightSpread
	| BitwiseAnd
	| LeftSpread -> true
	| _ -> false

let left (tok : t) : bool =
	match tok with
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
	| Alloc
	| Clean
	| LeftSpread -> true
	| _ -> false

let right (tok : t) : bool =
	match tok with
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

let binary (tok : t) : bool =
	match tok with
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
	| BitwiseOrEquals -> true
	| _ -> false

let prec (tok : t) : int option =
	match tok with
	(* Precedence=0 *)
	| Scope -> Some 0
	(* Precedence=1 *)
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
	| Dot -> Some 1
	(* Precedence=2 *)
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
	| Alloc
	| Clean -> Some 2
	(* Precedence=3 *)
	| Multiply
	| Divide
	| Modulus -> Some 3
	(* Precedence=4 *)
	| Add
	| Subtract -> Some 4
	(* Precedence=5 *)
	| LeftShift
	| RightShift -> Some 5
	(* Precedence=6 *)
	| LessThan
	| GreaterThan
	| LessThanEqualTo
	| GreaterThanEqualTo
	| In
	| Is
	| As
	| RightSpread -> Some 6
	(* Precedence=7 *)
	| ExclusiveRange
	| InclusiveRange -> Some 7
	(* Precedence=8 *)
	| ComparisonEquals
	| ComparisonNotEquals -> Some 8
	(* Precedence=9 *)
	| BitwiseAnd -> Some 9
	(* Precedence=10 *)
	| BitwiseXor -> Some 10
	(* Precedence=11 *)
	| BitwiseOr -> Some 11
	(* Precedence=12 *)
	| And -> Some 12
	(* Precedence=13 *)
	| Or -> Some 13
	(* Precedence=14 *)
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
	| LeftSpread -> Some 14

	| _ -> None

let oper (tok : t) : bool =
	match prec tok with
	| Some _ -> true
	| None -> false

let keyword (tok : t) : bool =
	match tok with
	| Let
	| Fn
	| Type
	| Priv
	| Pub
	| Stat
	| Oper
	| If
	| Else
	| For
	| Until
	| Match
	| Enum
	| Space
	| Use
	| Asm
	| Import
	| Break
	| Continue -> true
	| _ -> false

let symbol (tok : t) : bool =
	match tok with
	| OpenParen
	| ClosedParen
	| OpenBrace
	| ClosedBrace
	| OpenBracket
	| ClosedBracket
	| SemiColon
	| Colon
	| Arrow
	| DoubleArrow
	| Comma
	| Eof -> true
	| _ -> false

let unknown (tok : t) : bool =
	match tok with
	| Unknown -> true
	| _ -> false

let literal (tok : t) : bool =
	match tok with
	| Integer _
	| Float _
	| Character _
	| String _
	| True
	| False -> true
	| _ -> false

let id (tok : t) : bool =
	match tok with
	| Id _ -> true
	| _ -> false

let leftify (tok : t) : t =
	match tok with
	| RightIncrement -> LeftIncrement
	| RightDecrement -> LeftDecrement
	| RightPlus -> LeftPlus
	| RightMinus -> LeftMinus
	| RightNot -> LeftNot
	| RightQuestion -> LeftQuestion
	| RightBitwiseNot -> LeftBitwiseNot
	| RightDereference -> LeftDereference
	| RightReference -> LeftReference
	| RightDollar -> LeftDollar
	| RightSpread -> LeftSpread
	| Multiply -> LeftDereference
	| Add -> LeftPlus
	| Subtract -> LeftMinus
	| RightShift -> LeftShift
	| BitwiseAnd -> LeftReference
	| _ -> tok

let rightify (tok : t) : t =
	match tok with
	| LeftIncrement -> RightIncrement
	| LeftDecrement -> RightDecrement
	| LeftPlus -> RightPlus
	| LeftMinus -> RightMinus
	| LeftNot -> RightNot
	| LeftQuestion -> RightQuestion
	| LeftBitwiseNot -> RightBitwiseNot
	| LeftDereference -> RightDereference
	| LeftReference -> RightReference
	| LeftDollar -> RightDollar
	| LeftSpread -> RightSpread
	| Multiply -> RightDereference
	| Add -> RightPlus
	| Subtract -> RightMinus
	| LeftShift -> RightShift
	| BitwiseAnd -> RightReference
	| _ -> tok

let value (tok : t) : string =
	match tok with
	| Unknown -> "<unknown>"
	(* Identifiers/Literals *)
	| Id s -> s
	| Integer s -> s
	| Float s -> s
	| Character s -> s
	| String s -> s
	| True -> "true"
	| False -> "false"
	(* Symbols *)
	| OpenParen -> "("
	| ClosedParen -> ")"
	| OpenBrace -> "{"
	| ClosedBrace -> "}"
	| OpenBracket -> "["
	| ClosedBracket -> "]"
	| SemiColon -> ";"
	| Colon -> ":"
	| Arrow -> "->"
	| DoubleArrow -> "=>"
	| Comma -> ","
	| Eof -> "<eof>"
	(* Keywords *)
	| Let -> "let"
	| Fn -> "fn"
	| Type -> "type"
	| Priv -> "priv"
	| Pub -> "pub"
	| Stat -> "stat"
	| Oper -> "oper"
	| If -> "if"
	| Else -> "else"
	| For -> "for"
	| Until -> "until"
	| Match -> "match"
	| Enum -> "enum"
	| Space -> "space"
	| Use -> "use"
	| Asm -> "asm"
	| Import -> "import"
	| Break -> "brk"
	| Continue -> "cont"
	(* Operators *)
	(* Precedence=0 *)
	| Scope -> "::"
	(* Precedence=1 *)
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
	| Dot -> "."
	(* Precedence=2 *)
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
	| Alloc -> "alloc"
	| Clean -> "clean"
	(* Precedence=3 *)
	| Multiply -> "*"
	| Divide -> "/"
	| Modulus -> "%"
	(* Precedence=4 *)
	| Add -> "+"
	| Subtract -> "-"
	(* Precedence=5 *)
	| LeftShift -> "<<"
	| RightShift -> ">>"
	(* Precedence=6 *)
	| LessThan -> "<"
	| GreaterThan -> ">"
	| LessThanEqualTo -> "<="
	| GreaterThanEqualTo -> ">="
	| In -> "in"
	| Is -> "is"
	| As -> "as"
	| RightSpread -> "..."
	(* Precedence=7 *)
	| ExclusiveRange -> ".."
	| InclusiveRange -> "..="
	(* Precedence=8 *)
	| ComparisonEquals -> "=="
	| ComparisonNotEquals -> "!="
	(* Precedence=9 *)
	| BitwiseAnd -> "&"
	(* Precedence=10 *)
	| BitwiseXor -> "^"
	(* Precedence=11 *)
	| BitwiseOr -> "|"
	(* Precedence=12 *)
	| And -> "and"
	(* Precedence=13 *)
	| Or -> "or"
	(* Precedence=14 *)
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
	| LeftSpread -> "..."

let scope (tok : t) : int =
	match tok with
	| OpenBrace
	| OpenBracket
	| OpenParen -> 1
	| ClosedBrace
	| ClosedBracket
	| ClosedParen -> -1
	| _ -> 0

let binaryify (tok : t) : t =
	match tok with
	| RightPlus
	| LeftPlus -> Add
	| RightMinus
	| LeftMinus -> Subtract
	| RightDereference
	| LeftDereference -> Multiply
	| RightReference
	| LeftReference -> BitwiseAnd
	| _ -> tok

let has_right (tok : t) : bool =
	right tok || ((rightify tok) <> tok)

let has_left (tok : t) : bool =
	left tok || ((leftify tok) <> tok)

let has_binary (tok : t) : bool =
	binary tok || ((binaryify tok) <> tok)

let bp (tok : t) : (int option * int option) =
	match tok with
	(* Precedence=0 - Special scope operator *)
	| Scope -> (Some 0, Some 1)  (* left-to-right *)

	(* Precedence=1 - Postfix unary operators *)
	| RightIncrement
	| RightDecrement
	| RightPlus
	| RightMinus
	| RightNot
	| RightQuestion
	| RightBitwiseNot
	| RightDereference
	| RightReference
	| RightDollar -> (Some 1, None)
	| Dot -> (Some 1, Some 2)  (* member access, left-to-right *)

	(* Precedence=2 - Prefix unary operators *)
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
	| Alloc
	| Clean -> (None, Some 2)

	(* Precedence=3 - Multiplicative, left-to-right *)
	| Multiply
	| Divide
	| Modulus -> (Some 3, Some 4)

	(* Precedence=4 - Additive, left-to-right *)
	| Add
	| Subtract -> (Some 5, Some 6)

	(* Precedence=5 - Bitwise shifts, left-to-right *)
	| LeftShift
	| RightShift -> (Some 7, Some 8)

	(* Precedence=6 - Relational/type operators, left-to-right *)
	| LessThan
	| GreaterThan
	| LessThanEqualTo
	| GreaterThanEqualTo
	| In
	| Is
	| As -> (Some 9, Some 10)
	| RightSpread -> (Some 9, None)  (* postfix spread *)

	(* Precedence=7 - Range operators, left-to-right *)
	| ExclusiveRange
	| InclusiveRange -> (Some 11, Some 12)

	(* Precedence=8 - Equality, left-to-right *)
	| ComparisonEquals
	| ComparisonNotEquals -> (Some 13, Some 14)

	(* Precedence=9 - Bitwise AND, left-to-right *)
	| BitwiseAnd -> (Some 15, Some 16)

	(* Precedence=10 - Bitwise XOR, left-to-right *)
	| BitwiseXor -> (Some 17, Some 18)

	(* Precedence=11 - Bitwise OR, left-to-right *)
	| BitwiseOr -> (Some 19, Some 20)

	(* Precedence=12 - Logical AND, left-to-right *)
	| And -> (Some 21, Some 22)

	(* Precedence=13 - Logical OR, left-to-right *)
	| Or -> (Some 23, Some 24)

	(* Precedence=14 - Assignment operators, right-to-left *)
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
	| BitwiseOrEquals -> (Some 26, Some 25)  (* right-to-left: right_bp < left_bp *)
	| LeftSpread -> (None, Some 25)  (* prefix spread *)

	| _ -> (None, None)
