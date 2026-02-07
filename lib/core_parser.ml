(* Set of functions that take a shell ast and validate + convert it into a core ast with stricter invariants *)

open Shell_ast
open Core_ast

exception InternalError of string
exception FatalError of string * bounds * string

let is_decl node =
	match node with
	| Variable _
	| Type _
	| Alias _
	| Function _
	| OperatorOverload _
	| Enum _
	| Space _ -> true
	| _ -> false

let is_stmt node =
	match node with
	| Use _
	| StandardBreak
	| YieldingBreak _
	| Continue
	| StandardFor _
	| StandardUntil _ -> true
	| _ -> false

let is_access node =
	match node with
	| Pub _
	| Priv _
	| Stat _ -> true
	| _ -> false

let gen_bin_op = function
	| Token.Scope -> Scope
	| Token.Dot -> Dot
	| Token.Multiply -> Multiply
	| Token.Divide -> Divide
	| Token.Modulus -> Modulus
	| Token.Add -> Add
	| Token.Subtract -> Subtract
	| Token.LeftShift -> LeftShift
	| Token.RightShift -> RightShift
	| Token.LessThan -> LessThan
	| Token.GreaterThan -> GreaterThan
	| Token.LessThanEqualTo -> LessThanEqualTo
	| Token.GreaterThanEqualTo -> GreaterThanEqualTo
	| Token.In -> In
	| Token.Is -> Is
	| Token.As -> As
	| Token.ExclusiveRange -> ExclusiveRange
	| Token.InclusiveRange -> InclusiveRange
	| Token.ComparisonEquals -> ComparisonEquals
	| Token.ComparisonNotEquals -> ComparisonNotEquals
	| Token.BitwiseAnd -> BitwiseAnd
	| Token.BitwiseXor -> BitwiseXor
	| Token.BitwiseOr -> BitwiseOr
	| Token.And -> And
	| Token.Or -> Or
	| Token.AssignmentEquals -> AssignmentEquals
	| Token.AddEquals -> AddEquals
	| Token.SubtractEquals -> SubtractEquals
	| Token.MultiplyEquals -> MultiplyEquals
	| Token.DivideEquals -> DivideEquals
	| Token.ModulusEquals -> ModulusEquals
	| Token.LeftShiftEquals -> LeftShiftEquals
	| Token.RightShiftEquals -> RightShiftEquals
	| Token.BitwiseAndEquals -> BitwiseAndEquals
	| Token.BitwiseXorEquals -> BitwiseXorEquals
	| Token.BitwiseOrEquals -> BitwiseOrEquals
	| t -> raise (InternalError (Printf.sprintf "invalid binary operator: '%s'" (Token.value t)))

let gen_un_op = function
	| Token.RightIncrement -> RightIncrement
	| Token.RightDecrement -> RightDecrement
	| Token.RightPlus -> RightPlus
	| Token.RightMinus -> RightMinus
	| Token.RightNot -> RightNot
	| Token.RightQuestion -> RightQuestion
	| Token.RightBitwiseNot -> RightBitwiseNot
	| Token.RightDereference -> RightDereference
	| Token.RightReference -> RightReference
	| Token.RightDollar -> RightDollar
	| Token.LeftIncrement -> LeftIncrement
	| Token.LeftDecrement -> LeftDecrement
	| Token.LeftPlus -> LeftPlus
	| Token.LeftMinus -> LeftMinus
	| Token.LeftNot -> LeftNot
	| Token.LeftQuestion -> LeftQuestion
	| Token.LeftBitwiseNot -> LeftBitwiseNot
	| Token.LeftDereference -> LeftDereference
	| Token.LeftReference -> LeftReference
	| Token.LeftDollar -> LeftDollar
	| Token.RightSpread -> RightSpread
	| Token.LeftSpread -> LeftSpread
	| t -> raise (InternalError (Printf.sprintf "invalid unary operator: '%s'" (Token.value t)))

let gen_op = function
	| Token.Scope -> Binary Scope
	| Token.Dot -> Binary Dot
	| Token.Multiply -> Binary Multiply
	| Token.Divide -> Binary Divide
	| Token.Modulus -> Binary Modulus
	| Token.Add -> Binary Add
	| Token.Subtract -> Binary Subtract
	| Token.LeftShift -> Binary LeftShift
	| Token.RightShift -> Binary RightShift
	| Token.LessThan -> Binary LessThan
	| Token.GreaterThan -> Binary GreaterThan
	| Token.LessThanEqualTo -> Binary LessThanEqualTo
	| Token.GreaterThanEqualTo -> Binary GreaterThanEqualTo
	| Token.In -> Binary In
	| Token.Is -> Binary Is
	| Token.As -> Binary As
	| Token.ExclusiveRange -> Binary ExclusiveRange
	| Token.InclusiveRange -> Binary InclusiveRange
	| Token.ComparisonEquals -> Binary ComparisonEquals
	| Token.ComparisonNotEquals -> Binary ComparisonNotEquals
	| Token.BitwiseAnd -> Binary BitwiseAnd
	| Token.BitwiseXor -> Binary BitwiseXor
	| Token.BitwiseOr -> Binary BitwiseOr
	| Token.And -> Binary And
	| Token.Or -> Binary Or
	| Token.AssignmentEquals -> Binary AssignmentEquals
	| Token.AddEquals -> Binary AddEquals
	| Token.SubtractEquals -> Binary SubtractEquals
	| Token.MultiplyEquals -> Binary MultiplyEquals
	| Token.DivideEquals -> Binary DivideEquals
	| Token.ModulusEquals -> Binary ModulusEquals
	| Token.LeftShiftEquals -> Binary LeftShiftEquals
	| Token.RightShiftEquals -> Binary RightShiftEquals
	| Token.BitwiseAndEquals -> Binary BitwiseAndEquals
	| Token.BitwiseXorEquals -> Binary BitwiseXorEquals
	| Token.BitwiseOrEquals -> Binary BitwiseOrEquals

	| Token.RightIncrement -> Unary RightIncrement
	| Token.RightDecrement -> Unary RightDecrement
	| Token.RightPlus -> Unary RightPlus
	| Token.RightMinus -> Unary RightMinus
	| Token.RightNot -> Unary RightNot
	| Token.RightQuestion -> Unary RightQuestion
	| Token.RightBitwiseNot -> Unary RightBitwiseNot
	| Token.RightDereference -> Unary RightDereference
	| Token.RightReference -> Unary RightReference
	| Token.RightDollar -> Unary RightDollar
	| Token.LeftIncrement -> Unary LeftIncrement
	| Token.LeftDecrement -> Unary LeftDecrement
	| Token.LeftPlus -> Unary LeftPlus
	| Token.LeftMinus -> Unary LeftMinus
	| Token.LeftNot -> Unary LeftNot
	| Token.LeftQuestion -> Unary LeftQuestion
	| Token.LeftBitwiseNot -> Unary LeftBitwiseNot
	| Token.LeftDereference -> Unary LeftDereference
	| Token.LeftReference -> Unary LeftReference
	| Token.LeftDollar -> Unary LeftDollar
	| Token.RightSpread -> Unary RightSpread
	| Token.LeftSpread -> Unary LeftSpread

	| Token.OpenBracket | Token.ClosedBracket -> Bracket
	| Token.OpenParen | Token.ClosedParen -> Paren

	| t -> raise (InternalError (Printf.sprintf "invalid operator: '%s'" (Token.value t)))

let map_nodes mapper nodes state =
	(* let rec loop bnds stack acc =
		match stack with
		| [] -> bnds, acc
		| h::t ->
			loop (fst bnds, snd (fst h)) t ((mapper state h)::acc)
	in
	match nodes with
	| [] -> (Lexer.eof, Lexer.eof), []
	| nodes -> loop (fst (List.hd nodes), Lexer.eof) nodes [] *)
	List.map (mapper state) nodes

let parse_access_restricted f state node : 'a access_restricted =
	match node with
	| (bnds, Pub inner) ->
		ARPub (bnds, (f state inner))
	| (bnds, Priv inner) ->
		ARPriv (bnds, (f state inner))
	| (bnds, Stat inner) ->
		ARStat (bnds, (f state inner))
	| (bnds, n) ->
		ARPriv (bnds, (f state (bnds, n)))

let wrap_ctx ctx state f =
	let state' = Core_ast.push_ctx ctx state in
	let res = f state' in
	let state'' = Core_ast.pop_ctx state' in
	if state <> state'' then
		raise (InternalError "scopes do not cleanly resolve.")
	else res

let rec parse_root allow_root_expr root : prog =
	let parse file state node =
		if allow_root_expr then
			parse_any file state node
		else
			match node with
			| (_, Use _) -> UStmt (parse_stmt file state node)
			| _ -> UDecl (parse_decl file state node)
	in
	let state = Core_ast.default_state in
	let (_, root) = root in
	match root with
	| Program blocks -> (
		let rec loop state (stack : (string * (fat_node list)) list) acc =
			match stack with
			| [] -> acc
			| prog::t -> (
				let (file, block) = prog in
				let rec loop_acc state stack acc =
					match stack with
					| [] -> acc
					| h::t ->
						let code = parse file state h in
						loop_acc state t (code::acc)
				in
				loop state t (loop_acc state block acc)
			)
		in
		loop state blocks []
	)
	| _ -> raise (InternalError "expected a program.")

and parse_any file state node : code =
	match node with
	| (_, t) when is_decl t ->
		let decl = parse_decl file state node in
		UDecl decl
	| (_, t) when is_stmt t ->
		let stmt = parse_stmt file state node in
		UStmt stmt
	| n ->
		let expr = parse_expr file state n in
		UExpr expr

and parse_decl file state node : decl =
	match node with
	| (_, Program _) -> raise (InternalError (Printf.sprintf "nested programs detected when parsing '%s'." file))
	| (bnds, Variable {name;type_;value}) ->
		let var = parse_variable file bnds state name type_ value in
		DVariable var
	| (bnds, Type {name; members}) ->
		let t = parse_type file bnds state name members in
		DType t
	| (bnds, Alias {name;annotation}) ->
		let alias = parse_alias file bnds state name annotation in
		DAlias alias
	| (bnds, Function {name; params; result; code}) ->
		let f = parse_function file bnds state name params result code in
		DFunction f
	| (bnds, OperatorOverload {name; params; result; code}) ->
		let oo = parse_oper_overload file bnds state name params result code in
		DOperOverload oo
	| (bnds, Enum {name; members}) ->
		let enum = parse_enum file bnds state name members in
		DEnum enum
	| (bnds, Space {name; members}) ->
		let space = parse_space file bnds state name members in
		DSpace space
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a declaration."))

and parse_stmt file state node : stmt =
	match node with
	| (bnds, Use id) ->
		let use = parse_use file bnds state id in
		SUnresolvedUse use
	| (bnds, StandardBreak) ->
		SStandardBreak bnds
	| (bnds, YieldingBreak expr) ->
		let brk = parse_yielding_break file bnds state expr in
		SYieldingBreak brk
	| (bnds, Continue) ->
		SContinue bnds
	| (bnds, StandardFor {init; condition; iter; block}) ->
		let for_ = parse_standard_for file bnds state init condition iter block in
		SFor for_
	| (bnds, StandardUntil {condition; block}) ->
		let until = parse_standard_until file bnds state condition block in
		SUntil until
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a statement."))

and parse_expr file state node : expr =
	match node with
	| (bnds, Lambda {params; result; code}) ->
		let lambda = parse_lambda file bnds state params result code in
		ELambda lambda
	| (bnds, If {condition; true_block; false_block}) ->
		let i = parse_if file bnds state condition true_block false_block in
		EIf i
	| (bnds, Match {switcher; cases}) ->
		let m = parse_match file bnds state switcher cases in
		EMatch m
	| (bnds, BreakingFor {init; iter; block}) ->
		let f = parse_breaking_for file bnds state init iter block in
		EFor f
	| (bnds, BreakingUntil block) ->
		let u = parse_breaking_until file bnds state block in
		EUntil u
	| (bnds, Block block) ->
		let b = parse_block file bnds state block in
		EBlock b
	| (bnds, Binary {left; op; right}) ->
		let b = parse_binary file bnds state left op right in
		EBinary b
	| (bnds, Unary {op; arg}) ->
		let u = parse_unary file bnds state op arg in
		EUnary u
	| (bnds, Reference id) ->
		EUnresolvedReference (bnds, id)
	| (bnds, Call {from; args}) ->
		let c = parse_call file bnds state from args in
		ECall c
	| (bnds, Index {from; args}) ->
		let i = parse_index file bnds state from args in
		EIndex i
	| (bnds, Initializer {type_; args}) ->
		let i = parse_initializer file bnds state type_ args in
		EInitializer i
	| (bnds, ArrayInitializer {size; type_; args}) ->
		let ai = parse_array_initializer file bnds state size type_ args in
		EArrayInitializer ai
	| (bnds, TupleExpr exprs) ->
		let te = parse_tuple_expr file bnds state exprs in
		ETuple te
	| (bnds, Integer t) ->
		let i = parse_literal file bnds state t in
		ELiteral i
	| (bnds, Float t) ->
		let f = parse_literal file bnds state t in
		ELiteral f
	| (bnds, Character t) ->
		let c = parse_literal file bnds state t in
		ELiteral c
	| (bnds, String t) ->
		let s = parse_literal file bnds state t in
		ELiteral s
	| (bnds, True t) ->
		let t = parse_literal file bnds state t in
		ELiteral t
	| (bnds, False f) ->
		let f = parse_literal file bnds state f in
		ELiteral f
	| (bnds, _) -> raise (FatalError (file, bnds, "expected an expression."))

and parse_annotation file state node : annotation =
	match node with
	| (bnds, Enum {members; _}) ->
		let a = parse_nameless_enum file bnds state members in
		ANamelessEnum a
	| (bnds, Type {members; _}) ->
		let a = parse_nameless_type file bnds state members in
		ANamelessType a
	| (bnds, Tuple members) ->
		let a = parse_tuple file bnds state members in
		ATuple a
	| (bnds, AnonymousEnum members) ->
		let a = parse_anonymous_enum file bnds state members in
		AAnonymousEnum a
	| (bnds, FunctionType {params; result}) ->
		let a = parse_function_type file bnds state params result in
		AFunction a
	| (bnds, Reference id) ->
		AUnresolvedReference (bnds, id)
	| (bnds, Variadic inner) ->
		let a = parse_variadic file bnds state inner in
		AVariadic a
	| (bnds, Pointer inner) ->
		let a = parse_pointer file bnds state inner in
		APointer a
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a type annotation."))

and parse_variable file bnds state name type_ value =
	let type_ref =
		match type_ with
		| Some type_ ->
			let type_ref = parse_annotation file state type_ in
			Some type_ref
		| None -> None
	in

	let value =
		match value with
		| Some value ->
			let value = parse_expr file state value in
			Some value
		| None -> None
	in

	if Option.is_none value && Option.is_none type_ref then
		raise (FatalError (file, bnds, (Printf.sprintf "variable '%s' requires either an initial value or an explicit type annotation." name)))
	else
		{
			bounds = bnds;
			name = name;
			type_ref = type_ref;
			value = value
		}

and parse_type file bnds state name members = {
	bounds = bnds;
	name = name;
	members = (parse_nameless_type file bnds state members);
}

and parse_alias file bnds state name value =
	let value =
		match value with
		| (_, Space {members; _}) ->
			let members = parse_nameless_space file state members in
			ASpace members
		| (bnds, Import {path}) ->
			AImport (bnds, path)
		| v ->
			let annotation = parse_annotation file state v in
			AAnnotation annotation
	in
	{
		bounds = bnds;
		name = name;
		value = value
	}

and parse_function file bnds state name params result code =
	let params =
		wrap_ctx CFunctionParams state
			(map_nodes
				(fun state h ->
					match h with
					| (bnds, Variable {name; type_; value}) ->
						parse_variable file bnds state name type_ value
					| (bnds, _) -> raise (FatalError (file, bnds, "expected a parameter."))
				) params)
	in

	let result =
		match result with
		| Some result ->
			let result = parse_annotation file state result in
			Some result
		| None -> None
	in

	let code =
		wrap_ctx CFunction state
			(map_nodes (parse_any file) code)
	in

	{
		bounds = bnds;
		name = name;
		params = params;
		return_type = result;
		code = code
	}

and parse_oper_overload file bnds state op params result code =
	let op =
		match op with
		| Token.OpenBracket
		| Token.ClosedBracket
		| Token.OpenParen
		| Token.ClosedParen -> gen_op op
		| _ -> (
			match params with
			| [_] ->
				Unary (gen_un_op op)
			| [_;_] ->
				Binary (gen_bin_op op)
			| [] -> raise (FatalError (file, bnds, "operator overload has no parameters"))
			| _ -> raise (FatalError (file, bnds, "operator overload has more than 2 parameters."))
		)
	in

	let params =
		wrap_ctx CFunctionParams state
			(map_nodes
				(fun state h ->
					match h with
					| (bnds, Variable {name; type_; value}) ->
						parse_variable file bnds state name type_ value
					| (bnds, _) -> raise (FatalError (file, bnds, "expected a parameter."))
				) params)
	in

	let result =
		match result with
		| Some result ->
			let result = parse_annotation file state result in
			Some result
		| None -> None
	in

	let code =
		wrap_ctx CFunction state
			(map_nodes (parse_any file) code)
	in

	{
		bounds = bnds;
		op = op;
		params = params;
		return_type = result;
		code = code
	}

and parse_enum file bnds state name members = {
	bounds = bnds;
	name = name;
	members = (parse_nameless_enum file bnds state members)
}

and parse_space file bnds state name members = {
	bounds = bnds;
	name = name;
	members = (parse_nameless_space file state members)
}

and parse_nameless_space file state members =
	wrap_ctx CSpace state
		(map_nodes
			(parse_access_restricted (parse_decl file))
			members)

and parse_use _file bnds _state id = bnds, id

and parse_lambda file bnds state params result code =
	let params =
		wrap_ctx CFunctionParams state
			(map_nodes
				(fun state h ->
					match h with
					| (bnds, Variable {name; type_; value}) ->
						parse_variable file bnds state name type_ value
					| (bnds, _) -> raise (FatalError (file, bnds, "expected parameter."))
				) params)
	in

	let result =
		match result with
		| Some result ->
			let result = parse_annotation file state result in
			Some result
		| None -> None
	in

	let code =
		wrap_ctx CFunction state
			(map_nodes
				(parse_any file)
				code)
	in

	{
		bounds = bnds;
		params = params;
		return_type = result;
		code = code
	}

and parse_if file bnds state condition true_block false_block =
	let condition = parse_expr file state condition in
	let true_block =
		wrap_ctx CIf state
			(map_nodes
				(parse_any file)
				true_block)
	in
	let false_block =
		match false_block with
		| Some false_block ->
			wrap_ctx CElse state
				(map_nodes
					(parse_any file)
					false_block)
		| None -> []
	in
	{
		bounds = bnds;
		condition = condition;
		true_block = true_block;
		false_block = false_block
	}

and parse_match file bnds state switcher cases =
	let switcher = parse_expr file state switcher in
	let cases =
		wrap_ctx CMatch state
			(map_nodes
				(fun state h ->
					match h with
					| (bnds, MatchCase {pattern; guard; logic}) ->
						parse_match_branch file bnds state pattern guard logic
					| (bnds, _) -> raise (FatalError (file, bnds, "expected a match branch."))
				) cases)
	in
	{
		bounds = bnds;
		switcher = switcher;
		logic = cases
	}

and parse_breaking_for file bnds state init iter block =
	let init =
		wrap_ctx CBFor state
			(map_nodes
				(parse_any file)
				init)

	in
	let iter =
		wrap_ctx CBFor state
			(map_nodes
				(parse_any file)
				iter)
	in
	let code =
		wrap_ctx CBFor state
			(map_nodes
				(parse_any file)
				block)
	in
	{
		bounds = bnds;
		init = init;
		iter = iter;
		code = code;
	}

and parse_breaking_until file bnds state code =
	bnds, wrap_ctx CBUntil state
		(map_nodes
			(parse_any file)
			code)

and parse_standard_for file bnds state init condition iter code =
	let init =
		wrap_ctx CSFor state
			(map_nodes
				(parse_any file)
				init)
	in
	let condition =
		wrap_ctx CSFor state
			(fun s -> parse_expr file s condition)
	in
	let iter =
		wrap_ctx CSFor state
			(map_nodes
				(parse_any file)
				iter)
	in
	let code =
		wrap_ctx CSFor state
			(map_nodes
				(parse_any file)
				code)
	in
	{
		bounds = bnds;
		init = init;
		condition = condition;
		iter = iter;
		code = code;
	}

and parse_standard_until file bnds state condition code =
	let condition =
		wrap_ctx CSUntil state
			(fun s -> parse_expr file s condition)
	in
	let code =
		wrap_ctx CSUntil state
			(map_nodes
				(parse_any file)
				code)
	in
	{
		bounds = bnds;
		condition = condition;
		code = code;
	}

and parse_block file bnds state block =
	bnds, wrap_ctx CBlock state
		(map_nodes
			(parse_any file)
			block)

and parse_yielding_break file bnds state expr =
	if Core_ast.in_ctx CBFor state || Core_ast.in_ctx CBUntil state then (
		bnds, (parse_expr file state expr)
	) else (
		raise (FatalError (file, bnds, "cannot use a yielding break in a non-breaking loop context."))
	)

and parse_binary file bnds state left op right =
	let left = parse_expr file state left in
	let op = gen_bin_op op in
	let right =
		match right with
		| Some right -> parse_expr file state right
		| None ->
			raise (InternalError "binary operator requires a right-side argument.")
	in
	{
		bounds = bnds;
		left = left;
		op = op;
		right = right;
	}

and parse_unary file bnds state op arg =
	let op = gen_un_op op in
	let arg =
		match arg with
		| Some arg -> parse_expr file state arg
		| None -> raise (InternalError "unary operator requires one argument.")
	in
	{
		bounds = bnds;
		op = op;
		arg = arg;
	}

and parse_call file bnds state from args =
	let from = parse_expr file state from in
	let args =
		map_nodes
			(parse_expr file)
			args state
	in
	{
		bounds = bnds;
		from = from;
		args = args;
	}

and parse_index file bnds state from args =
	let from = parse_expr file state from in
	let args =
		map_nodes
			(parse_expr file)
			args state
	in
	{
		bounds = bnds;
		from = from;
		args = args;
	}

and parse_initializer file bnds state type_ args =
	let type_ =
		match type_ with
		| (bnds, Reference id) -> AUnresolvedReference (bnds, id)
		| (bnds, _) -> raise (FatalError (file, bnds, "expected a reference to a type."))
	in
	let args =
		wrap_ctx CInitializer state
			(map_nodes
				(fun state n ->
					match n with
					| (_, MemberAssignment {name; value}) ->
						name, (parse_expr file state value)
					| (bnds, _) -> raise (FatalError (file, bnds, "invalid type initializer member assignment."))
				) args)
	in
	{
		bounds = bnds;
		type_ref = type_;
		args = args;
	}

and parse_array_initializer file bnds state size type_ args =
	let size =
		match size with
		| Some size ->
			let size = parse_expr file state size in
			Some size
		| None -> None
	in
	let type_ = parse_annotation file state type_ in
	let args =
		wrap_ctx CArrayInitializer state
			(map_nodes
				(parse_expr file)
				args)
	in
	{
		bounds = bnds;
		size = size;
		type_ref = type_;
		elements = args;
	}

and parse_tuple_expr file _bnds state exprs =
	map_nodes
		(parse_expr file)
		exprs state

and parse_literal _file bnds _state t =
	match t.token with
	| Token.Integer i -> LInteger (bnds, i)
	| Token.Float i -> LFloat (bnds, i)
	| Token.Character i -> LCharacter (bnds, i)
	| Token.String i -> LString (bnds, i)
	| Token.True -> LTrue bnds
	| Token.False -> LFalse bnds
	| _ -> raise (InternalError "expected literal.")

and parse_pointer file _bnds state inner =
	parse_annotation file state inner

and parse_tuple file _bnds state members =
	map_nodes
		(parse_annotation file)
		members state

and parse_anonymous_enum file _bnds state members =
	wrap_ctx CAnonymousEnum state
		(map_nodes
			(fun state h ->
				match h with
				| (_, EnumVariant {type_; _}) ->
					let type_ = parse_annotation file state type_ in
					AEMVariant type_
				| (bnds, Function {name; params; result; code}) ->
					let f = parse_function file bnds state name params result code in
					AEMFunction (ARPriv (bnds, f))
				| (am_bnds, Priv (bnds, Function {name; params; result; code})) ->
					let f = parse_function file bnds state name params result code in
					AEMFunction (ARPriv ((fst am_bnds, snd f.bounds), f))
				| (am_bnds, Pub (bnds, Function {name; params; result; code})) ->
					let f = parse_function file bnds state name params result code in
					AEMFunction (ARPub ((fst am_bnds, snd f.bounds), f))
				| (am_bnds, Stat (bnds, Function {name; params; result; code})) ->
					let f = parse_function file bnds state name params result code in
					AEMFunction (ARStat ((fst am_bnds, snd f.bounds), f))
				| n ->
					let a = parse_annotation file state n in
					AEMVariant a
			) members)

and parse_function_type file bnds state params return_type =
	let params =
		wrap_ctx CFunctionParams state
			(map_nodes
				(fun state p ->
					match p with
					| (bnds, Variable {type_; _}) -> (
						match type_ with
						| Some type_ -> parse_annotation file state type_
						| None -> raise (FatalError (file, bnds, "expected parameter with explicit type."))
					)
					| n -> parse_annotation file state n
				)
				params)
	in
	let return_type =
		match return_type with
		| Some return_type -> Some (parse_annotation file state return_type)
		| None -> None
	in
	{
		bounds = bnds;
		params = params;
		return_type = return_type;
	}

and parse_variadic file _bnds state inner =
	parse_annotation file state inner

and parse_nameless_enum file _bnds state members =
	wrap_ctx CEnum state
		(map_nodes
			(fun state h ->
				match h with
				| (bnds, EnumVariant {name; type_}) ->
					let type_ = parse_annotation file state type_ in
					EMVariant (bnds, (name, type_))
				| nd ->
					let f =
						parse_access_restricted (fun state nd ->
							match nd with
							| (bnds, Function {name; params; result; code}) ->
								(* parse_access_restricted (fun s _ -> parse_function file s name params result code) state nd *)
								parse_function file bnds state name params result code
							| (bnds, _) ->
								raise (FatalError (file, bnds, "expected a (potentially access restricted) method."))
						) state nd
					in
					EMFunction f
			) members)

and parse_nameless_type file _bnds state members =
	wrap_ctx CType state
		(map_nodes
			(parse_access_restricted (parse_decl file))
			members)

and parse_match_branch file bnds state pattern guard logic =
	let pattern = parse_pattern file state pattern guard in
	let branch =
		match logic with
		| None -> []
		| Some logic -> (
			let f =
				match logic with
				| (_, Block code) ->
					map_nodes
						(parse_any file)
						code
				| n ->
					fun s ->
						let n = parse_any file s n in
						[n]
			in
			wrap_ctx CMatchBranch state f
		)
	in
	{
		bounds = bnds;
		pattern = pattern;
		branch = branch;
	}

and parse_pattern file state pattern guard =
	match guard with
	| Some guard -> PGuard ((fst (fst pattern), snd (fst guard)), (parse_pattern_expr file state pattern, parse_expr file state guard))
	| None -> PStandard (parse_pattern_expr file state pattern)

and parse_pattern_expr file state pattern : pattern_expr =
	match pattern with
	| (bnds, Reference (_, ["_"])) -> PWildcard bnds
	| (bnds, Reference (_, [v])) -> PBind (bnds, v)
	| (bnds, Integer t) -> PLiteral (parse_literal file bnds state t)
	| (bnds, Float t) -> PLiteral (parse_literal file bnds state t)
	| (bnds, Character t) -> PLiteral (parse_literal file bnds state t)
	| (bnds, String t) -> PLiteral (parse_literal file bnds state t)
	| (bnds, True t) -> PLiteral (parse_literal file bnds state t)
	| (bnds, False t) -> PLiteral (parse_literal file bnds state t)
	| (_, TupleExpr m) ->
		PTuple (map_nodes
			(parse_pattern_expr file)
			m state)
	| (bnds, Call {from; args}) ->
		let ref =
			match from with
			| (_, Reference ref) -> ref
			| (bnds, _) -> raise (FatalError (file, bnds, "expected an enum constructor."))
		in
		let args =
			map_nodes
				(parse_pattern_expr file)
				args state
		in
		PUnresolvedConstructor (bnds, (ref, CTuple args))
	| (bnds, Initializer {type_; args}) ->
		let ref =
			match type_ with
			| (_, Reference ref) -> ref
			| (bnds, _) -> raise (FatalError (file, bnds, "expected an enum constructor."))
		in
		let args =
			map_nodes
				(fun state h ->
					match h with
					| (bnds, MemberAssignment {name; value}) ->
						IPExpect (bnds, (name, parse_pattern_expr file state value))
					| (bnds, Reference (false, [name])) ->
						IPBind (bnds, name)
					| (bnds, _) -> raise (FatalError (file, bnds, "expected a type field binding or an expected value."))
				)
				args state
		in
		PUnresolvedConstructor (bnds, (ref, CInitializer args))
	| (bnds, Binary {left; op; right}) when op = Token.ExclusiveRange ->
		let left = parse_expr file state left in
		let right =
			match right with
			| Some right -> parse_expr file state right
			| None -> raise (InternalError "binary operator not given a right operand.")
		in
		PExclusiveRange (bnds, (left, right))
	| (bnds, Binary {left; op; right}) when op = Token.InclusiveRange ->
		let left = parse_expr file state left in
		let right =
			match right with
			| Some right -> parse_expr file state right
			| None -> raise (InternalError "binary operator not given a right operand.")
		in
		PInclusiveRange (bnds, (left, right))
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a pattern expression."))
