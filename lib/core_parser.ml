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
	List.rev_map (mapper state) nodes |> List.rev

let parse_access_restricted f state node : 'a access_restricted =
	match node with
	| (_, Pub inner) ->
		ARPub (f state inner)
	| (_, Priv inner) ->
		ARPriv (f state inner)
	| (_, Stat inner) ->
		ARStat (f state inner)
	| n ->
		ARPriv (f state n)

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
		let var = parse_variable bnds file state name type_ value in
		DVariable var
	| (_, Type {name; members}) ->
		let t = parse_type file state name members in
		DType t
	| (_, Alias {name;annotation}) ->
		let alias = parse_alias file state name annotation in
		DAlias alias
	| (_, Function {name; params; result; code}) ->
		let f = parse_function file state name params result code in
		DFunction f
	| (bnds, OperatorOverload {name; params; result; code}) ->
		let oo = parse_oper_overload bnds file state name params result code in
		DOperOverload oo
	| (_, Enum {name; members}) ->
		let enum = parse_enum file state name members in
		DEnum enum
	| (_, Space {name; members}) ->
		let space = parse_space file state name members in
		DSpace space
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a declaration."))

and parse_stmt file state node : stmt =
	match node with
	| (_, Use id) ->
		let use = parse_use file state id in
		SUnresolvedUse use
	| (_, StandardBreak) ->
		SStandardBreak
	| (bnds, YieldingBreak expr) ->
		let brk = parse_yielding_break bnds file state expr in
		SYieldingBreak brk
	| (_, Continue) ->
		SContinue
	| (_, StandardFor {init; condition; iter; block}) ->
		let for_ = parse_standard_for file state init condition iter block in
		SFor for_
	| (_, StandardUntil {condition; block}) ->
		let until = parse_standard_until file state condition block in
		SUntil until
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a statement."))

and parse_expr file state node : expr =
	match node with
	| (_, Lambda {params; result; code}) ->
		let lambda = parse_lambda file state params result code in
		ELambda lambda
	| (_, If {condition; true_block; false_block}) ->
		let i = parse_if file state condition true_block false_block in
		EIf i
	| (_, Match {switcher; cases}) ->
		let m = parse_match file state switcher cases in
		EMatch m
	| (_, BreakingFor {init; iter; block}) ->
		let f = parse_breaking_for file state init iter block in
		EFor f
	| (_, BreakingUntil block) ->
		let u = parse_breaking_until file state block in
		EUntil u
	| (_, Block block) ->
		let b = parse_block file state block in
		EBlock b
	| (_, Binary {left; op; right}) ->
		let b = parse_binary file state left op right in
		EBinary b
	| (_, Unary {op; arg}) ->
		let u = parse_unary file state op arg in
		EUnary u
	| (_, Reference id) ->
		EUnresolvedReference id
	| (_, Call {from; args}) ->
		let c = parse_call file state from args in
		ECall c
	| (_, Index {from; args}) ->
		let i = parse_index file state from args in
		EIndex i
	| (_, Initializer {type_; args}) ->
		let i = parse_initializer file state type_ args in
		EInitializer i
	| (_, ArrayInitializer {size; type_; args}) ->
		let ai = parse_array_initializer file state size type_ args in
		EArrayInitializer ai
	| (_, TupleExpr exprs) ->
		let te = parse_tuple_expr file state exprs in
		ETuple te
	| (_, Integer t) ->
		let i = parse_literal file state t in
		ELiteral i
	| (_, Float t) ->
		let f = parse_literal file state t in
		ELiteral f
	| (_, Character t) ->
		let c = parse_literal file state t in
		ELiteral c
	| (_, String t) ->
		let s = parse_literal file state t in
		ELiteral s
	| (_, True t) ->
		let t = parse_literal file state t in
		ELiteral t
	| (_, False f) ->
		let f = parse_literal file state f in
		ELiteral f
	| (bnds, _) -> raise (FatalError (file, bnds, "expected an expression."))

and parse_annotation file state node : annotation =
	match node with
	| (_, Enum {members; _}) ->
		let a = parse_nameless_enum file state members in
		ANamelessEnum a
	| (_, Type {members; _}) ->
		let a = parse_nameless_type file state members in
		ANamelessType a
	| (_, Tuple members) ->
		let a = parse_tuple file state members in
		ATuple a
	| (_, AnonymousEnum members) ->
		let a = parse_anonymous_enum file state members in
		AAnonymousEnum a
	| (_, FunctionType {params; result}) ->
		let a = parse_function_type file state params result in
		AFunction a
	| (_, Reference id) ->
		AUnresolvedReference id
	| (_, Variadic inner) ->
		let a = parse_variadic file state inner in
		AVariadic a
	| (_, Pointer inner) ->
		let a = parse_pointer file state inner in
		APointer a
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a type annotation."))

and parse_variable bnds file state name type_ value =
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
			name = name;
			type_ref = type_ref;
			value = value
		}

and parse_type file state name members = {
	name = name;
	members = (parse_nameless_type file state members);
}

and parse_alias file state name value =
	let value =
		match value with
		| (_, Space {members; _}) ->
			let members = parse_nameless_space file state members in
			ASpace members
		| (_, Import {path}) ->
			AImport path
		| v ->
			let annotation = parse_annotation file state v in
			AAnnotation annotation
	in
	{
		name = name;
		value = value
	}

and parse_function file state name params result code =
	let params =
		wrap_ctx CFunctionParams state
			(map_nodes
				(fun state h ->
					match h with
					| (bnds, Variable {name; type_; value}) ->
						parse_variable bnds file state name type_ value
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
		name = name;
		params = params;
		return_type = result;
		code = code
	}

and parse_oper_overload bnds file state op params result code =
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
						parse_variable bnds file state name type_ value
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
		op = op;
		params = params;
		return_type = result;
		code = code
	}

and parse_enum file state name members = {
	name = name;
	members = (parse_nameless_enum file state members)
}

and parse_space file state name members = {
	name = name;
	members = (parse_nameless_space file state members)
}

and parse_nameless_space file state members =
	wrap_ctx CSpace state
		(map_nodes
			(parse_access_restricted (parse_decl file))
			members)

and parse_use _file _state id = id

and parse_lambda file state params result code =
	let params =
		wrap_ctx CFunctionParams state
			(map_nodes
				(fun state h ->
					match h with
					| (bnds, Variable {name; type_; value}) ->
						parse_variable bnds file state name type_ value
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
		params = params;
		return_type = result;
		code = code
	}

and parse_if file state condition true_block false_block =
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
		condition = condition;
		true_block = true_block;
		false_block = false_block
	}

and parse_match file state switcher cases =
	let switcher = parse_expr file state switcher in
	let cases =
		wrap_ctx CMatch state
			(map_nodes
				(fun state h ->
					match h with
					| (_, MatchCase {pattern; guard; logic}) ->
						parse_match_branch file state pattern guard logic
					| (bnds, _) -> raise (FatalError (file, bnds, "expected a match branch."))
				) cases)
	in
	{
		switcher = switcher;
		logic = cases
	}

and parse_breaking_for file state init iter block =
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
		init = init;
		iter = iter;
		code = code;
	}

and parse_breaking_until file state code =
	wrap_ctx CBUntil state
		(map_nodes
			(parse_any file)
			code)

and parse_standard_for file state init condition iter code =
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
		init = init;
		condition = condition;
		iter = iter;
		code = code;
	}

and parse_standard_until file state condition code =
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
		condition = condition;
		code = code;
	}

and parse_block file state block =
	wrap_ctx CBlock state
		(map_nodes
			(parse_any file)
			block)

and parse_yielding_break bnds file state expr =
	if Core_ast.in_ctx CBFor state || Core_ast.in_ctx CBUntil state then (
		parse_expr file state expr
	) else (
		raise (FatalError (file, bnds, "cannot use a yielding break in a non-breaking loop context."))
	)

and parse_binary file state left op right =
	let left = parse_expr file state left in
	let op = gen_bin_op op in
	let right =
		match right with
		| Some right -> parse_expr file state right
		| None ->
			raise (InternalError "binary operator requires a right-side argument.")
	in
	{
		left = left;
		op = op;
		right = right;
	}

and parse_unary file state op arg =
	let op = gen_un_op op in
	let arg =
		match arg with
		| Some arg -> parse_expr file state arg
		| None -> raise (InternalError "unary operator requires one argument.")
	in
	{
		op = op;
		arg = arg;
	}

and parse_call file state from args =
	let from = parse_expr file state from in
	let args =
		map_nodes
			(parse_expr file)
			args state
	in
	{
		from = from;
		args = args;
	}

and parse_index file state from args =
	let from = parse_expr file state from in
	let args =
		map_nodes
			(parse_expr file)
			args state
	in
	{
		from = from;
		args = args;
	}

and parse_initializer file state type_ args =
	let type_ =
		match type_ with
		| (_, Reference id) -> AUnresolvedReference id
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
		type_ref = type_;
		args = args;
	}

and parse_array_initializer file state size type_ args =
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
		size = size;
		type_ref = type_;
		elements = args;
	}

and parse_tuple_expr file state exprs =
	map_nodes
		(parse_expr file)
		exprs state

and parse_literal _file _state t =
	match t.token with
	| Token.Integer i -> LInteger i
	| Token.Float i -> LFloat i
	| Token.Character i -> LCharacter i
	| Token.String i -> LString i
	| Token.True -> LTrue
	| Token.False -> LFalse
	| _ -> raise (InternalError "expected literal.")

and parse_pointer file state inner =
	parse_annotation file state inner

and parse_tuple file state members =
	map_nodes
		(parse_annotation file)
		members state

and parse_anonymous_enum file state members =
	wrap_ctx CAnonymousEnum state
		(map_nodes
			(fun state h ->
				match h with
				| (_, EnumVariant {type_; _}) ->
					let type_ = parse_annotation file state type_ in
					AEMVariant type_
				| (_, Function {name; params; result; code}) ->
					let f = parse_function file state name params result code in
					AEMFunction (ARPriv f)
				| (_, Priv (_, Function {name; params; result; code})) ->
					let f = parse_function file state name params result code in
					AEMFunction (ARPriv f)
				| (_, Pub (_, Function {name; params; result; code})) ->
					let f = parse_function file state name params result code in
					AEMFunction (ARPub f)
				| (_, Stat (_, Function {name; params; result; code})) ->
					let f = parse_function file state name params result code in
					AEMFunction (ARStat f)
				| n ->
					let a = parse_annotation file state n in
					AEMVariant a
			) members)

and parse_function_type file state params return_type =
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
		params = params;
		return_type = return_type;
	}

and parse_variadic file state inner =
	parse_annotation file state inner

and parse_nameless_enum file state members =
	wrap_ctx CEnum state
		(map_nodes
			(fun state h ->
				match h with
				| (_, EnumVariant {name; type_}) ->
					let type_ = parse_annotation file state type_ in
					EMVariant (name, type_)
				| nd ->
					let f =
						parse_access_restricted (fun state nd ->
							match nd with
							| (_, Function {name; params; result; code}) ->
								(* parse_access_restricted (fun s _ -> parse_function file s name params result code) state nd *)
								parse_function file state name params result code
							| (bnds, _) ->
								raise (FatalError (file, bnds, "expected a (potentially access restricted) method."))
						) state nd
					in
					EMFunction f
			) members)

and parse_nameless_type file state members =
	wrap_ctx CType state
		(map_nodes
			(parse_access_restricted (parse_decl file))
			members)

and parse_match_branch file state pattern guard logic =
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
		pattern = pattern;
		branch = branch;
	}

and parse_pattern file state pattern guard =
	match guard with
	| Some guard -> PGuard (parse_pattern_expr file state pattern, parse_expr file state guard)
	| None -> PStandard (parse_pattern_expr file state pattern)

and parse_pattern_expr file state pattern =
	match pattern with
	| (_, Reference (_, ["_"])) -> PWildcard
	| (_, Reference (_, [v])) -> PBind v
	| (_, Integer t) -> PLiteral (parse_literal file state t)
	| (_, Float t) -> PLiteral (parse_literal file state t)
	| (_, Character t) -> PLiteral (parse_literal file state t)
	| (_, String t) -> PLiteral (parse_literal file state t)
	| (_, True t) -> PLiteral (parse_literal file state t)
	| (_, False t) -> PLiteral (parse_literal file state t)
	| (_, TupleExpr m) ->
		PTuple (map_nodes
			(parse_pattern_expr file)
			m state)
	| (_, Call {from; args}) ->
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
		PUnresolvedConstructor (ref, CTuple args)
	| (_, Initializer {type_; args}) ->
		let ref =
			match type_ with
			| (_, Reference ref) -> ref
			| (bnds, _) -> raise (FatalError (file, bnds, "expected an enum constructor."))
		in
		let args =
			map_nodes
				(fun state h ->
					match h with
					| (_, MemberAssignment {name; value}) ->
						IPExpect (name, parse_pattern_expr file state value)
					| (_, Reference (false, [name])) ->
						IPBind name
					| (bnds, _) -> raise (FatalError (file, bnds, "expected a type field binding or an expected value."))
				)
				args state
		in
		PUnresolvedConstructor (ref, CInitializer args)
	| (_, Binary {left; op; right}) when op = Token.ExclusiveRange ->
		let left = parse_expr file state left in
		let right =
			match right with
			| Some right -> parse_expr file state right
			| None -> raise (InternalError "binary operator not given a right operand.")
		in
		PExclusiveRange (left, right)
	| (_, Binary {left; op; right}) when op = Token.InclusiveRange ->
		let left = parse_expr file state left in
		let right =
			match right with
			| Some right -> parse_expr file state right
			| None -> raise (InternalError "binary operator not given a right operand.")
		in
		PInclusiveRange (left, right)
	| (bnds, _) -> raise (FatalError (file, bnds, "expected a pattern expression."))
