module Rga = Registered_ast
module Rsa = Resolved_ast
module SymbolTable = Registered_parser.SymbolTable

(*
	TODO:
	1) reference resolution
	2) annotation resolution (!important)
*)

let f16_id = -100
let f32_id = -101
let f64_id = -102
let f128_id = -103
let u8_id = -104
let bool_id = -106
let void_id = -200

let create_primitives (table : Registered_parser.symbol_table) =
	(* does not create integer types since those will be variable width/dynamically created *)
	(* f16, f32, f64, f128 *)
	(* u8 -> for characters *)
	(* bool *)
	let primitives = [
		(f16_id, "f16");
		(f32_id, "f32");
		(f64_id, "f64");
		(f128_id, "f128");
		(u8_id, "u8");
		(bool_id, "bool");
		(void_id, "");
	] in

	List.iter
		(fun (id, name) ->
			let root = SymbolTable.find table.symbols (-1) in
			SymbolTable.replace table.symbols (-1) { root with next = (id::(root.next)) };
			SymbolTable.add table.symbols id {
				name = name;
				parent = -1;
				next = [];
			}
		) primitives;
	table

let type_of_expr = function
	| Rsa.ELambda (_, type_ref) -> type_ref
	| Rsa.EIf (_, type_ref) -> type_ref
	| Rsa.EMatch (_, type_ref) -> type_ref
	| Rsa.EFor (_, type_ref) -> type_ref
	| Rsa.EUntil (_, type_ref) -> type_ref
	| Rsa.EBlock (_, type_ref) -> type_ref
	| Rsa.EBinary (_, type_ref) -> type_ref
	| Rsa.EUnary (_, type_ref) -> type_ref
	| Rsa.EResolvedReference (_, type_ref) -> type_ref
	| Rsa.ECall (_, type_ref) -> type_ref
	| Rsa.EIndex (_, type_ref) -> type_ref
	| Rsa.EInitializer (_, type_ref) -> type_ref
	| Rsa.EArrayInitializer (_, type_ref) -> type_ref
	| Rsa.ETuple (_, type_ref) -> type_ref
	| Rsa.ELiteral (_, type_ref) -> type_ref

let type_of_code _root _table _code =
	failwith "TODO"

let type_of_code_list _root _table _code =
	failwith "TODO"

let type_of_match_branch_list _root _table _code =
	failwith "TODO"

let type_of_breaking_list _root _table _code =
	failwith "TODO"

let common_type_of _root _table _a _b =
	failwith "TODO"

let type_of_binary _root _table _a _op _b =
	failwith "TODO"

let type_of_unary _root _table _a _op =
	failwith "TODO"

let type_of_call _root _table _from _args =
	failwith "TODO"

let type_of_index _root _table _from _args =
	failwith "TODO"

let type_of_initializer _root _table _type_ref _args =
	failwith "TODO"

let type_of_array_initializer _root _table _type_ref =
	failwith "TODO"

let type_of_tuple _root _table _elements =
	failwith "TODO"

let resolve_non_primitive_id _root _table (_from_root, _path) =
	failwith "TODO"

let resolve_primitive _table _signed _width =
	failwith "TODO"

let resolve_id root table (from_root, path) =
	match path with
	| [] -> failwith "impossible"
	| [h] -> (
		match h with
		| "f16" -> f16_id
		| "f32" -> f32_id
		| "f64" -> f64_id
		| "f128" -> f128_id
		| "u8" -> u8_id
		| "bool" -> bool_id
		| h -> (
			if String.get h 0 = 'i' || String.get h 0 = 'u' then (
				let rec parse_width i width =
					if String.length h >= i then
						if i = 1 then (-1) else width
					else
						match int_of_char (String.get h i) with
						| c when c >= (int_of_char '0') && c <= (int_of_char '9') ->
							parse_width (i+1) ((width*10) + c)
						| _ -> (-1)
				in
				let width = parse_width 1 0 in
				if width = (-1) then
					resolve_non_primitive_id root table (from_root, path)
				else
					resolve_primitive table (String.get h 0 = 'i') width
			) else resolve_non_primitive_id root table (from_root, path)
		)
	)
	| _ -> resolve_non_primitive_id root table (from_root, path)

let resolve_list f table l =
	let rec loop table stack acc =
		match stack with
		| [] -> table, List.rev acc
		| h::t ->
			let table, h = f table h in
			loop table t (h::acc)
	in loop table l []

let resolve_access_restricted f table a =
	match a with
	| Rga.ARPub (bnds, i) -> let table, i = f table i in table, Rsa.ARPub (bnds, i)
	| Rga.ARPriv (bnds, i) -> let table, i = f table i in table, Rsa.ARPriv (bnds, i)
	| Rga.ARStat (bnds, i) -> let table, i = f table i in table, Rsa.ARStat (bnds, i)

let rec resolve_code root table code =
	match code with
	| Rga.UDecl d -> let table, i = resolve_decl root table d in table, Rsa.UDecl i
	| Rga.UExpr e -> let table, i = resolve_expr root table e in table, Rsa.UExpr i
	| Rga.UStmt s -> let table, i = resolve_stmt root table s in table, Rsa.UStmt i

and resolve_decl root table d =
	match d with
	| Rga.DVariable i -> (
		match i.value with
		| None -> let table, i = resolve_unassigned_variable root table i in table, Rsa.DUnassignedVariable i
		| _ -> let table, i = resolve_assigned_variable root table i in table, Rsa.DAssignedVariable i
	)
	| Rga.DAlias i -> let table, i = resolve_alias root table i in table, Rsa.DAlias i
	| Rga.DType i -> let table, i = resolve_type root table i in table, Rsa.DType i
	| Rga.DFunction i -> let table, i = resolve_function root table i in table, Rsa.DFunction i
	| Rga.DOperOverload i -> let table, i = resolve_oper_overload root table i in table, Rsa.DOperOverload i
	| Rga.DEnum i -> let table, i = resolve_enum root table i in table, Rsa.DEnum i
	| Rga.DSpace i -> let table, i = resolve_space root table i in table, Rsa.DSpace i

and resolve_expr root table e =
	match e with
	| Rga.ELambda i ->
		let table, (e, t) = resolve_lambda root table i in
		table, Rsa.ELambda (e, t)
	| Rga.EIf i ->
		let table, (e, t) = resolve_if root table i in
		table, Rsa.EIf (e, t)
	| Rga.EMatch i ->
		let table, (e, t) = resolve_match root table i in
		table, Rsa.EMatch (e, t)
	| Rga.EFor i ->
		let table, (e, t) = resolve_breaking_for root table i in
		table, Rsa.EFor (e, t)
	| Rga.EUntil i ->
		let table, (e, t) = resolve_breaking_until root table i in
		table, Rsa.EUntil (e, t)
	| Rga.EBlock i ->
		let table, (e, t) = resolve_block root table i in
		table, Rsa.EBlock (e, t)
	| Rga.EBinary i ->
		let table, (e, t) = resolve_binary root table i in
		table, Rsa.EBinary (e, t)
	| Rga.EUnary i ->
		let table, (e, t) = resolve_unary root table i in
		table, Rsa.EUnary (e, t)
	| Rga.EUnresolvedReference i ->
		let table, (e, t) = resolve_unresolved_reference root table i in
		table, Rsa.EResolvedReference (e, t)
	| Rga.ECall i ->
		let table, (e, t) = resolve_call root table i in
		table, Rsa.ECall (e, t)
	| Rga.EIndex i ->
		let table, (e, t) = resolve_index root table i in
		table, Rsa.EIndex (e, t)
	| Rga.EInitializer i ->
		let table, (e, t) = resolve_initializer root table i in
		table, Rsa.EInitializer (e, t)
	| Rga.EArrayInitializer i ->
		let table, (e, t) = resolve_array_initializer root table i in
		table, Rsa.EArrayInitializer (e, t)
	| Rga.ETuple i ->
		let table, (e, t) = resolve_tuple root table i in
		table, Rsa.ETuple (e, t)
	| Rga.ELiteral i ->
		let table, (e, t) = resolve_literal root table i in
		table, Rsa.ELiteral (e, t)

and resolve_stmt root table s =
	match s with
	| Rga.SUnresolvedUse i -> let table, i = resolve_unresolved_use root table i in table, Rsa.SUse i
	| Rga.SStandardBreak bnds -> table, Rsa.SStandardBreak bnds
	| Rga.SYieldingBreak i -> let table, i = resolve_yielding_break root table i in table, Rsa.SYieldingBreak i
	| Rga.SContinue bnds -> table, Rsa.SContinue bnds
	| Rga.SFor i -> let table, i = resolve_standard_for root table i in table, Rsa.SFor i
	| Rga.SUntil i -> let table, i = resolve_standard_until root table i in table, Rsa.SUntil i

and resolve_annotation root table a =
	match a with
	| Rga.AAnonymousEnum (id, _members) ->
		(* same shit *)
		table, Rsa.TReference id
	| Rga.ANamelessEnum (id, _members) ->
		(* same shit *)
		table, Rsa.TReference id
	| Rga.ANamelessType (id, _members) ->
		(* edit for each function to return a modified root, so then this can return root with this new nameless type in the root level *)
		(* this way you can drop the data from the return level, into just being in the tree *)
		table, Rsa.TReference id
	| Rga.AFunction {params; return_type; _} ->
		let table, params =
			resolve_list
				(resolve_annotation root)
				table params
		in
		let table, return_type =
			match return_type with
			| Some return_type -> resolve_annotation root table return_type
			| None -> table, Rsa.TReference void_id
		in
		table, Rsa.TFunction (params, return_type)
	| Rga.ATuple elements ->
		let table, elements =
			resolve_list
				(resolve_annotation root)
				table elements
		in table, Rsa.TTuple elements
	| Rga.AUnresolvedReference (_, id) ->
		table, Rsa.TReference (resolve_id root table id)
	| Rga.AVariadic a ->
		let table, a = resolve_annotation root table a in
		table, Rsa.TVariadic a
	| Rga.APointer a ->
		let table, a = resolve_annotation root table a in
		table, Rsa.TPointer a

and resolve_assigned_variable root table {bounds; id; type_ref; value} =
	let table, value =
		resolve_expr root table (Option.get value)
	in
	let table, type_ref = (
		match type_ref with
		| Some type_ref -> resolve_annotation root table type_ref
		| None -> table, (type_of_expr value)
	) in
	(* TODO: value = type_ref *)
	table, {
		bounds = bounds;
		id = id;
		type_ref = type_ref;
		value = value;
	}

and resolve_unassigned_variable root table {bounds; id; type_ref; _} =
	let table, type_ref = (
		match type_ref with
		| Some type_ref -> resolve_annotation root table type_ref
		| None -> failwith "type_ref & value = None"
	) in
	(* TODO: value = type_ref *)
	table, {
		bounds = bounds;
		id = id;
		type_ref = type_ref;
	}

and resolve_alias root table {bounds; id; value} =
	let table, value = resolve_alias_value root table value in
	table, {
		bounds = bounds;
		id = id;
		value = value;
	}

and resolve_alias_value root table value =
	match value with
	| Rga.AAnnotation a ->
		let table, i = resolve_annotation root table a in
		table, Rsa.AAnnotation i
	| Rga.ASpace members ->
		let table, members =
			resolve_list
				(resolve_access_restricted
					(resolve_decl root)) table members
		in table, Rsa.ASpace members
	| Rga.AImport i -> table, Rsa.AImport i

and resolve_type root table {bounds; id; members} =
	let table, members =
		resolve_list
			(resolve_access_restricted (resolve_decl root))
			table members
	in table, {
		bounds = bounds;
		id = id;
		members = members;
	}

and resolve_function root table {bounds; id; params; return_type; code} =
	let required_params, default_params =
		List.partition
			(fun ({value; _} : Rga.variable) ->
				match value with | None -> true | _ -> false)
			params
	in
	let table, required_params =
		resolve_list
			(resolve_unassigned_variable root)
			table required_params
	in
	let table, default_params =
		resolve_list
			(resolve_assigned_variable root)
			table default_params
	in
	let table, code =
		resolve_list
			(resolve_code root)
			table code
	in
	let table, return_type =
		match return_type with
		| Some return_type ->
			let table, return_type = resolve_annotation root table return_type in
			table, return_type
		| None -> table, (type_of_code_list root table code) (* Make this return whatever the result of the code is *)
	in table, {
		bounds = bounds;
		id = id;
		required_params = required_params;
		default_params = default_params;
		return_type = return_type;
		code = code;
	}

and resolve_oper_overload root table {bounds; id; params; return_type; code} =
	let required_params, default_params =
		List.partition
			(fun ({value; _} : Rga.variable) ->
				match value with | None -> true | _ -> false)
			params
	in
	let table, required_params =
		resolve_list
			(resolve_unassigned_variable root)
			table required_params
	in
	let table, default_params =
		resolve_list
			(resolve_assigned_variable root)
			table default_params
	in
	let table, code =
		resolve_list
			(resolve_code root)
			table code
	in
	let table, return_type =
		match return_type with
		| Some return_type ->
			let table, return_type = resolve_annotation root table return_type in
			table, return_type
		| None -> table, (type_of_code_list root table code) (* Make this return whatever the result of the code is *)
	in table, {
		bounds = bounds;
		id = id;
		required_params = required_params;
		default_params = default_params;
		return_type = return_type;
		code = code;
	}

and resolve_enum root table {bounds; id; members} =
	let table, members =
		resolve_list (resolve_enum_member root) table members
	in table, {
		bounds = bounds;
		id = id;
		members = members;
	}

and resolve_space root table {bounds; id; members} =
	let table, members =
		resolve_list
			(resolve_access_restricted
				(resolve_decl root))
			table members
	in table, {
		bounds = bounds;
		id = id;
		members = members;
	}

and resolve_lambda root table {bounds; params; return_type; code} =
	let required_params, default_params =
		List.partition
			(fun ({value; _} : Rga.variable) ->
				match value with | None -> true | _ -> false)
			params
	in
	let table, required_params =
		resolve_list
			(resolve_unassigned_variable root)
			table required_params
	in
	let table, default_params =
		resolve_list
			(resolve_assigned_variable root)
			table default_params
	in
	let table, code =
		resolve_list
			(resolve_code root)
			table code
	in
	let table, return_type =
		match return_type with
		| Some return_type ->
			let table, return_type = resolve_annotation root table return_type in
			table, return_type
		| None -> table, (type_of_code_list root table code) (* Make this return whatever the result of the code is *)
	in table, ({
		bounds = bounds;
		required_params = required_params;
		default_params = default_params;
		return_type = return_type;
		code = code;
	}, Rsa.TFunction (
		(List.map (fun (p : Rsa.unassigned_variable) -> p.type_ref) required_params) @ (List.map (fun (p : Rsa.assigned_variable) -> p.type_ref) default_params),
		return_type
	))

and resolve_if root table {bounds; condition; true_block; false_block} =
	let table, condition = resolve_expr root table condition in
	let table, true_block =
		resolve_list
			(resolve_code root)
			table true_block
	in
	let table, false_block =
		resolve_list
			(resolve_code root)
			table false_block
	in table, ({
		bounds = bounds;
		condition = condition;
		true_block = true_block;
		false_block = false_block;
	}, common_type_of root table (type_of_code_list root table true_block) (type_of_code_list root table false_block))

and resolve_match root table {bounds; switcher; logic} =
	let table, switcher = resolve_expr root table switcher in
	let table, logic =
		resolve_list
			(resolve_match_branch root)
			table logic
	in table, ({
		bounds = bounds;
		switcher = switcher;
		logic = logic;
	}, type_of_match_branch_list root table logic)

and resolve_breaking_for root table {bounds; init; iter; code} =
	let table, init =
		resolve_list
			(resolve_code root)
			table init
	in
	let table, iter =
		resolve_list
			(resolve_code root)
			table iter
	in
	let table, code =
		resolve_list
			(resolve_code root)
			table code
	in table, ({
		bounds = bounds;
		init = init;
		iter = iter;
		code = code;
	}, common_type_of root table
		(type_of_breaking_list root table code)
		(common_type_of root table
			(type_of_breaking_list root table init)
			(type_of_breaking_list root table iter)))

and resolve_breaking_until root table (bnds, inner) =
	let table, inner =
		resolve_list
			(resolve_code root)
			table inner
	in table, ((bnds, inner), type_of_breaking_list root table inner)

and resolve_block root table (bnds, inner) =
	let table, inner =
		resolve_list
			(resolve_code root)
			table inner
	in table, ((bnds, inner), type_of_code_list root table inner)

and resolve_binary root table {bounds; left; op; right} =
	let table, left = resolve_expr root table left in
	let table, right = resolve_expr root table right in
	table, ({
		bounds = bounds;
		left = left;
		op = op;
		right = right;
	}, type_of_binary root table left op right)

and resolve_unary root table {bounds; arg; op} =
	let table, arg = resolve_expr root table arg in
	table, ({
		bounds = bounds;
		arg = arg;
		op = op;
	}, type_of_unary root table arg op)

and resolve_unresolved_reference _root _table _i = failwith "TODO"

and resolve_call root table {bounds; from; args} =
	let table, from = resolve_expr root table from in
	let table, args =
		resolve_list
			(resolve_expr root)
			table args
	in table, ({
		bounds = bounds;
		from = from;
		args = args;
	}, type_of_call root table from args)

and resolve_index root table {bounds; from; args} =
	let table, from = resolve_expr root table from in
	let table, args =
		resolve_list
			(resolve_expr root)
			table args
	in table, ({
		bounds = bounds;
		from = from;
		args = args;
	}, type_of_index root table from args)

and resolve_initializer root table {bounds; type_ref; args} =
	let table, type_ref = resolve_annotation root table type_ref in
	let table, args =
		resolve_list
			(fun table (id, value) ->
				let table, value = resolve_expr root table value in
				table, (id, value)
			) table args
	in table, ({
		bounds = bounds;
		type_ref = type_ref;
		args = args;
	}, type_of_initializer root table type_ref args)

and resolve_array_initializer root table {bounds; size; type_ref; elements} =
	(* TODO: remove the option from size, it should turn into an integer literal of the # of elems in the array or the explicit size *)
	let table, size =
		match size with
		| Some size -> resolve_expr root table size
		| None -> table, Rsa.ELiteral (
			Rsa.LInteger (
				(Lexer.eof, Lexer.eof),
				(string_of_int (List.length elements))
			),
			failwith "TODO" (* TODO: Rsa.TReference (the id of the unsigned int primitive) *)
		)
	in
	let table, type_ref = resolve_annotation root table type_ref in
	let table, elements =
		resolve_list
			(resolve_expr root)
			table elements
	in table, ({
		bounds = bounds;
		size = size;
		type_ref = type_ref;
		elements = elements;
	}, type_of_array_initializer root table type_ref)

and resolve_tuple root table elements =
	let table, elements =
		resolve_list
			(resolve_expr root)
			table elements
	in table, (elements, type_of_tuple root table elements)

and resolve_literal _root table lit =
	match lit with
	| Rga.LInteger i -> table, (Rsa.LInteger i, failwith "TODO")
	| Rga.LFloat i -> table, (Rsa.LFloat i, failwith "TODO")
	| Rga.LCharacter i -> table, (Rsa.LCharacter i, failwith "TODO")
	| Rga.LString i -> table, (Rsa.LString i, failwith "TODO")
	| Rga.LTrue i -> table, (Rsa.LTrue i, failwith "TODO")
	| Rga.LFalse i -> table, (Rsa.LFalse i, failwith "TODO")

and resolve_unresolved_use root table (bnds, id) =
	table, (bnds, resolve_id root table id)

and resolve_yielding_break root table (bnds, value) =
	let table, value = resolve_expr root table value in
	table, (bnds, value)

and resolve_standard_for root table {bounds; init; condition; iter; code} =
	let table, init =
		resolve_list
			(resolve_code root)
			table init
	in
	let table, condition = resolve_expr root table condition in
	let table, iter =
		resolve_list
			(resolve_code root)
			table iter
	in
	let table, code =
		resolve_list
			(resolve_code root)
			table code
	in table, {
		bounds = bounds;
		init = init;
		condition = condition;
		iter = iter;
		code = code;
	}

and resolve_standard_until root table {bounds; condition; code} =
	let table, condition = resolve_expr root table condition in
	let table, code =
		resolve_list
			(resolve_code root)
			table code
	in table, {
		bounds = bounds;
		condition = condition;
		code = code;
	}

and resolve_enum_member root table member =
	match member with
	| Rga.EMVariant (bnds, (id, ann)) ->
		let table, ann = resolve_annotation root table ann in
		table, Rsa.EMVariant (bnds, (id, ann))
	| Rga.EMFunction f ->
		let table, f =
			resolve_access_restricted
				(resolve_function root)
				table f
		in table, Rsa.EMFunction f

and resolve_match_branch root table {bounds; pattern; branch} =
	let table, pattern = resolve_pattern root table pattern in
	let table, branch =
		resolve_list
			(resolve_code root)
			table branch
	in table, {
		bounds = bounds;
		pattern = pattern;
		branch = branch;
	}

and resolve_pattern root table pattern =
	match pattern with
	| Rga.PGuard (bnds, (pattern, guard)) ->
		let table, pattern = resolve_pattern_expr root table pattern in
		let table, guard = resolve_expr root table guard in
		table, Rsa.PGuard (bnds, (pattern, guard))
	| Rga.PStandard pattern ->
		let table, pattern = resolve_pattern_expr root table pattern in
		table, Rsa.PStandard pattern

and resolve_pattern_expr root table pattern =
	match pattern with
	| Rga.PWildcard bnds -> table, Rsa.PWildcard bnds
	| Rga.PBind (bnds, id) -> table, Rsa.PBind (bnds, id)
	| Rga.PLiteral lit ->
		let table, lit = resolve_literal root table lit in
		table, Rsa.PLiteral lit
	| Rga.PTuple args ->
		let table, args =
			resolve_list
				(resolve_pattern_expr root)
				table args
		in table, Rsa.PTuple args
	| Rga.PUnresolvedConstructor (bnds, (id, args)) -> (
		let id = resolve_id root table id in
		(* TODO: go through all resolve_id calls and add logic after to check if the resolved id is correct *)
		let table, args =
			match args with
			| Rga.CTuple args ->
				let table, args =
					resolve_list
						(resolve_pattern_expr root)
						table args
				in table, Rsa.CTuple args
			| Rga.CInitializer fields ->
				let table, fields =
					resolve_list
						(fun table field ->
							match field with
							| Rga.IPBind (bnds, id) -> table, Rsa.IPBind (bnds, id)
							| Rga.IPExpect (bnds, (id, pattern)) ->
								let table, pattern = resolve_pattern_expr root table pattern in
								table, Rsa.IPExpect (bnds, (id, pattern))
						) table fields
				in table, Rsa.CInitializer fields
		in table, Rsa.PConstructor (bnds, (id, args))
	)
	| Rga.PExclusiveRange (bnds, (a, b)) ->
		let table, a = resolve_expr root table a in
		let table, b = resolve_expr root table b in
		table, Rsa.PExclusiveRange (bnds, (a, b))
	| Rga.PInclusiveRange (bnds, (a, b)) ->
		let table, a = resolve_expr root table a in
		let table, b = resolve_expr root table b in
		table, Rsa.PInclusiveRange (bnds, (a, b))

(* and find_node_points ?(context_filter=(fun _ -> true)) scope_stack table (from_root, path) =
	let rec starting_points scope_stack first_name =
		let parent_id =
			match scope_stack with
			| [] -> (-1)
			| h::_ -> h
		in

		let {next;_} = SymbolTable.find table.symbols parent_id in
		let points =
			List.filter
				(fun id ->
					let {name; _} = SymbolTable.find table.symbols id in
					name = first_name
				) next
		in

		match points with
		| [] -> (
			match scope_stack with
			| [] -> []
			| _::t -> starting_points t first_name
		)
		| p -> p
	in

	let rec explore_paths points path =
		match path with
		| [] -> points
		| h::t ->
			let next_points =
				List.concat_map
					(fun point ->
						let {next; _} = SymbolTable.find table.symbols point in
						List.filter
							(fun id ->
								let {name; _} = SymbolTable.find table.symbols id in
								name = h
							) next
					)
					points
			in explore_paths next_points t
	in

	let wrapped_context_filter points =
		let rec track_path point acc =
			if point = -1 then (
				acc
			) else (
				let {parent; _} = SymbolTable.find table.symbols point in
				track_path parent (point::acc)
			)
		in

		let child_of node name =
			match node with
			| Some d -> (
				match d with
				| Ca.DVariable a ->
					if a.name = name then Some d else None
				| Ca.DAlias a ->
					if a.name = name then Some d else None
				| Ca.DType a ->
					if a.name = name then Some d else None
				| Ca.DFunction a ->
					if a.name = name then Some d else None
				| Ca.DOperOverload a ->
					if (op_func_name a.op) = name then Some d else None
				| Ca.DEnum a ->
					if a.name = name then Some d else None
				| Ca.DSpace a ->
					if a.name = name then Some d else None
			)
			| None -> failwith "Search root"
		in

		let (* rec *) find_node path node =
			match path with
			| [] -> (Option.get node)
			| _h::_t -> (
				failwith "TODO"
				(*
					TODO:
					1. make a let child_of (node : decl option) name : decl then returns the child
					2. use this to find the child of node with the name in _h and then call find_node _t (the node just found with child_of)
				*)
			)
		in

		List.filter
			(fun point ->
				let path = track_path point [] in
				let node = find_node path None in
				context_filter node
			)
			points
	in

	let points = starting_points (if from_root then [] else scope_stack) (List.hd path) in
	let points = explore_paths points (List.tl path) in
	wrapped_context_filter points *)

let resolve_prog root table prog =
	let table = create_primitives table in
	List.map (resolve_code root table) prog
