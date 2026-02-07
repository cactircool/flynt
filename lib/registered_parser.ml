module Ca = Core_ast
module Ra = Registered_ast

module SymbolTable = Hashtbl.Make(Int)

exception InvalidReferenceError of string Ra.fat
exception AmbiguousReferenceError of string Ra.fat
exception InternalError of string Ra.fat

type symbol_entry = {
	name : string;
	parent : int;
	next : int list;
}

type symbol_table = {
	symbols : symbol_entry SymbolTable.t;
	id : int;
	nameless_ctr : int
}

let op_func_name op =
	match op with
	| Ca.Bracket -> "[]"
	| Ca.Paren -> "()"
	| Ca.Binary bop -> "b(" ^ (Ca.binop_value bop) ^ ")"
	| Ca.Unary uop -> (if Ca.unop_left uop then "l(" else "r(") ^ (Ca.unop_value uop) ^ ")"

let nameless_name table thing =
	"<anon_" ^ thing ^ (string_of_int table.nameless_ctr) ^ ">"

let rec stringify_path path acc =
	match path with
	| [] -> acc
	| h::t -> stringify_path t (acc ^ "::" ^ h)

let stringify_id (from_root, path) =
	(if from_root then "::" else "") ^ (stringify_path path "")

let convert_literal = function
	| Ca.LInteger i -> Ra.LInteger i
	| Ca.LFloat i -> Ra.LFloat i
	| Ca.LCharacter i -> Ra.LCharacter i
	| Ca.LString i -> Ra.LString i
	| Ca.LTrue i -> Ra.LTrue i
	| Ca.LFalse i -> Ra.LFalse i

let register scope_stack name table =
	(* TODO: add feature where if the name already exists as a child of the parent error (you can only shadow variables outside the current scope) *)
	let new_id = table.id + 1 in
	let table = { table with id = new_id } in
	let parent_id =
		match scope_stack with
		| [] -> -1
		| h::_ -> h
	in
	let parent_entry = SymbolTable.find table.symbols parent_id in
	SymbolTable.replace table.symbols parent_id { parent_entry with next = (new_id::(parent_entry.next)) };
	SymbolTable.add table.symbols new_id {
		name = name;
		parent =
			(match scope_stack with
			| [] -> (-1)
			| h::_ -> h)
		;
		next = [];
	}; new_id, table

let register_list reg table l =
	let rec loop table l =
		match l with
		| [] -> table, []
		| h::t ->
			let table, h = reg table h in
			let table, rest = loop table t in
			table, h::rest
	in loop table l

let register_access_restricted reg table ar =
	match ar with
	| Ca.ARPub (bnds, i) ->
		let table, i = reg table i in
		table, Ra.ARPub (bnds, i)
	| Ca.ARPriv (bnds, i) ->
		let table, i = reg table i in
		table, Ra.ARPriv (bnds, i)
	| Ca.ARStat (bnds, i) ->
		let table, i = reg table i in
		table, Ra.ARStat (bnds, i)

let rec register_code scope_stack table code =
	match code with
	| Ca.UDecl decl ->
		let table, decl = register_decl scope_stack table decl in
		table, Ra.UDecl decl
	| Ca.UExpr expr ->
		let table, expr = register_expr scope_stack table expr in
		table, Ra.UExpr expr
	| Ca.UStmt stmt ->
		let table, stmt = register_stmt scope_stack table stmt in
		table, Ra.UStmt stmt

and register_decl scope_stack table decl =
	match decl with
	| DVariable v ->
		let table, v = register_variable scope_stack table v in
		table, DVariable v
	| DAlias a ->
		let table, a = register_alias scope_stack table a in
		table, DAlias a
	| DType t ->
		let table, t = register_type scope_stack table t in
		table, DType t
	| DFunction f ->
		let table, f = register_function scope_stack table f in
		table, DFunction f
	| DOperOverload oo ->
		let table, oo = register_oper_overload scope_stack table oo in
		table, DOperOverload oo
	| DEnum e ->
		let table, e = register_enum scope_stack table e in
		table, DEnum e
	| DSpace s ->
		let table, s = register_space scope_stack table s in
		table, DSpace s

and register_expr scope_stack table expr =
	match expr with
	| Ca.ELambda l ->
		let table, l = register_lambda scope_stack table l in
		table, Ra.ELambda l
	| Ca.EIf i ->
		let table, i = register_if scope_stack table i in
		table, Ra.EIf i
	| Ca.EMatch m ->
		let table, m = register_match scope_stack table m in
		table, Ra.EMatch m
	| Ca.EFor bf ->
		let table, bf = register_breaking_for scope_stack table bf in
		table, Ra.EFor bf
	| Ca.EUntil u ->
		let table, u = register_breaking_until scope_stack table u in
		table, Ra.EUntil u
	| Ca.EBlock b ->
		let table, b = register_block scope_stack table b in
		table, Ra.EBlock b
	| Ca.EBinary b ->
		let table, b = register_binary scope_stack table b in
		table, Ra.EBinary b
	| Ca.EUnary u ->
		let table, u = register_unary scope_stack table u in
		table, Ra.EUnary u
	| Ca.EUnresolvedReference ur -> table, Ra.EUnresolvedReference ur
	| Ca.ECall c ->
		let table, c = register_call scope_stack table c in
		table, Ra.ECall c
	| Ca.EIndex i ->
		let table, i = register_index scope_stack table i in
		table, Ra.EIndex i
	| Ca.EInitializer i ->
		let table, i = register_initializer scope_stack table i in
		table, Ra.EInitializer i
	| Ca.EArrayInitializer i ->
		let table, i = register_array_initializer scope_stack table i in
		table, Ra.EArrayInitializer i
	| Ca.ETuple t ->
		let table, t = register_tuple scope_stack table t in
		table, Ra.ETuple t
	| Ca.ELiteral l -> table, Ra.ELiteral (convert_literal l)

and register_pattern scope_stack table pattern =
	match pattern with
	| Ca.PGuard (bnds, (pat, guard)) ->
		let table, pat = register_pattern_expr scope_stack table pat in
		let table, guard = register_expr scope_stack table guard in
		table, Ra.PGuard (bnds, (pat, guard))
	| Ca.PStandard pat ->
		let table, pat = register_pattern_expr scope_stack table pat in
		table, Ra.PStandard pat

and register_stmt scope_stack table stmt =
	match stmt with
	| Ca.SUnresolvedUse (bnds, use) -> table, Ra.SUnresolvedUse (bnds, use)
	| Ca.SStandardBreak bnds -> table, Ra.SStandardBreak bnds
	| Ca.SYieldingBreak (bnds, value) ->
		let table, value = register_expr scope_stack table value in
		table, Ra.SYieldingBreak (bnds, value)
	| Ca.SContinue bnds -> table, Ra.SContinue bnds
	| Ca.SFor sf ->
		let table, sf = register_standard_for scope_stack table sf in
		table, Ra.SFor sf
	| Ca.SUntil su ->
		let table, su = register_standard_until scope_stack table su in
		table, Ra.SUntil su

and register_variable scope_stack table {name; value; bounds; type_ref} =
	let id, table = register scope_stack name table in
	let table, type_ref =
		match type_ref with
		| Some type_ref ->
			let table, type_ref = register_annotation scope_stack table type_ref in
			table, Some type_ref
		| None -> table, None
	in
	let table, value =
		match value with
		| Some value ->
			let table, value = register_expr scope_stack table value in
			table, Some value
		| None -> table, None
	in
	table, {
		bounds = bounds;
		id = id;
		type_ref = type_ref;
		value = value;
	}

and register_alias scope_stack table {name; value; bounds} =
	let id, table = register scope_stack name table in
	let table, value = register_alias_value scope_stack table value in
	table, {
		bounds = bounds;
		id = id;
		value = value;
	}

and register_type scope_stack table {name; members; bounds} =
	let id, table = register scope_stack name table in
	let table, members =
		register_list
			(register_access_restricted
				(register_decl (id::scope_stack)))
			table members
	in table, {
		bounds = bounds;
		id = id;
		members = members;
	}

and register_function scope_stack table {name; params; return_type; code; bounds} =
	let id, table = register scope_stack name table in
	let scope_stack = id::scope_stack in
	let table, params =
		register_list
			(register_variable scope_stack)
			table params
	in
	let table, return_type =
		match return_type with
		| None -> table, None
		| Some rt ->
			let table, rt = register_annotation scope_stack table rt in
			table, Some rt
	in
	let table, code =
		register_list
			(register_code scope_stack)
			table code
	in table, {
		id = id;
		bounds = bounds;
		params = params;
		return_type = return_type;
		code = code;
	}

and register_oper_overload scope_stack table {op; params; return_type; code; bounds} =
	let id, table = register scope_stack (op_func_name op) table in
	let scope_stack = id :: scope_stack in
	let table, params =
		register_list
			(register_variable scope_stack)
			table params
	in
	let table, return_type =
		match return_type with
		| None -> table, None
		| Some rt ->
			let table, rt = register_annotation scope_stack table rt in
			table, Some rt
	in
	let table, code =
		register_list
			(register_code scope_stack)
			table code
	in table, {
		id = id;
		bounds = bounds;
		params = params;
		return_type = return_type;
		code = code;
	}

and register_enum scope_stack table {name; members; bounds} =
	let id, table = register scope_stack name table in
	let scope_stack = id :: scope_stack in
	let table, members =
		register_list
			(register_enum_member scope_stack)
			table members
	in table, {
		id = id;
		bounds = bounds;
		members = members;
	}

and register_space scope_stack table {name = (from_root, path) as name; members; bounds} =
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
		| [] -> points, path
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
			in

			match next_points with
			| [] -> points, path
			| _ -> explore_paths next_points t
	in

	let start_points = starting_points (if from_root then [] else scope_stack) (List.hd path) in
	let points, path = explore_paths start_points (List.tl path) in

	match points with
	| [] -> raise (InvalidReferenceError (bounds, Printf.sprintf "could not resolve space '%s'." (stringify_id name)))
	| [id] -> (
		let id, table =
			match path with
			| p when p <> [] ->
				let rec loop_register_path table parent path =
					match path with
					| [] -> parent, table
					| h::t ->
						let id, table = register [parent] h table in
						loop_register_path table id t
				in loop_register_path table id p
			| _ -> (-1), table
		in
		let table, members =
			register_list
				(register_access_restricted (register_decl (id::scope_stack)))
				table members
		in table, {
			id = id;
			bounds = bounds;
			members = members;
		}
	)
	| _ -> raise (AmbiguousReferenceError (bounds, Printf.sprintf "multiple candidates found for '%s'." (stringify_id name)))

and register_enum_member scope_stack table member =
	match member with
	| Ca.EMVariant (bnds, (name, value)) ->
		let id, table = register scope_stack name table in
		let table, value =
			register_annotation scope_stack table value
		in table, Ra.EMVariant (bnds, (id, value))
	| Ca.EMFunction a ->
		let table, f =
			register_access_restricted
				(register_function scope_stack)
				table a
		in table, Ra.EMFunction f

and register_alias_value scope_stack table alias_value =
	match alias_value with
	| Ca.AAnnotation a ->
		let table, a = register_annotation scope_stack table a in
		table, Ra.AAnnotation a
	| Ca.ASpace members ->
		let id, table = register scope_stack (nameless_name table "space") table in
		let table = { table with nameless_ctr = table.nameless_ctr + 1 } in
		let table, members =
			register_list
				(register_access_restricted
					(register_decl (id::scope_stack)))
				table members
		in table, Ra.ASpace members
	| Ca.AImport import ->
		table, Ra.AImport import

and register_annotation scope_stack table annotation =
	match annotation with
	| Ca.AAnonymousEnum members ->
		let id, table = register scope_stack (nameless_name table "anon_enum") table in
		let table, members =
			register_list
				(register_anonymous_enum_member (id::scope_stack))
				table members
		in table, Ra.AAnonymousEnum (id, members)
	| Ca.ANamelessEnum members ->
		let id, table = register scope_stack (nameless_name table "enum") table in
		let table, members =
			register_list
				(register_enum_member (id::scope_stack))
				table members
		in table, Ra.ANamelessEnum (id, members)
	| Ca.ANamelessType members ->
		let id, table = register scope_stack (nameless_name table "type") table in
		let table, members =
			register_list
				(register_access_restricted
					(register_decl (id::scope_stack)))
				table members
		in table, Ra.ANamelessType (id, members)
	| Ca.AFunction {params; return_type; bounds} ->
		let table, params =
			register_list
				(register_annotation scope_stack)
				table params
		in
		let table, return_type =
			match return_type with
			| None -> table, None
			| Some rt ->
				let table, rt =
					register_annotation scope_stack table rt
				in table, Some rt
		in table, Ra.AFunction {
			bounds = bounds;
			params = params;
			return_type = return_type
		}
	| Ca.ATuple members ->
		let table, members =
			register_list
				(register_annotation scope_stack)
				table members
		in table, Ra.ATuple members
	| Ca.AUnresolvedReference (bnds, ref) ->
		table, Ra.AUnresolvedReference (bnds, ref)
		(* let context_filter = function
		| Ca.DType _
		| Ca.DAlias _
		| Ca.DEnum _ -> true
		| _ -> false in

		(match find_node_points scope_stack table ref ~context_filter:context_filter with
		| [] -> raise (InvalidReferenceError (bnds, Printf.sprintf "invalid reference: '%s'." (stringify_id ref)))
		| [h] -> table, Ra.AReference (bnds, h)
		| _ -> raise (AmbiguousReferenceError (bnds, Printf.sprintf "multiple candidates for '%s' found." (stringify_id ref)))) *)
	| Ca.AVariadic t ->
		let table, t =
			register_annotation scope_stack table t
		in table, Ra.AVariadic t
	| Ca.APointer t ->
		let table, t =
			register_annotation scope_stack table t
		in table, Ra.AVariadic t

and register_anonymous_enum_member scope_stack table member =
	match member with
	| Ca.AEMVariant a ->
		let id, table = register scope_stack (nameless_name table "aem") table in
		let table, a = register_annotation scope_stack table a in
		table, Ra.AEMVariant (id, a)
	| Ca.AEMFunction f ->
		let table, f =
			register_access_restricted
				(register_function scope_stack)
				table f
		in table, Ra.AEMFunction f

and register_standard_for scope_stack table {bounds; init; condition; iter; code} =
	let id, table = register scope_stack (nameless_name table "sfor") table in
	let scope_stack = id::scope_stack in
	let register_code_list =
		register_list
			(register_code scope_stack)
	in
	let table, init = register_code_list table init in
	let table, condition = register_expr scope_stack table condition in
	let table, iter = register_code_list table iter in
	let table, code = register_code_list table code in
	table, {
		bounds = bounds;
		init = init;
		condition = condition;
		iter = iter;
		code = code;
	}

and register_standard_until scope_stack table {bounds; condition; code} =
	let id, table = register scope_stack (nameless_name table "suntil") table in
	let scope_stack = id::scope_stack in
	let table, condition = register_expr scope_stack table condition in
	let table, code =
		register_list
			(register_code scope_stack)
			table code
	in
	table, {
		bounds = bounds;
		condition = condition;
		code = code;
	}

and register_lambda scope_stack table {bounds; params; return_type; code} =
	let id, table = register scope_stack (nameless_name table "lambda") table in
	let scope_stack = id::scope_stack in
	let table, params =
		register_list
			(register_variable scope_stack)
			table params
	in
	let table, return_type =
		match return_type with
		| Some rt ->
			let table, rt = register_annotation scope_stack table rt in
			table, Some rt
		| None -> table, None
	in
	let table, code =
		register_list
			(register_code scope_stack)
			table code
	in table, {
		bounds = bounds;
		params = params;
		return_type = return_type;
		code = code;
	}

and register_if scope_stack table {bounds; condition; true_block; false_block} =
	let id, table = register scope_stack (nameless_name table "if") table in
	let scope_stack = id::scope_stack in
	let table, condition = register_expr scope_stack table condition in
	let table, true_block = register_list (register_code scope_stack) table true_block in
	let table, false_block = register_list (register_code scope_stack) table false_block in
	table, {
		bounds = bounds;
		condition = condition;
		true_block = true_block;
		false_block = false_block;
	}

and register_match scope_stack table {bounds; switcher; logic} =
	let id, table = register scope_stack (nameless_name table "match") table in
	let scope_stack = id::scope_stack in
	let table, switcher = register_expr scope_stack table switcher in
	let table, logic = register_list (register_match_branch scope_stack) table logic in
	table, {
		bounds = bounds;
		switcher = switcher;
		logic = logic;
	}

and register_match_branch scope_stack table {bounds; pattern; branch} =
	let id, table = register scope_stack (nameless_name table "matchb") table in
	let scope_stack = id::scope_stack in
	let table, pattern = register_pattern scope_stack table pattern in
	let table, branch = register_list (register_code scope_stack) table branch in
	table, {
		bounds = bounds;
		pattern = pattern;
		branch = branch;
	}

and register_breaking_for scope_stack table {bounds; init; iter; code} =
	let id, table = register scope_stack (nameless_name table "bfor") table in
	let scope_stack = id::scope_stack in
	let register_code_list =
		register_list
			(register_code scope_stack)
	in
	let table, init = register_code_list table init in
	let table, iter = register_code_list table iter in
	let table, code = register_code_list table code in
	table, {
		bounds = bounds;
		init = init;
		iter = iter;
		code = code;
	}

and register_breaking_until scope_stack table (bnds, code) =
	let id, table = register scope_stack (nameless_name table "buntil") table in
	let scope_stack = id::scope_stack in
	let table, code =
		register_list
			(register_code scope_stack)
			table code
	in
	table, (bnds, code)

and register_block scope_stack table (bnds, code) =
	let id, table = register scope_stack (nameless_name table "block") table in
	let scope_stack = id::scope_stack in
	let table, code =
		register_list
			(register_code scope_stack)
			table code
	in
	table, (bnds, code)

and register_binary scope_stack table {bounds; left; op; right} =
	let table, left = register_expr scope_stack table left in
	let table, right = register_expr scope_stack table right in
	table, {
		bounds = bounds;
		left = left;
		op = op;
		right = right;
	}

and register_unary scope_stack table {bounds; arg; op} =
	let table, arg = register_expr scope_stack table arg in
	table, {
		bounds = bounds;
		arg = arg;
		op = op;
	}

and register_call scope_stack table {bounds; from; args} =
	let table, from = register_expr scope_stack table from in
	let table, args =
		register_list
			(register_expr scope_stack)
			table args
	in table, {
		bounds = bounds;
		from = from;
		args = args;
	}

and register_index scope_stack table {bounds; from; args} =
	let table, from = register_expr scope_stack table from in
	let table, args =
		register_list
			(register_expr scope_stack)
			table args
	in table, {
		bounds = bounds;
		from = from;
		args = args;
	}

and register_initializer scope_stack table {bounds; type_ref; args} =
	let table, type_ref = register_annotation scope_stack table type_ref in
	let table, args =
		register_list
			(fun table (name, expr) ->
				let id, table = register scope_stack name table in
				let table, expr = register_expr scope_stack table expr in
				table, (id, expr)
			) table args
	in table, {
		bounds = bounds;
		type_ref = type_ref;
		args = args;
	}

and register_array_initializer scope_stack table {bounds; size; type_ref; elements} =
	let table, size =
		match size with
		| Some size ->
			let table, size = register_expr scope_stack table size in
			table, Some size
		| None -> table, None
	in
	let table, type_ref = register_annotation scope_stack table type_ref in
	let table, elements =
		register_list (register_expr scope_stack) table elements
	in table, {
		bounds = bounds;
		size = size;
		type_ref = type_ref;
		elements = elements;
	}

and register_tuple scope_stack table elements =
	register_list
		(register_expr scope_stack)
		table elements

and register_pattern_expr scope_stack table pattern =
	match pattern with
	| Ca.PWildcard bnds -> table, Ra.PWildcard bnds
	| Ca.PBind (bnds, name) ->
		let id, table = register scope_stack name table in
		table, Ra.PBind (bnds, id)
	| Ca.PLiteral l -> table, Ra.PLiteral (convert_literal l)
	| Ca.PTuple elems ->
		let table, elems =
		register_list
			(register_pattern_expr scope_stack)
			table elems
		in table, Ra.PTuple elems
	| Ca.PUnresolvedConstructor (bnds, (id, args)) ->
		let table, args = register_constructor_args scope_stack table args in
		table, Ra.PUnresolvedConstructor (bnds, (id, args))
	| Ca.PExclusiveRange (bnds, (from, t)) ->
		let table, from = register_expr scope_stack table from in
		let table, t = register_expr scope_stack table t in
		table, Ra.PExclusiveRange (bnds, (from, t))
	| Ca.PInclusiveRange (bnds, (from, t)) ->
		let table, from = register_expr scope_stack table from in
		let table, t = register_expr scope_stack table t in
		table, Ra.PInclusiveRange (bnds, (from, t))

and register_constructor_args scope_stack table c =
	match c with
	| Ca.CTuple elems ->
		let table, elems =
			register_list
				(register_pattern_expr scope_stack)
				table elems
		in table, Ra.CTuple elems
	| Ca.CInitializer elems ->
		let table, elems =
			register_list
				(fun table ipf ->
					match ipf with
					| Ca.IPBind (bnds, name) ->
						let id, table = register scope_stack name table in
						table, Ra.IPBind (bnds, id)
					| Ca.IPExpect (bnds, (name, pat)) ->
						let id, table = register scope_stack name table in
						let table, pat = register_pattern_expr scope_stack table pat in
						table, Ra.IPExpect (bnds, (id, pat))
				) table elems
		in table, Ra.CInitializer elems

let register_prog prog =
	let table = {
		symbols = SymbolTable.create 16;
		id = 0;
		nameless_ctr = 0;
	} in
	SymbolTable.add table.symbols (-1) {
		name = "<root>";
		parent = -1;
		next = []
	};
	let rec loop table prog acc =
		match prog with
		| [] -> table, acc
		| h::t ->
			let table, h = register_code [] table h in
			loop table t (h::acc)
	in loop table prog []
