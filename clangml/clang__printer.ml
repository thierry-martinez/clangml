open Clang__ast

open Clang__ast_utils

let maybe_parentheses in_prec out_prec fmt k =
  if in_prec >= out_prec then
    Format.fprintf fmt "(@[%t@])" k
  else
    k fmt

let rec decl fmt (d : decl) =
  match d.desc with
  | Function { linkage; function_type; name; body } ->
      print_linkage fmt linkage;
      print_function_type fmt function_type name;
      print_function_body fmt body
  | Var { linkage; qual_type = t; name; init } ->
      Format.fprintf fmt "@[%a%a@ %s%a;@]"
	print_linkage linkage
	qual_type t
	name
	print_variable_init init
  | _ -> failwith (Format.asprintf "Not implemented decl: %a" pp_decl d)

and print_variable_init fmt init =
  match init with
  | None -> ()
  | Some value ->
      Format.fprintf fmt "@ =@ %a" expr value

and expr fmt e =
  expr_prec 15 fmt e

and expr_prec prec fmt (e : expr) =
  match e.desc with
  | IntegerLiteral i ->
      pp_print_integer_literal fmt i
  | FloatingLiteral f ->
      pp_print_floating_literal fmt f
  | Call { callee; args } ->
      maybe_parentheses 1 prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a(@[%a@])@]" (expr_prec 1) callee
	  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@ ")
	     (expr_prec 15)) args)
  | UnaryOperator { kind = PostInc; operand } ->
      maybe_parentheses 2 prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a++@]" (expr_prec 2) operand)
  | BinaryOperator { lhs; kind = Assign; rhs } ->
      maybe_parentheses 14 prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a@ =@ %a@]" (expr_prec 14) lhs (expr_prec 14) rhs)
  | BinaryOperator { lhs; kind = GT; rhs } ->
      maybe_parentheses 6 prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a@ >@ %a@]" (expr_prec 6) lhs (expr_prec 6) rhs)
  | BinaryOperator { lhs; kind = Add; rhs } ->
      maybe_parentheses 4 prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a@ +@ %a@]" (expr_prec 4) lhs (expr_prec 4) rhs)
  | DeclRef name ->
      Format.pp_print_string fmt name
  | _ -> failwith (Format.asprintf "Not implemented expr %a" pp_expr e)

and stmt fmt (s : stmt) =
  match s.desc with
  | Null -> Format.pp_print_string fmt ";"
  | Compound list ->
      Format.fprintf fmt "@[{@[%a@]}@]" (Format.pp_print_list stmt) list
  | If { cond; then_branch; else_branch; _ } ->
      Format.fprintf fmt "@[if@ (@[%a@])@ %a%a@]"
	expr cond stmt then_branch print_else_branch else_branch
  | While { cond; body; _ } ->
      Format.fprintf fmt "@[while@ (@[%a@])@ %a@]"
	expr cond stmt body
  | Return None ->
      Format.fprintf fmt "@[return;@]"
  | Return (Some value) ->
      Format.fprintf fmt "@[return@ %a;@]" expr value
  | Decl d ->
      Format.pp_print_list decl fmt d
  | Expr e ->
      Format.fprintf fmt "@[%a;@]" expr e
  | _ -> failwith (Format.asprintf "Not implemented stmt: %a" pp_stmt s)

and print_else_branch fmt else_branch =
  match else_branch with
  | None -> ()
  | Some else_branch ->
      Format.fprintf fmt "@[else@ %a@]" stmt else_branch

and print_linkage fmt linkage =
  match linkage with
  | Internal -> Format.fprintf fmt "static@ "
  | External
  | NoLinkage -> ()
  | _ -> failwith (Format.asprintf "Not implemented linkage: %a" pp_linkage_kind linkage)

and print_function_parameters fmt parameters =
  match parameters with
  | None -> ()
  | Some parameters ->
      let all_parameters = List.map Option.some parameters.non_variadic in
      let all_parameters =
        if parameters.variadic then all_parameters @ [None] else all_parameters in
      let print_parameter fmt (parameter : parameter option) =
	match parameter with
	| None -> Format.pp_print_string fmt "..."
	| Some { desc = { name = ""; qual_type = ty; _ }} -> qual_type fmt ty
	| Some { desc = { name; qual_type = ty; _ }} ->
            Format.fprintf fmt "%a@ %s" qual_type ty name in
      Format.pp_print_list
	~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@ ")
	print_parameter fmt all_parameters

and print_function_type fmt function_type name =
  Format.fprintf fmt "@[%a@ %s(%a)@]" qual_type function_type.result name
    print_function_parameters function_type.parameters

and print_function_body fmt body =
  match body with
  | None -> Format.pp_print_string fmt ";"
  | Some body -> stmt fmt body 

and qual_type fmt t =
  match t.desc with
  | BuiltinType Int ->
      Format.pp_print_string fmt "int"
  | _ -> failwith (Format.asprintf "Not implemented qual type: %a" pp_qual_type t)

let translation_unit fmt tu =
  List.iter (decl fmt) tu.items
