open Clang__ast

open Clang__ast_utils

let maybe_parentheses in_prec out_prec fmt k =
  if in_prec >= out_prec then
    Format.fprintf fmt "(@[%t@])" k
  else
    k fmt

let pp_print_option pp_print_value fmt opt =
  match opt with
  | None -> ()
  | Some value -> pp_print_value fmt value

let c_escape_char fmt c =
  match c with
  | '"' | '\'' | '\\' ->  Format.fprintf fmt "\\%c" c
  | '\x07' -> Format.pp_print_string fmt "\\a"
  | '\x08' -> Format.pp_print_string fmt "\\b"
  | '\x09' -> Format.pp_print_string fmt "\\t"
  | '\x0A' -> Format.pp_print_string fmt "\\n"
  | '\x0B' -> Format.pp_print_string fmt "\\v"
  | '\x0C' -> Format.pp_print_string fmt "\\f"
  | '\x0D' -> Format.pp_print_string fmt "\\r"
  | '\x00' .. '\x20' -> Format.fprintf fmt "\\x%02X" (int_of_char c)
  | _ -> Format.pp_print_char fmt c

type associativity =
  | Left_to_right
  | Right_to_left

type unary_position =
  | Prefix
  | Postfix

let prec_of_unary_operator (kind : unary_operator_kind) =
  match kind with
  | PostInc | PostDec -> 1, Postfix
  | PreInc | PreDec -> 1, Prefix
  | AddrOf
  | Deref
  | Plus | Minus
  | Not | LNot -> 2, Prefix
  | _ -> invalid_arg "prec_of_unary_operator"

let prec_of_binary_operator (kind : binary_operator_kind) =
  match kind with
  | Mul | Div | Rem -> 3, Left_to_right
  | Add | Sub -> 4, Left_to_right
  | Shl | Shr -> 5, Left_to_right
  | LT | GT | LE | GE -> 6, Left_to_right
  | EQ | NE -> 7, Left_to_right
  | And -> 8, Left_to_right
  | Xor -> 9, Left_to_right
  | Or -> 10, Left_to_right
  | LAnd -> 11, Left_to_right
  | LOr -> 12, Left_to_right
  | Assign
  | MulAssign
  | DivAssign
  | RemAssign
  | AddAssign
  | SubAssign
  | ShlAssign
  | ShrAssign
  | AndAssign
  | XorAssign
  | OrAssign -> 14, Right_to_left
  | Comma -> 15, Left_to_right
  | _ -> invalid_arg "prec_of_binary_operator"

let rec decl fmt (d : decl) =
  match d.desc with
  | Function { linkage; function_type; name; body } ->
      print_linkage fmt linkage;
      print_function_type fmt function_type name;
      print_function_body fmt body
  | Var { linkage; var_type = ty; var_name; var_init } ->
      Format.fprintf fmt "@[%a%a%a;@]"
	print_linkage linkage
	(typed_value (fun fmt -> Format.pp_print_string fmt var_name)) ty
	print_variable_init var_init
  | RecordDecl { keyword; name; fields } ->
      Format.fprintf fmt "@[%s@ %s@ {%a}@]"
        (Clang__bindings.ext_elaborated_type_get_keyword_spelling keyword)
        name
        (Format.pp_print_list decl)
        fields
  | Field { name; qual_type = ty; bitwidth } ->
      Format.fprintf fmt "@[%a%t;@]"
        (typed_value (fun fmt -> Format.pp_print_string fmt name)) ty
        (fun fmt ->
          match bitwidth with
          | None -> ()
          | Some bitwidth -> Format.fprintf fmt "@ :@ %a" expr bitwidth)
  | AccessSpecifier specifier ->
      Format.fprintf fmt "%s:" (Clang__utils.string_of_cxx_access_specifier specifier)
  | Constructor { class_name; parameters; initializer_list; body; explicit; defaulted; deleted } ->
      Format.fprintf fmt "%t%s(%a)%t%t%a"
        (fun fmt ->
          if explicit then Format.fprintf fmt "explicit@ ")
        class_name
        print_parameters parameters
        (fun fmt ->
          match initializer_list with
          | [] -> ()
          | _ ->
              Format.fprintf fmt "@ :@ ";
              Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
                (fun fmt (name, value) ->
                  Format.fprintf fmt "%s(%a)" name expr value)
                fmt initializer_list)
        (fun fmt ->
          if defaulted then
            Format.fprintf fmt "@ =@ default";
          if deleted then
            Format.fprintf fmt "@ =@ delete")
        print_function_body body
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
  | CharacterLiteral { kind; value } ->
      begin
        match kind with
        | Ascii -> Format.fprintf fmt "'%a'" c_escape_char (char_of_int value)
        | _ -> failwith "Not implemented character kind"
      end
  | StringLiteral str ->
      Format.pp_print_string fmt "\"";
      String.iter (c_escape_char fmt) str;
      Format.pp_print_string fmt "\""
  | Call {
        callee = { desc = DeclRef (BinaryOperatorRef kind) };
        args = [lhs; rhs]} ->
      expr_prec prec fmt { e with desc = BinaryOperator { lhs; kind; rhs }}
  | Call { callee; args } ->
      maybe_parentheses 1 prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a(@[%a@])@]" (expr_prec 1) callee
	  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@ ")
	     (expr_prec 15)) args)
  | UnaryOperator { kind; operand } ->
      let op_prec, position = prec_of_unary_operator kind in
      begin match position with
      | Prefix ->
          maybe_parentheses op_prec prec fmt (fun fmt ->
	    Format.fprintf fmt "@[%s@ %a@]"
              (Clang__bindings.ext_unary_operator_get_opcode_spelling kind)
              (expr_prec op_prec) operand)
      | Postfix ->
          maybe_parentheses op_prec prec fmt (fun fmt ->
	    Format.fprintf fmt "@[%a@ %s@]" (expr_prec op_prec)
              operand (Clang__bindings.ext_unary_operator_get_opcode_spelling kind))
      end
  | BinaryOperator { lhs; kind; rhs } ->
      let op_prec, associativity = prec_of_binary_operator kind in
      let left_prec, right_prec =
        match associativity with
        | Left_to_right -> op_prec - 1, op_prec
        | Right_to_left -> op_prec, op_prec - 1 in
      maybe_parentheses op_prec prec fmt (fun fmt ->
	Format.fprintf fmt "@[%a@ %s@ %a@]" (expr_prec left_prec) lhs
          (Clang__bindings.ext_binary_operator_get_opcode_spelling kind)
          (expr_prec right_prec) rhs)
  | DeclRef ident_ref ->
      print_ident_ref fmt ident_ref
  | MemberRef member_ref ->
      Format.pp_print_string fmt member_ref
  | Cast { kind = CStyle; qual_type = ty; operand } ->
      maybe_parentheses 2 prec fmt (fun fmt ->
        Format.fprintf fmt "@[(%a)@ %a@]" qual_type ty (expr_prec 4) operand)
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

and print_parameters fmt parameters =
  let all_parameters = List.map Option.some parameters.non_variadic in
  let all_parameters =
    if parameters.variadic then all_parameters @ [None] else all_parameters in
  let print_parameter fmt (parameter : parameter option) =
    match parameter with
    | None -> Format.pp_print_string fmt "..."
    | Some { desc = { name; qual_type = ty; _ }} ->
        typed_value (fun fmt -> Format.pp_print_string fmt name) fmt ty in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@ ")
    print_parameter fmt all_parameters

and print_function_type fmt function_type name =
  typed_value
    (fun fmt -> Format.fprintf fmt "@[%s(%a)@]" name
        (pp_print_option print_parameters) function_type.parameters)
    fmt function_type.result

and print_function_body fmt body =
  match body with
  | None -> Format.pp_print_string fmt ";"
  | Some body -> stmt fmt body

and print_ident_ref fmt ident_ref =
  match ident_ref with
  | Ident name -> Format.pp_print_string fmt name
  | BinaryOperatorRef EQ ->
      Format.pp_print_string fmt "operator=="
  | NamespaceRef { namespace_ref = ref; ident }
  | TypeRef { type_ref = ref; ident; _ } ->
      Format.fprintf fmt "@[%a::%s@]" print_ident_ref ref ident
  | _ -> failwith "Not implemented ident"

and typed_value fmt_value fmt t =
  match t.desc with
  | Pointer t ->
      typed_value (fun fmt -> Format.fprintf fmt "@[*%t@]" fmt_value) fmt t
  | BuiltinType Void ->
      Format.fprintf fmt "@[void@ %t@]" fmt_value
  | BuiltinType Int ->
      Format.fprintf fmt "@[int@ %t@]" fmt_value
  | ConstantArray { element; size } ->
      typed_value (fun fmt -> Format.fprintf fmt "@[%t[%d]@]" fmt_value size) fmt element
  | _ -> failwith (Format.asprintf "Not implemented qual type: %a" pp_qual_type t)

and qual_type fmt t =
  typed_value (fun fmt -> ()) fmt t

let translation_unit fmt (tu : translation_unit) =
  List.iter (decl fmt) tu.desc.items
