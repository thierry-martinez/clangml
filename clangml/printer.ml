module Make (Node : Clang__ast.NodeS) = struct
  module Ast = Clang__ast.Custom (Node)

  let string_of_builtin_type (ty : Clang__ast.builtin_type) =
    match ty with
    | Void -> "void"
    | Bool -> "bool"
    | Char_S
    | SChar -> "char"
    | UChar -> "unsigned char"
    | Float -> "float"
    | Int -> "int"
    | UInt -> "unsigned int"
    | Long -> "long"
    | ULong -> "unsigned long"
    | LongLong -> "long long"
    | Short -> "short"
    | UShort -> "unsigned short"
    | Double -> "double"
    | _ ->
        Printf.sprintf "<unknown builtin type:%s>"
          (Clang__bindings.get_type_kind_spelling ty)

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

  let prec_of_unary_operator (kind : Clang__ast.unary_operator_kind) =
    match kind with
    | PostInc | PostDec -> 1, Postfix
    | PreInc | PreDec -> 1, Prefix
    | AddrOf
    | Deref
    | Plus | Minus
    | Not | LNot -> 2, Prefix
    | _ -> invalid_arg "prec_of_unary_operator"

  let prec_of_binary_operator (kind : Clang__ast.binary_operator_kind) =
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

  let pp_comma fmt () =
    Format.fprintf fmt ",@ "

  let pp_typedef pp_sub fmt name =
    Format.fprintf fmt "@[typedef@ %t@ %s@]" pp_sub name

  let rec decl fmt (d : Ast.decl) =
    match Node.force d.desc with
    | Function { storage; function_type; name; body; _ } ->
        Format.fprintf fmt "@[<v>%a%a%a@]"
          pp_storage storage
          pp_function_type (function_type, name)
          pp_function_body body
    | Var var_decl ->
        Format.fprintf fmt "@[%a@]" pp_var_decl var_decl
    | EnumDecl { name; constants; _ } ->
        let pp_constant fmt (constant : Ast.enum_constant) =
          let desc = Node.force constant.desc in
          Format.pp_print_string fmt desc.constant_name;
          desc.constant_init |> Option.iter (fun init ->
            Format.fprintf fmt "@ =@ %a" expr init) in
        Format.fprintf fmt "@[<v>@[enum@ %s@ {@]@,%a}@]" name
          (Format.pp_print_list ~pp_sep:pp_comma pp_constant) constants
    | RecordDecl { keyword; name; fields; _ } ->
        Format.fprintf fmt "@[<v>@[%s@ %s@ {@]@,%a}@]"
          (Clang__bindings.ext_elaborated_type_get_keyword_spelling keyword)
          name
          decls
          fields
    | Field { name; qual_type = ty; bitwidth; _ } ->
        Format.fprintf fmt "@[%a%t@]"
          (typed_value (fun fmt -> Format.pp_print_string fmt name)) ty
          (fun fmt ->
            match bitwidth with
            | None -> ()
            | Some bitwidth -> Format.fprintf fmt "@ :@ %a" expr bitwidth)
    | AccessSpecifier specifier ->
        Format.fprintf fmt "%s:" (Clang__utils.string_of_cxx_access_specifier specifier)
    | Constructor { class_name; parameters; initializer_list; body; explicit; defaulted; deleted; _ } ->
        Format.fprintf fmt "%t%s(%a)%t%t%a"
          (fun fmt ->
            if explicit then Format.fprintf fmt "explicit@ ")
          class_name
          pp_parameters parameters
          (fun fmt ->
            match initializer_list with
            | [] -> ()
            | _ ->
                Format.fprintf fmt "@ :@ ";
                Format.pp_print_list ~pp_sep:pp_comma
                  (fun fmt ({ kind; init } : Ast.constructor_initializer) ->
                    let name fmt =
                      match kind with
                      | Base { qual_type; _ } | Delegating qual_type ->
                          begin
                            match Node.force qual_type.desc with
                            | Record name -> pp_ident_ref fmt name
                            | _ -> assert false
                          end
                      | Member { field; _ } ->
                          Format.pp_print_string fmt (Node.force field.desc) in
                    Format.fprintf fmt "%t(%a)" name expr init)
                  fmt initializer_list)
          (fun fmt ->
            if defaulted then
              Format.fprintf fmt "@ =@ default";
            if deleted then
              Format.fprintf fmt "@ =@ delete")
          pp_function_body body
    | Directive (Include { program_context; filename }) ->
        let pp_arg fmt =
          if program_context then
            Format.fprintf fmt "\"%s\"" filename
          else
            Format.fprintf fmt "<%s>" filename in
        Format.fprintf fmt "@\n#include %t@\n" pp_arg
    | Directive (Ifdef var) ->
        Format.fprintf fmt "@\n#ifdef %s@\n" var
    | Directive (Ifndef var) ->
        Format.fprintf fmt "@\n#ifndef %s@\n" var
    | Directive Endif ->
        Format.fprintf fmt "@\n#endif@\n"
    | LinkageSpec { language; decls = list } ->
        Format.fprintf fmt "@[extern@ \"%s\"@ {@ @[%a@]@ }@]"
          (Clang__utils.extern_of_language language)
          decls list
    | TypedefDecl { name; underlying_type } ->
        pp_typedef (fun fmt -> pp_qual_type fmt underlying_type) fmt name
    | _ ->
        Format.fprintf fmt {|@[\<not implemented decl: %a>@]|}
          (Refl.pp [%refl: Ast.decl] []) d

  and pp_var_decl fmt (var_decl : Ast.var_decl_desc) =
    match var_decl with { storage; var_type = ty; var_name; var_init; _ } ->
    Format.fprintf fmt "@[%a%a%a@]"
      pp_storage storage
      (typed_value (fun fmt -> Format.pp_print_string fmt var_name)) ty
      pp_variable_init var_init

  and pp_variable_init fmt init =
    match init with
    | None -> ()
    | Some value ->
        Format.fprintf fmt "@ =@ %a" expr value

  and expr fmt e =
    expr_prec 15 fmt e

  and expr_prec prec fmt (e : Ast.expr) =
    match Node.force e.desc with
    | IntegerLiteral i ->
        Clang__ast_utils.pp_print_integer_literal fmt i
    | FloatingLiteral f ->
        Clang__ast_utils.pp_print_floating_literal fmt f
    | CharacterLiteral { kind; value } ->
        begin
          match kind with
          | Ascii ->
              Format.fprintf fmt "'%a'" c_escape_char (char_of_int value)
          | _ -> failwith "Not implemented character kind"
        end
    | StringLiteral { bytes; _ } ->
        Format.pp_print_string fmt "\"";
        String.iter (c_escape_char fmt) bytes;
        Format.pp_print_string fmt "\""
    | BoolLiteral b ->
        Format.pp_print_bool fmt b
    | ArraySubscript { base; index } ->
        maybe_parentheses 1 prec fmt (fun fmt ->
          Format.fprintf fmt "@[%a[@[%a@]]@]" (expr_prec 1) base (expr_prec 15)
            index)

    | Call { callee; args } ->
        begin match Node.force callee.desc, args with
        | DeclRef { name = OperatorName kind; _ }, [lhs; rhs] ->
            let kind = Clang__utils.binary_of_overloaded_operator_kind kind in
            let desc : Ast.expr_desc = BinaryOperator { lhs; kind; rhs } in
            expr_prec prec fmt { e with desc = Node.from_val desc}
        | _ ->
            maybe_parentheses 1 prec fmt (fun fmt ->
                Format.fprintf fmt "@[%a(@[%a@])@]" (expr_prec 1) callee
                  (Format.pp_print_list ~pp_sep:pp_comma
                     (expr_prec 15)) args)
        end
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
          | Left_to_right -> op_prec + 1, op_prec
          | Right_to_left -> op_prec, op_prec + 1 in
        maybe_parentheses op_prec prec fmt (fun fmt ->
          Format.fprintf fmt "@[%a@ %s@ %a@]" (expr_prec left_prec) lhs
            (Clang__bindings.ext_binary_operator_get_opcode_spelling kind)
            (expr_prec right_prec) rhs)
    | DeclRef ident_ref ->
        pp_ident_ref fmt ident_ref
    | Member { base = None; field = FieldName field; _ } ->
        pp_ident_ref fmt (Node.force field.desc)
    | Member { base = Some base; arrow; field } ->
        let arrow_str =
          if arrow then "->"
          else "." in
        let pp_field fmt (field : Ast.field) =
          match field with
          | FieldName name -> pp_ident_ref fmt (Node.force name.desc)
          | _ -> invalid_arg "print_field" in
        Format.fprintf fmt "@[%a%s%a@]" expr base arrow_str
          pp_field field
    | Cast { kind = CStyle; qual_type = ty; operand } ->
        maybe_parentheses 2 prec fmt (fun fmt ->
          Format.fprintf fmt "@[(%a)@ %a@]" pp_qual_type ty (expr_prec 4) operand)
    | New { qual_type = ty; init; _ } ->
        let format_init fmt (init : Ast.expr option) =
          match init with
          | None -> ()
          | Some init ->
              match Node.force init.desc with
              | Construct { args; _ } ->
                  Format.fprintf fmt "@[(%a)@]"
                    (Format.pp_print_list ~pp_sep:pp_comma expr) args
              | _ ->
                  failwith
                    (Format.asprintf "Unexpected constructor argument %a"
                       (Refl.pp [%refl: Ast.expr] []) e) in
        Format.fprintf fmt "new@ %a%a" pp_qual_type ty format_init init
    | Delete { argument; _ } ->
        Format.fprintf fmt "delete@ %a" expr argument
    | ConditionalOperator { cond; then_branch; else_branch } ->
        begin match then_branch with
        | Some then_branch ->
            Format.fprintf fmt "%a ? %a : %a" expr cond expr then_branch expr
              else_branch
        | None ->
            Format.fprintf fmt "%a ?: %a" expr cond expr else_branch
        end
    | _ ->
        Format.fprintf fmt "<not implemented expr: %a>"
          (Refl.pp [%refl: Ast.expr] []) e

  and pp_condition_variable fmt
      ((condition_variable : Ast.var_decl option), cond) =
    match condition_variable with
    | Some condition_variable ->
        pp_var_decl fmt (Node.force condition_variable.desc)
    | None -> expr fmt cond

  and stmt fmt (s : Ast.stmt) =
    match Node.force s.desc with
    | Null -> Format.pp_print_string fmt ";"
    | Break -> Format.pp_print_string fmt "break;"
    | Case { lhs; rhs; body } ->
        let pp_rhs fmt =
          match rhs with
          | None -> ()
          | Some rhs -> Format.fprintf fmt " .. %a" expr rhs in
        Format.fprintf fmt "@[case@ %a%t:@]@ %a" expr lhs pp_rhs stmt body
    | Default body ->
        Format.fprintf fmt "default:@ %a" stmt body
    | Switch { condition_variable; cond; body; _ } ->
        Format.fprintf fmt "@[switch@ (@[%a@])@ %a@]"
          pp_condition_variable (condition_variable, cond) stmt body
    | Compound list ->
        Format.fprintf fmt "@[{@[<v>%a@]}@]" (Format.pp_print_list stmt) list
    | If { condition_variable; cond; then_branch; else_branch; _ } ->
        Format.fprintf fmt "@[if@ (@[%a@])@ %a%a@]"
          pp_condition_variable (condition_variable, cond) stmt then_branch
          pp_else_branch else_branch
    | While { cond; body; _ } ->
        Format.fprintf fmt "@[while@ (@[%a@])@ %a@]"
          expr cond stmt body
    | For { init; condition_variable; cond; inc; body } ->
        Format.fprintf fmt "@[for@ (@[%t@];@ @[%t@];@ @[%t@])@ %a@]"
          (fun fmt -> init |> Option.iter @@ stmt_without_semicolon fmt)
          (fun fmt -> cond |> Option.iter @@ fun cond ->
            pp_condition_variable fmt (condition_variable, cond))
          (fun fmt -> inc |> Option.iter @@ stmt_without_semicolon fmt)
          stmt body
    | ForRange { var = { desc; _}; range; body } ->
        let { var_name; var_type; _ } : Ast.var_decl_desc = Node.force desc in
        Format.fprintf fmt "@[for@ (@[%a@ :@ %a@])@ %a@]"
          (typed_value (fun fmt -> Format.pp_print_string fmt var_name))
          var_type expr range stmt body
    | Return None ->
        Format.fprintf fmt "@[return;@]"
    | Return (Some value) ->
        Format.fprintf fmt "@[return@ %a;@]" expr value
    | Decl d ->
        decls fmt d
    | Expr e ->
        Format.fprintf fmt "@[%a;@]" expr e
    | _ ->
        Format.fprintf fmt "<not implemented stmt: %a>"
          (Refl.pp [%refl: Ast.stmt] []) s

  and stmt_without_semicolon fmt (s : Ast.stmt) =
    match Node.force s.desc with
    | Decl [{ desc; _ }] ->
        begin match Node.force desc with
        | Var var_decl ->
            pp_var_decl fmt var_decl
        | _ ->
            Format.fprintf fmt "<not implemented stmt_without_semicolon: %a>"
              (Refl.pp [%refl: Ast.stmt] []) s
        end
    | Expr e ->
        expr fmt e
    | _ ->
        Format.fprintf fmt "<not implemented stmt_without_semicolon: %a>"
          (Refl.pp [%refl: Ast.stmt] []) s

  and pp_else_branch fmt else_branch =
    match else_branch with
    | None -> ()
    | Some else_branch ->
        Format.fprintf fmt "@[else@ %a@]" stmt else_branch

  and pp_storage fmt (storage : Clang__bindings.cx_storageclass) =
    match storage with
    | None -> ()
    | Extern -> Format.fprintf fmt "extern@ "
    | Static -> Format.fprintf fmt "static@ "
    | _ ->
        failwith (Format.asprintf "Not implemented storage: %a"
          (Refl.pp [%refl: Clang__bindings.cx_storageclass] []) storage)

  and pp_parameters fmt parameters =
    let all_parameters = List.map Option.some parameters.non_variadic in
    let all_parameters =
      if parameters.variadic then all_parameters @ [None] else all_parameters in
    let pp_parameter fmt (parameter : Ast.parameter option) =
      match parameter with
      | None -> Format.pp_print_string fmt "..."
      | Some { desc; _ } ->
          let { name; qual_type = ty; _ } : Ast.parameter_desc =
            Node.force desc in
          typed_value (fun fmt -> Format.pp_print_string fmt name) fmt ty in
    Format.pp_print_list
      ~pp_sep:pp_comma
      pp_parameter fmt all_parameters

  and pp_function_type fmt (function_type, name) =
    typed_value
      (fun fmt -> Format.fprintf fmt "@[%a(@]@,@[%a)@]" pp_declaration_name name
          (pp_print_option pp_parameters) function_type.parameters)
      fmt function_type.result

  and pp_function_body fmt body =
    match body with
    | None -> Format.pp_print_string fmt ";"
    | Some body -> Format.fprintf fmt "@ %a" stmt body

  and pp_ident_ref fmt ident_ref =
    let pp_nested_name_specifier_component
        (component : Ast.nested_name_specifier_component) =
      match component with
      | Global -> Format.pp_print_string fmt "::"
      | NestedIdentifier s -> Format.fprintf fmt "%s::" s
      | NamespaceName name
      | NamespaceAliasName name ->
          Format.fprintf fmt "%s::" name
      | TypeSpec ty
      | TypeSpecWithTemplate ty ->
          Format.fprintf fmt "%a::" pp_qual_type ty in
    Option.iter (List.iter pp_nested_name_specifier_component)
      ident_ref.nested_name_specifier;
    pp_declaration_name fmt ident_ref.name;
    if ident_ref.template_arguments <> [] then
      Format.fprintf fmt "<%a>"
        (Format.pp_print_list ~pp_sep:pp_comma pp_template_argument)
        ident_ref.template_arguments

  and pp_template_argument fmt (argument : Ast.template_argument) =
    match argument with
    | Type ty -> pp_qual_type fmt ty
    | ExprTemplateArgument e -> expr fmt e
    | _ ->
        Format.fprintf fmt "@[<not implemented template argument: %a>@]"
          (Refl.pp [%refl: Ast.template_argument] []) argument

  and pp_declaration_name fmt (name : Ast.declaration_name) =
    match name with
    | IdentifierName s
    | LiteralOperatorName s -> Format.pp_print_string fmt s
    | ConstructorName ty
    | ConversionFunctionName ty -> pp_qual_type fmt ty
    | DestructorName ty -> Format.fprintf fmt "~%a" pp_qual_type ty
    | OperatorName op ->
        Format.fprintf fmt "operator%s"
          (Clang__bindings.ext_overloaded_operator_get_spelling op)
    | _ -> failwith "Not implemented pp_ident_ref.declaration_name"

  and typed_value fmt_value fmt t =
    let fmt_value =
      if t.const then
        (fun fmt -> Format.fprintf fmt "@[const %t@]" fmt_value)
      else
        fmt_value in
    match Node.force t.desc with
    | Pointer t ->
        typed_value (fun fmt -> Format.fprintf fmt "@[*%t@]" fmt_value) fmt t
    | LValueReference t ->
        typed_value (fun fmt -> Format.fprintf fmt "@[&%t@]" fmt_value) fmt t
    | RValueReference t ->
        typed_value (fun fmt -> Format.fprintf fmt "@[&&%t@]" fmt_value) fmt t
    | BuiltinType ty ->
        Format.fprintf fmt "@[%s@ %t@]" (string_of_builtin_type ty) fmt_value
    | ConstantArray { element; size_as_expr = Some size_as_expr; _ } ->
        typed_value
          (fun fmt ->
            Format.fprintf fmt "@[%t[%a]@]" fmt_value expr size_as_expr)
          fmt element
    | ConstantArray { element; size_as_expr = None; size } ->
        typed_value
          (fun fmt -> Format.fprintf fmt "@[%t[%d]@]" fmt_value size) fmt
          element
    | Elaborated { keyword; named_type; _ } ->
        Format.fprintf fmt "@[%s@ %a@]"
          (Clang__bindings.ext_elaborated_type_get_keyword_spelling keyword)
          (typed_value fmt_value) named_type
    | Record ident
    | Enum ident
    | Typedef ident ->
        Format.fprintf fmt "@[%a@ %t@]" pp_ident_ref ident fmt_value
    | Auto ->
        Format.fprintf fmt "@[auto@ %t@]" fmt_value
    | FunctionType { result; parameters; _ } ->
        typed_value (fun fmt -> Format.fprintf fmt "@[(%t)(%t)@]"
            fmt_value (fun fmt -> parameters |> Option.iter @@ pp_parameters fmt))
          fmt result
    | _ ->
        Format.fprintf fmt {|@[\<not implemented qual type: %a>@]|}
          (Refl.pp [%refl: Ast.qual_type] []) t

  and pp_qual_type fmt t =
    typed_value (fun _fmt -> ()) fmt t

  and decls fmt list =
    let rec aux fmt (list : Ast.decl list) =
      match list with
      | [] -> ()
      | hd :: tl ->
          let default_case () =
            begin match Node.force hd.desc with
            | Function _ ->
                Format.fprintf fmt "@[%a@]@ " decl hd;
            | _ ->
                Format.fprintf fmt "@[%a;@]@ " decl hd
            end;
            aux fmt tl in
          match Node.force hd.desc, tl with
          | (EnumDecl _ | RecordDecl { keyword = Struct; _ }) as desc,
            (hd' :: tl) ->
              begin match Node.force hd'.desc with
              | TypedefDecl { name = typedef_name; underlying_type } ->
                  begin match Node.force underlying_type.desc with
                  | Elaborated { keyword; named_type; _ } ->
                      begin match desc, keyword, Node.force named_type.desc with
                      | EnumDecl { name = enum_name; _ },
                        Enum,
                        Enum { name = IdentifierName ref_name; _ }
                        when enum_name = ref_name ->
                          Format.fprintf fmt "@[%a;@]@ "
                            (pp_typedef (fun fmt -> decl fmt hd)) typedef_name;
                          Format.pp_print_space fmt ();
                          aux fmt tl
                      | RecordDecl { name = struct_name; _ },
                        Struct,
                        Record { name = IdentifierName ref_name; _ }
                        when struct_name = ref_name ->
                          Format.fprintf fmt "@[%a;@]@ "
                            (pp_typedef (fun fmt -> decl fmt hd)) typedef_name;
                          aux fmt tl
                      | _ -> default_case ()
                      end
                  | _ -> default_case ()
                  end
              | _ -> default_case ()
              end
          | _ -> default_case () in
    Format.fprintf fmt "@[<v>%a@]" aux list

  let translation_unit fmt (tu : Ast.translation_unit) =
    decls fmt (Node.force tu.desc).items

  let qual_type = pp_qual_type
end
