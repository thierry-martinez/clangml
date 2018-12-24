include Clang__bindings

let iter_children f c =
  let exn_ref = ref (None : exn option) in
  if
    visit_children c begin fun cur _par ->
      try
        f cur;
        Continue
      with exn ->
        exn_ref := Some exn;
        Break
    end
  then
    match !exn_ref with
    | None -> assert false
    | Some exn -> raise exn
  else
    assert (!exn_ref = None)

let list_of_children c =
  let children_ref = ref [] in
  c |> iter_children (fun cur -> children_ref := cur :: !children_ref);
  List.rev !children_ref

let iter_type_fields f ty =
  let exn_ref = ref (None : exn option) in
  assert (
    type_visit_fields ty begin fun cur ->
      try
        f cur;
        Continue
      with exn ->
        exn_ref := Some exn;
        Break
    end);
  match !exn_ref with
  | None -> ()
  | Some exn -> raise exn

let list_of_type_fields c =
  let fields_ref = ref [] in
  c |> iter_type_fields (fun cur -> fields_ref := cur :: !fields_ref);
  List.rev !fields_ref

let seq_of_diagnostics tu =
  let count = get_num_diagnostics tu in
  let rec next i () =
    if i < count then
      Seq.Cons (get_diagnostic tu i, next (succ i))
    else
      Seq.Nil in
  next 0

let is_error diagnostic_severity =
  match diagnostic_severity with
  | Error | Fatal -> true
  | _ -> false

let has_error tu =
  try
    seq_of_diagnostics tu |>
    Seq.iter (fun d -> if is_error (get_diagnostic_severity d) then raise Exit);
    false
  with Exit ->
    true

let int64_of_cxint_opt cxint =
  if ext_int_get_min_signed_bits cxint <= 64 then
    Some (ext_int_get_sext_value cxint)
  else
    None

let int64_of_cxint cxint =
  if ext_int_get_min_signed_bits cxint <= 64 then
    ext_int_get_sext_value cxint
  else
    failwith "int64_of_cxint"

let int_of_cxint_opt cxint =
  int64_of_cxint_opt cxint |> Option.map Int64.to_int

let int_of_cxint cxint =
  int64_of_cxint cxint |> Int64.to_int

module Ast = struct
  include Clang__ast

  let rec list_last l =
    match l with
    | [] -> failwith "list_last"
    | [last] -> [], last
    | hd :: tl ->
        let tl, last = list_last tl in
        hd :: tl, last

  module type OptionsS = sig
    val ignore_implicit_cast : bool

    val ignore_paren : bool

    val ignore_paren_in_types : bool
  end


  module Converter (Options : OptionsS) = struct
    let rec of_cxtype cxtype =
      let desc =
        match get_type_kind cxtype with
        | ConstantArray ->
            let element = cxtype |> get_array_element_type |> of_cxtype in
            let size = cxtype |> get_array_size in
            ConstantArray { element; size }
        | IncompleteArray ->
            let element = cxtype |> get_array_element_type |> of_cxtype in
            IncompleteArray { element }
        | VariableArray ->
            let element = cxtype |> get_array_element_type |> of_cxtype in
            let size =
              cxtype |> ext_variable_array_type_get_size_expr |>
              expr_of_cxcursor in
            VariableArray { element; size }
        | Pointer ->
            let pointee = cxtype |> get_pointee_type |> of_cxtype in
            Pointer pointee
        | Elaborated ->
            Elaborated {
              keyword = ext_elaborated_type_get_keyword cxtype;
              named_type = type_get_named_type cxtype |> of_cxtype;
            }
        | Enum ->
            let name = cxtype |> get_type_declaration |> get_cursor_spelling in
            Enum name
        | Record ->
            let name = cxtype |> get_type_declaration |> get_cursor_spelling in
            Record name
        | Typedef ->
            let name = cxtype |> get_type_declaration |> get_cursor_spelling in
            Typedef name
        | FunctionProto
        | FunctionNoProto ->
            let function_type =
              cxtype |> function_type_of_cxtype (fun _ -> "") in
            Function function_type
        | Complex ->
            let element_type = cxtype |> get_element_type |> of_cxtype in 
            Complex element_type
        | Unexposed ->
            begin
              match ext_get_type_kind cxtype with
              | Paren -> Paren (cxtype |> ext_get_inner_type |> of_cxtype)
              | _ -> OtherType (get_type_kind cxtype)
            end
        | _ -> OtherType (get_type_kind cxtype) in
      match desc with
      | Paren inner when Options.ignore_paren_in_types -> inner
      | _ ->
          { cxtype; desc;
            const = is_const_qualified_type cxtype;
            volatile = is_volatile_qualified_type cxtype;
            restrict = is_restrict_qualified_type cxtype; }

    and decl_of_cxcursor cxcursor =
      { cxcursor; desc = decl_desc_of_cxcursor cxcursor }

    and decl_desc_of_cxcursor cxcursor =
        match get_cursor_kind cxcursor with
        | FunctionDecl -> function_decl_of_cxcursor cxcursor
        | VarDecl -> Var (var_decl_desc_of_cxcursor cxcursor)
        | StructDecl -> struct_decl_of_cxcursor cxcursor
        | UnionDecl -> union_decl_of_cxcursor cxcursor
        | EnumDecl -> enum_decl_of_cxcursor cxcursor
        | TypedefDecl ->
            let name = get_cursor_spelling cxcursor in
            let underlying_type = cxcursor |>
            get_typedef_decl_underlying_type |> of_cxtype in
            Typedef { name; underlying_type }
        | _ -> OtherDecl

    and function_decl_of_cxcursor cxcursor =
      let linkage = cxcursor |> get_cursor_linkage in
      let function_type = cxcursor |> get_cursor_type |>
        function_type_of_cxtype @@ fun i ->
          cursor_get_argument cxcursor i |> get_cursor_spelling in
      let name = get_cursor_spelling cxcursor in
      let stmt =
        List.nth_opt (list_of_children cxcursor)
          (cursor_get_num_arguments cxcursor) |>
        Option.map stmt_of_cxcursor in
      Function { linkage; function_type; name; stmt }

    and function_type_of_cxtype get_argument_name cxtype =
      let calling_conv = cxtype |> get_function_type_calling_conv in
      let result = cxtype |> get_result_type |> of_cxtype in
      let args =
        if cxtype |> get_type_kind = FunctionProto then
          let non_variadic =
            List.init (get_num_arg_types cxtype) @@ fun i ->
              get_argument_name i, of_cxtype (get_arg_type cxtype i) in
          let variadic = is_function_type_variadic cxtype in
          Some { non_variadic; variadic }
        else
          None in
      { calling_conv; result; args }

    and var_decl_of_cxcursor cxcursor =
      { cxcursor; desc = var_decl_desc_of_cxcursor cxcursor }

    and var_decl_desc_of_cxcursor cxcursor =
      let linkage = cxcursor |> get_cursor_linkage in
      let name = get_cursor_spelling cxcursor in
      let qual_type = of_cxtype (get_cursor_type cxcursor) in
      let init =
        if ext_var_decl_has_init cxcursor then
          begin
            let _, init_value = list_last (list_of_children cxcursor) in
            Some (expr_of_cxcursor init_value)
          end
        else
          None in
      { linkage; name; qual_type; init }

    and enum_decl_of_cxcursor cxcursor =
      let name = get_cursor_spelling cxcursor in
      let constants =
        list_of_children cxcursor |> List.map @@ fun cxcursor ->
          match get_cursor_kind cxcursor with
          | EnumConstantDecl ->
              let name = get_cursor_spelling cxcursor in
              let init =
                match list_of_children cxcursor with
                | [init] -> Some (expr_of_cxcursor init)
                | [] -> None
                | _ -> failwith "enum_decl_of_cxcursor (init)" in
              { cxcursor; desc = { name; init } }
          | _ -> failwith "enum_decl_of_cxcursor" in
      Enum { name; constants }

    and struct_decl_of_cxcursor cxcursor =
      let name = get_cursor_spelling cxcursor in
      let fields = fields_of_cxcursor cxcursor in
      Struct { name; fields }

    and union_decl_of_cxcursor cxcursor =
      let name = get_cursor_spelling cxcursor in
      let fields = fields_of_cxcursor cxcursor in
      Union { name; fields }

    and fields_of_cxcursor cxcursor =
      list_of_children cxcursor |> List.map @@ fun cxcursor ->
        match get_cursor_kind cxcursor with
        | FieldDecl ->
            let name = get_cursor_spelling cxcursor in
            let qual_type = get_cursor_type cxcursor |> of_cxtype in
            let bitfield =
              if cursor_is_bit_field cxcursor then
                match list_of_children cxcursor with
                | [bitfield] -> Some (expr_of_cxcursor bitfield)
                | _ -> failwith "fields_of_cxcursor (bitfield)"
              else
                None in
            { cxcursor; desc = Named { name; qual_type; bitfield }}
        | UnionDecl ->
            { cxcursor; desc = AnonymousUnion (fields_of_cxcursor cxcursor)}
        | StructDecl ->
            { cxcursor; desc = AnonymousStruct (fields_of_cxcursor cxcursor)}
        | _ -> failwith "fields_of_cxcursor"

    and stmt_of_cxcursor cxcursor =
      let desc =
        match get_cursor_kind cxcursor with
        | NullStmt ->
            Null
        | CompoundStmt ->
            let items =
              cxcursor |> list_of_children |> List.map stmt_of_cxcursor in
            Compound items
        | ForStmt ->
            let children_set = ext_for_stmt_get_children_set cxcursor in
            let queue = Queue.create () in
            cxcursor |> iter_children (fun cur -> Queue.add cur queue);
            let init =
              if children_set land 1 <> 0 then
                Some (stmt_of_cxcursor (Queue.pop queue))
              else
                None in
            let condition_variable =
              if children_set land 2 <> 0 then
                Some (var_decl_of_cxcursor (Queue.pop queue))
              else
                None in
            let cond =
              if children_set land 4 <> 0 then
                Some (stmt_of_cxcursor (Queue.pop queue))
              else
                None in
            let inc =
              if children_set land 8 <> 0 then
                Some (stmt_of_cxcursor (Queue.pop queue))
              else
                None in
            let body = stmt_of_cxcursor (Queue.pop queue) in
            assert (Queue.is_empty queue);
            For { init; condition_variable; cond; inc; body }
        | IfStmt ->
            let children_set = ext_if_stmt_get_children_set cxcursor in
            let queue = Queue.create () in
            cxcursor |> iter_children (fun cur -> Queue.add cur queue);
            let init =
              if children_set land 1 <> 0 then
                Some (ext_if_stmt_get_init cxcursor |> stmt_of_cxcursor)
              else
                None in
            let condition_variable =
              if children_set land 2 <> 0 then
                Some (var_decl_of_cxcursor (Queue.pop queue))
              else
                None in
            let cond = expr_of_cxcursor (Queue.pop queue) in
            let then_branch = stmt_of_cxcursor (Queue.pop queue) in
            let else_branch =
              if children_set land 4 <> 0 then
                Some (stmt_of_cxcursor (Queue.pop queue))
              else
                None in
            assert (Queue.is_empty queue);
            If { init; condition_variable; cond; then_branch; else_branch }
        | SwitchStmt ->
            let children_set = ext_switch_stmt_get_children_set cxcursor in
            let queue = Queue.create () in
            cxcursor |> iter_children (fun cur -> Queue.add cur queue);
            let init =
              if children_set land 1 <> 0 then
                Some (ext_switch_stmt_get_init cxcursor |> stmt_of_cxcursor)
              else
                None in
            let condition_variable =
              if children_set land 2 <> 0 then
                Some (var_decl_of_cxcursor (Queue.pop queue))
              else
                None in
            let cond = expr_of_cxcursor (Queue.pop queue) in
            let body = stmt_of_cxcursor (Queue.pop queue) in
            assert (Queue.is_empty queue);
            Switch { init; condition_variable; cond; body }
        | CaseStmt ->
            let lhs, rhs, body =
              match list_of_children cxcursor with
              | [lhs; rhs; body] ->
                  lhs, Some (expr_of_cxcursor rhs), body
              | [lhs; body] ->
                  lhs, None, body
              | _ ->
                  failwith "stmt_of_cxcursor (CaseStmt)" in
            let lhs = expr_of_cxcursor lhs in
            let body = stmt_of_cxcursor body in
            Case { lhs; rhs; body }
        | DefaultStmt ->
            let body =
              match list_of_children cxcursor with
              | [body] -> stmt_of_cxcursor body
              | _ -> failwith "stmt_of_cxcursor (DefaultStmt)" in
            Default body
        | WhileStmt ->
            let children_set = ext_while_stmt_get_children_set cxcursor in
            let queue = Queue.create () in
            cxcursor |> iter_children (fun cur -> Queue.add cur queue);
            let condition_variable =
              if children_set land 1 <> 0 then
                Some (var_decl_of_cxcursor (Queue.pop queue))
              else
                None in
            let cond = expr_of_cxcursor (Queue.pop queue) in
            let body = stmt_of_cxcursor (Queue.pop queue) in
            assert (Queue.is_empty queue);
            While { condition_variable; cond; body }
        | DoStmt ->
            let body, cond =
              match list_of_children cxcursor with
              | [body; cond] -> stmt_of_cxcursor body, expr_of_cxcursor cond
              | _ -> failwith "stmt_of_cxcursor (DoStmt)" in
            Do { body; cond }
        | LabelStmt ->
            let label = cxcursor |> get_cursor_spelling in
            let body =
              match list_of_children cxcursor with
              | [body] -> stmt_of_cxcursor body
              | _ -> failwith "stmt_of_cxcursor (LabelStmt)" in
            Label { label; body }
        | GotoStmt ->
            let label =
              match list_of_children cxcursor with
              | [label] -> label |> get_cursor_spelling
              | _ -> failwith "stmt_of_cxcursor (GotoStmt)" in
            Goto label
        | IndirectGotoStmt ->
            let target =
              match list_of_children cxcursor with
              | [target] -> expr_of_cxcursor target
              | _ -> failwith "stmt_of_cxcursor (IndirectGotoStmt)" in
            IndirectGoto target
        | ContinueStmt ->
            Continue
        | BreakStmt ->
            Break
        | DeclStmt ->
            let decl = list_of_children cxcursor |> List.map decl_of_cxcursor in
            Decl decl
        | ReturnStmt ->
            let value =
              match list_of_children cxcursor with
              | [value] -> expr_of_cxcursor value
              | _ -> failwith "stmt_of_cxcursor (ReturnStmt)" in
            Return value
        | GCCAsmStmt ->
            let code = ext_asm_stmt_get_asm_string cxcursor in
            let parameters =
              list_of_children cxcursor |> List.map @@ fun cxcursor ->
                { cxcursor; desc = get_cursor_spelling cxcursor } in
            GCCAsm (code, parameters)
        | MSAsmStmt ->
            MSAsm (ext_asm_stmt_get_asm_string cxcursor)
        | _ -> Decl [{ cxcursor; desc = decl_desc_of_cxcursor cxcursor }] in
      match desc with
      | Decl [{ desc = OtherDecl }] ->
          let expr = expr_of_cxcursor cxcursor in
          { cxcursor = expr.cxcursor; desc = Expr expr.desc }
      | _ -> { cxcursor; desc }

    and expr_of_cxcursor cxcursor =
      match expr_desc_of_cxcursor cxcursor with
      | Paren subexpr when Options.ignore_paren -> subexpr
      | Cast { kind = Implicit; operand } when Options.ignore_implicit_cast ->
          operand
      | desc -> { cxcursor; desc }

    and expr_desc_of_cxcursor cxcursor =
      match get_cursor_kind cxcursor with
      | IntegerLiteral ->
          let i = ext_integer_literal_get_value cxcursor in
          IntegerLiteral i
      | FloatingLiteral ->
          let f = ext_floating_literal_get_value cxcursor in
          FloatingLiteral f
      | StringLiteral ->
          StringLiteral (ext_string_literal_get_string cxcursor)
      | UnaryOperator ->
          let operand =
            match list_of_children cxcursor with
            | [operand] -> expr_of_cxcursor operand
            | _ -> failwith "expr_of_cxcursor (UnaryOperator)" in
          let kind = ext_unary_operator_get_opcode cxcursor in
          UnaryOperator { kind; operand }
      | BinaryOperator ->
          let lhs, rhs =
            match list_of_children cxcursor with
            | [lhs; rhs] -> expr_of_cxcursor lhs, expr_of_cxcursor rhs
            | _ -> failwith "expr_of_cxcursor (BinaryOperator)" in
          let kind = ext_binary_operator_get_opcode cxcursor in
          BinaryOperator { lhs; kind; rhs }
      | DeclRefExpr -> DeclRef (get_cursor_spelling cxcursor)
      | CallExpr ->
          let f, args =
            match list_of_children cxcursor with
            | f :: args ->
                let f = f |> expr_of_cxcursor in
                let args = args |> List.map expr_of_cxcursor in
                f, args
            | _ -> failwith "expr_of_cxcursor (CallExpr)" in
          Call { f; args }
      | CStyleCastExpr ->
          let qual_type = get_cursor_type cxcursor |> of_cxtype in
          let operand =
            match list_of_children cxcursor with
            | [operand] -> expr_of_cxcursor operand
            | _ -> failwith "expr_of_cxcursor (CStyleCastExpr)" in
          Cast { kind = CStyle; qual_type; operand }
      | MemberRefExpr ->
          let base =
            match list_of_children cxcursor with
            | [lhs] -> expr_of_cxcursor lhs
            | _ -> failwith "expr_of_cxcursor" in
          let field = get_cursor_referenced cxcursor in
          let arrow = ext_member_ref_expr_is_arrow cxcursor in
          Member { base; arrow; field = {
            cxcursor = field; desc = get_cursor_spelling field }};
      | ArraySubscriptExpr ->
          let lhs, rhs =
            match list_of_children cxcursor with
            | [lhs; rhs] -> expr_of_cxcursor lhs, expr_of_cxcursor rhs
            | _ -> failwith "expr_of_cxcursor (ArraySubscriptExpr)" in
          ArraySubscript { lhs; rhs }
      | ConditionalOperator ->
          let cond, then_branch, else_branch =
            match list_of_children cxcursor |> List.map expr_of_cxcursor with
            | [cond; then_branch; else_branch] ->
                cond, Some then_branch, else_branch
            | _ -> failwith "expr_of_cxcursor (ConditionalOperator)" in
          ConditionalOperator { cond; then_branch; else_branch }
      | ParenExpr ->
          let subexpr =
            match list_of_children cxcursor with
            | [subexpr] -> expr_of_cxcursor subexpr
            | _ -> failwith "expr_of_cxcursor (ParenExpr)" in
          Paren subexpr
      | AddrLabelExpr ->
          let label =
            match list_of_children cxcursor with
            | [label] -> get_cursor_spelling label
            | _ -> failwith "expr_of_cxcursor (AddrLabelExpr)" in
          AddrLabel label
      | InitListExpr ->
          InitList (list_of_children cxcursor |> List.map expr_of_cxcursor)
      | CompoundLiteralExpr ->
          let qual_type = cxcursor |> get_cursor_type |> of_cxtype in
          let init =
            match list_of_children cxcursor with
            | [init] -> init |> expr_of_cxcursor
            | _ -> failwith "expr_of_cxcursor (CompoundLiteralExpr)" in
          CompoundLiteral { qual_type; init }
      | FirstExpr (* TODO: UnexposedExpr! *) ->
          begin
            match ext_get_cursor_kind cxcursor with
            | ImplicitCastExpr ->
                let operand =
                  match list_of_children cxcursor with
                  | [operand] -> expr_of_cxcursor operand
                  | _ -> failwith "expr_of_cxcursor (ImplicitCastExpr)" in
                let qual_type = get_cursor_type cxcursor |> of_cxtype in
                Cast { kind = Implicit; qual_type; operand }
            | BinaryConditionalOperator ->
                let cond, else_branch =
                  match
                    list_of_children cxcursor |> List.map expr_of_cxcursor with
                  | [_; cond; _; else_branch] ->
                      cond, else_branch
                  | _ ->
                      failwith "expr_of_cxcursor (BinaryConditionalOperator)" in
                ConditionalOperator { cond; then_branch = None; else_branch }
            | Unknown -> UnexposedExpr { s = get_cursor_spelling cxcursor }
          end
      | _ -> OtherExpr

    let translation_unit_of_cxcursor cxcursor =
      let filename = get_cursor_spelling cxcursor in
      let items = list_of_children cxcursor |> List.map decl_of_cxcursor in
      { cxcursor; desc = { filename; items } }

    let of_cxtranslationunit tu =
      translation_unit_of_cxcursor (get_translation_unit_cursor tu)
  end

  let of_cxtype
      ?(ignore_implicit_cast = true) ?(ignore_paren = true)
      ?(ignore_paren_in_types = true)
      tu =
    let module Convert = Converter (struct
      let ignore_implicit_cast = ignore_implicit_cast
      let ignore_paren = ignore_paren
      let ignore_paren_in_types = ignore_paren_in_types
     end) in
    Convert.of_cxtype tu

  let expr_of_cxcursor
      ?(ignore_implicit_cast = true) ?(ignore_paren = true)
      ?(ignore_paren_in_types = true)
      cur =
    let module Convert = Converter (struct
      let ignore_implicit_cast = ignore_implicit_cast
      let ignore_paren = ignore_paren
      let ignore_paren_in_types = ignore_paren_in_types
    end) in
    Convert.expr_of_cxcursor cur

  let stmt_of_cxcursor
      ?(ignore_implicit_cast = true) ?(ignore_paren = true)
      ?(ignore_paren_in_types = true)
      cur =
    let module Convert = Converter (struct
      let ignore_implicit_cast = ignore_implicit_cast
      let ignore_paren = ignore_paren
      let ignore_paren_in_types = ignore_paren_in_types
    end) in
    Convert.stmt_of_cxcursor cur

  let of_cxtranslationunit
      ?(ignore_implicit_cast = true) ?(ignore_paren = true)
      ?(ignore_paren_in_types = true)
      tu =
    let module Convert = Converter (struct
      let ignore_implicit_cast = ignore_implicit_cast
      let ignore_paren = ignore_paren
      let ignore_paren_in_types = ignore_paren_in_types
    end) in
    Convert.of_cxtranslationunit tu
end

let string_of_cxerrorcode cxerrorcode =
  match cxerrorcode with
  | Failure -> "generic error code, no further details are available"
  | Crashed -> "libclang crashed while performing the requested operation"
  | InvalidArguments -> "the arguments violate the function contract"
  | ASTReadError -> "an AST deserialization error has occurred"

let parse_file ?(index = create_index true true)
    ?(command_line_args = []) ?(unsaved_files = [])
    ?(options = default_editing_translation_unit_options ()) filename =
  match
    parse_translation_unit2 index filename (Array.of_list command_line_args)
      (Array.of_list unsaved_files) options
  with
  | Ok cxtranslationunit -> cxtranslationunit
  | Error cxerrorcode -> failwith (string_of_cxerrorcode cxerrorcode)

let parse_string ?index ?(filename = "<string>.c")
    ?command_line_args ?(unsaved_files = [])
    ?options contents =
  parse_file ?index ?command_line_args
    ~unsaved_files:({ filename; contents } :: unsaved_files)
    ?options filename
