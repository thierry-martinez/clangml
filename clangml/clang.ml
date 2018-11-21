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

module Ast = struct
  include Clang__ast

  let label_ref_of_cursor cxcursor =
    let desc = { name = get_cursor_spelling cxcursor } in
    { cxcursor; desc }

  let rec list_last l =
    match l with
    | [] -> failwith "list_last"
    | [last] -> [], last
    | hd :: tl ->
        let tl, last = list_last tl in
        hd :: tl, last

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
      | Pointer ->
          let pointee = cxtype |> get_pointee_type |> of_cxtype in
          Pointer { pointee }
      | Elaborated ->
          Elaborated {
            keyword = ext_elaborated_type_get_keyword cxtype;
            named_type = type_get_named_type cxtype |> of_cxtype;
          }
      | Enum ->
          let name = cxtype |> get_type_declaration |> get_cursor_spelling in
          Enum { name }
      | Record ->
          let name = cxtype |> get_type_declaration |> get_cursor_spelling in
          Struct { name }
      | Typedef ->
          let name = cxtype |> get_type_declaration |> get_cursor_spelling in
          Typedef { name }
      | FunctionProto
      | FunctionNoProto ->
          let function_type = cxtype |> function_type_of_cxtype (fun _ -> "") in
          Function function_type
      | _ -> OtherType { kind = get_type_kind cxtype } in
    { cxtype; desc;
      const = is_const_qualified_type cxtype;
      volatile = is_volatile_qualified_type cxtype;
      restrict = is_restrict_qualified_type cxtype; }

  and decl_of_cursor cxcursor =
    { cxcursor; desc = decl_desc_of_cursor cxcursor }

  and decl_desc_of_cursor cxcursor =
      match get_cursor_kind cxcursor with
      | FunctionDecl -> function_decl_of_cursor cxcursor
      | VarDecl -> Var (var_decl_desc_of_cursor cxcursor)
      | StructDecl -> struct_decl_of_cursor cxcursor
      | EnumDecl -> enum_decl_of_cursor cxcursor
      | TypedefDecl ->
          let name = get_cursor_spelling cxcursor in
          let underlying_type = cxcursor |>
          get_typedef_decl_underlying_type |> of_cxtype in
          Typedef { name; underlying_type }
      | _ -> OtherDecl

  and function_decl_of_cursor cxcursor =
    let linkage = cxcursor |> get_cursor_linkage in
    let function_type = cxcursor |> get_cursor_type |>
      function_type_of_cxtype @@ fun i ->
        cursor_get_argument cxcursor i |> get_cursor_spelling in
    let name = get_cursor_spelling cxcursor in
    let stmt =
      List.nth_opt (list_of_children cxcursor)
        (cursor_get_num_arguments cxcursor) |>
      Option.map stmt_of_cursor in
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

  and var_decl_of_cursor cxcursor =
    { cxcursor; desc = var_decl_desc_of_cursor cxcursor }

  and var_decl_desc_of_cursor cxcursor =
    let linkage = cxcursor |> get_cursor_linkage in
    let name = get_cursor_spelling cxcursor in
    let qual_type = of_cxtype (get_cursor_type cxcursor) in
    let init =
      if ext_var_decl_has_init cxcursor then
        begin
          let _, init_value = list_last (list_of_children cxcursor) in
          Some (expr_of_cursor init_value)
        end
      else
        None in
    { linkage; name; qual_type; init }

  and enum_decl_of_cursor cxcursor =
    let name = get_cursor_spelling cxcursor in
    let constants =
      list_of_children cxcursor |> List.map @@ fun cxcursor ->
        match get_cursor_kind cxcursor with
        | EnumConstantDecl ->
            let name = get_cursor_spelling cxcursor in
            let init =
              match list_of_children cxcursor with
              | [init] -> Some (expr_of_cursor init)
              | [] -> None
              | _ -> failwith "enum_decl_of_cursor (init)" in
            { cxcursor; desc = { name; init } }
        | _ -> failwith "enum_decl_of_cursor" in
    Enum { name; constants }

  and struct_decl_of_cursor cxcursor =
    let name = get_cursor_spelling cxcursor in
    let fields =
      list_of_children cxcursor |> List.map @@ fun cur ->
        match get_cursor_kind cur with
        | FieldDecl ->
            let name = get_cursor_spelling cur in
            let qual_type = get_cursor_type cur |> of_cxtype in
            name, qual_type
        | _ -> failwith "struct_decl_of_cursor" in
    Struct { name; fields }

  and stmt_of_cursor cxcursor =
    let desc =
      match get_cursor_kind cxcursor with
      | NullStmt ->
          Null
      | CompoundStmt ->
          let items = cxcursor |> list_of_children |> List.map stmt_of_cursor in
          Compound { items }
      | ForStmt ->
          let children_set = ext_for_stmt_get_children_set cxcursor in
          let queue = Queue.create () in
          cxcursor |> iter_children (fun cur -> Queue.add cur queue);
          let init =
            if children_set land 1 <> 0 then
              Some (stmt_of_cursor (Queue.pop queue))
            else
              None in
          let condition_variable =
            if children_set land 2 <> 0 then
              Some (var_decl_of_cursor (Queue.pop queue))
            else
              None in
          let cond =
            if children_set land 4 <> 0 then
              Some (stmt_of_cursor (Queue.pop queue))
            else
              None in
          let inc =
            if children_set land 8 <> 0 then
              Some (stmt_of_cursor (Queue.pop queue))
            else
              None in
          let body = stmt_of_cursor (Queue.pop queue) in
          assert (Queue.is_empty queue);
          For { init; condition_variable; cond; inc; body }
      | IfStmt ->
          let children_set = ext_if_stmt_get_children_set cxcursor in
          let queue = Queue.create () in
          cxcursor |> iter_children (fun cur -> Queue.add cur queue);
          let init =
            if children_set land 1 <> 0 then
              Some (stmt_of_cursor (Queue.pop queue))
            else
              None in
          let condition_variable =
            if children_set land 2 <> 0 then
              Some (var_decl_of_cursor (Queue.pop queue))
            else
              None in
          let cond = expr_of_cursor (Queue.pop queue) in
          let then_branch = stmt_of_cursor (Queue.pop queue) in
          let else_branch =
            if children_set land 4 <> 0 then
              Some (stmt_of_cursor (Queue.pop queue))
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
              Some (stmt_of_cursor (Queue.pop queue))
            else
              None in
          let condition_variable =
            if children_set land 2 <> 0 then
              Some (var_decl_of_cursor (Queue.pop queue))
            else
              None in
          let cond = expr_of_cursor (Queue.pop queue) in
          let body = stmt_of_cursor (Queue.pop queue) in
          assert (Queue.is_empty queue);
          Switch { init; condition_variable; cond; body }
      | CaseStmt ->
          let lhs, rhs, body =
            match list_of_children cxcursor with
            | [lhs; rhs; body] ->
                lhs, Some (expr_of_cursor rhs), body
            | [lhs; body] ->
                lhs, None, body
            | _ ->
                failwith "stmt_of_cursor (CaseStmt)" in
          let lhs = expr_of_cursor lhs in
          let body = stmt_of_cursor body in
          Case { lhs; rhs; body }
      | DefaultStmt ->
          let body =
            match list_of_children cxcursor with
            | [body] -> stmt_of_cursor body
            | _ -> failwith "stmt_of_cursor (DefaultStmt)" in
          Default { body }
      | WhileStmt ->
          let children_set = ext_while_stmt_get_children_set cxcursor in
          let queue = Queue.create () in
          cxcursor |> iter_children (fun cur -> Queue.add cur queue);
          let condition_variable =
            if children_set land 1 <> 0 then
              Some (var_decl_of_cursor (Queue.pop queue))
            else
              None in
          let cond = expr_of_cursor (Queue.pop queue) in
          let body = stmt_of_cursor (Queue.pop queue) in
          assert (Queue.is_empty queue);
          While { condition_variable; cond; body }
      | DoStmt ->
          let body, cond =
            match list_of_children cxcursor with
            | [body; cond] -> stmt_of_cursor body, expr_of_cursor cond
            | _ -> failwith "stmt_of_cursor (DoStmt)" in
          Do { body; cond }
      | LabelStmt ->
          let label, body =
            match list_of_children cxcursor with
            | [label; body] -> label_ref_of_cursor label, stmt_of_cursor body
            | _ -> failwith "stmt_of_cursor (LabelStmt)" in
          Label { label; body }
      | GotoStmt ->
          let label =
            match list_of_children cxcursor with
            | [label] -> label_ref_of_cursor label
            | _ -> failwith "stmt_of_cursor (GotoStmt)" in
          Goto { label }
      | IndirectGotoStmt ->
          let target =
            match list_of_children cxcursor with
            | [target] -> expr_of_cursor target
            | _ -> failwith "stmt_of_cursor (IndirectGotoStmt)" in
          IndirectGoto { target }
      | ContinueStmt ->
          Continue
      | BreakStmt ->
          Break
      | DeclStmt ->
          let decl = list_of_children cxcursor |> List.map decl_of_cursor in
          Decl decl
      | ReturnStmt ->
          let value =
            match list_of_children cxcursor with
            | [value] -> expr_of_cursor value
            | _ -> failwith "stmt_of_cursor (ReturnStmt)" in
          Return { value }
      | _ ->
          match decl_desc_of_cursor cxcursor with
          | OtherDecl -> Expr (expr_desc_of_cursor cxcursor)
          | desc -> Decl [{ cxcursor; desc }] in
    { cxcursor; desc }

  and expr_of_cursor cxcursor =
    { cxcursor; desc = expr_desc_of_cursor cxcursor }

  and expr_desc_of_cursor cxcursor =
    match get_cursor_kind cxcursor with
    | IntegerLiteral ->
        IntegerLiteral {
          s = ext_integer_literal_get_value_as_string cxcursor 10 true;
        }
    | UnaryOperator ->
        let operand =
          match list_of_children cxcursor with
          | [operand] -> expr_of_cursor operand
          | _ -> failwith "expr_of_cursor (UnaryOperator)" in
        let kind = ext_unary_operator_get_opcode cxcursor in
        UnaryOperator { kind; operand }
    | BinaryOperator ->
        let lhs, rhs =
          match list_of_children cxcursor with
          | [lhs; rhs] -> expr_of_cursor lhs, expr_of_cursor rhs
          | _ -> failwith "expr_of_cursor (BinaryOperator)" in
        let kind = ext_binary_operator_get_opcode cxcursor in
        BinaryOperator { lhs; kind; rhs }
    | DeclRefExpr -> DeclRef { s = get_cursor_spelling cxcursor }
    | CallExpr ->
        let f, args =
          match list_of_children cxcursor with
          | f :: args ->
              let f = f |> expr_of_cursor in
              let args = args |> List.map expr_of_cursor in
              f, args
          | _ -> failwith "expr_of_cursor (CallExpr)" in
        Call { f; args }
    | CStyleCastExpr ->
        let qual_type = get_cursor_type cxcursor |> of_cxtype in
        let operand =
          match list_of_children cxcursor with
          | [operand] -> expr_of_cursor operand
          | _ -> failwith "expr_of_cursor (CStyleCastExpr)" in
        CStyleCast { qual_type; operand }
    | MemberRefExpr ->
        let base =
          match list_of_children cxcursor with
          | [lhs] -> expr_of_cursor lhs
          | _ -> failwith "expr_of_cursor" in
        let field = get_cursor_referenced cxcursor in
        let arrow = ext_member_ref_expr_is_arrow cxcursor in
        Member { base; arrow; field = {
          cxcursor = field; desc = get_cursor_spelling field }};
    | FirstExpr ->
        UnexposedExpr { s = get_cursor_spelling cxcursor }
    | _ -> OtherExpr

  let translation_unit_of_cursor cxcursor =
    let filename = get_cursor_spelling cxcursor in
    let items = list_of_children cxcursor |> List.map decl_of_cursor in
    { cxcursor; desc = { filename; items } }

  let of_cxtranslationunit tu =
    translation_unit_of_cursor (get_translation_unit_cursor tu)
end

let parse_string ?(index = create_index true true) ?(filename = "<string>.c")
    ?(command_line_args = [| |]) ?(unsaved_files = [])
    ?(options = default_editing_translation_unit_options ()) contents =
  parse_translation_unit2 index filename command_line_args
    (Array.of_list ({ filename; contents } :: unsaved_files)) options
