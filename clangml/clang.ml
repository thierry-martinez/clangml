include Clang__bindings

let list_of_children c =
  let children_ref = ref [] in
  assert (not (visit_children c begin fun cur _par ->
    children_ref := cur :: !children_ref;
    Continue
  end));
  List.rev !children_ref

module Ast = struct
  include Clang__ast

  let rec qual_type_of_cxtype cxtype =
    let desc =
      match get_type_kind cxtype with
      | IncompleteArray ->
          let element = cxtype |> get_element_type |> qual_type_of_cxtype in
          IncompleteArray { element }
      | Pointer ->
          let pointee = cxtype |> get_pointee_type |> qual_type_of_cxtype in
          Pointer { pointee }
      | Elaborated ->
          Elaborated {
            keyword = ext_elaborated_type_get_keyword cxtype;
            named_type = type_get_named_type cxtype |> qual_type_of_cxtype;
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
      | _ -> OtherType { kind = get_type_kind cxtype } in
    { cxtype; desc; const = is_const_qualified_type cxtype }

  let rec list_last l =
    match l with
    | [] -> failwith "list_last"
    | [last] -> [], last
    | hd :: tl ->
        let tl, last = list_last tl in
        hd :: tl, last

  let rec decl_of_cursor cxcursor =
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
          get_typedef_decl_underlying_type |> qual_type_of_cxtype in
          Typedef { name; underlying_type }
      | _ -> OtherDecl

  and function_decl_of_cursor cxcursor =
    let result = cxcursor |>
      get_cursor_type |> get_result_type |> qual_type_of_cxtype in
    let name = get_cursor_spelling cxcursor in
    let _, stmt = list_last (list_of_children cxcursor) in
    let args = List.init (cursor_get_num_arguments cxcursor) @@ fun i ->
      let c = cursor_get_argument cxcursor i in
      get_cursor_spelling c, qual_type_of_cxtype (get_cursor_type c) in
    let stmt = stmt_of_cursor stmt in
    Function { result; name; args; stmt }

  and var_decl_of_cursor cxcursor =
    { cxcursor; desc = var_decl_desc_of_cursor cxcursor }

  and var_decl_desc_of_cursor cxcursor =
    let name = get_cursor_spelling cxcursor in
    let qual_type = qual_type_of_cxtype (get_cursor_type cxcursor) in
    let init =
      if ext_var_decl_has_init cxcursor then
        begin
          let _, init_value = list_last (list_of_children cxcursor) in
          Some (expr_of_cursor init_value)
        end
      else
        None in
    { name; qual_type; init }

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
            let qual_type = get_cursor_type cur |> qual_type_of_cxtype in
            name, qual_type
        | _ -> failwith "struct_decl_of_cursor" in
    Struct { name; fields }

  and stmt_of_cursor cxcursor =
    let desc =
      match get_cursor_kind cxcursor with
      | NullStmt ->
          Null
      | CompoundStmt ->
          Compound { items = cxcursor |> list_of_children |> List.map stmt_of_cursor }
      | ForStmt ->
          let children_set = ext_for_stmt_get_children_set cxcursor in
          let queue =
            cxcursor |> list_of_children |> List.to_seq |> Queue.of_seq in
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
          let queue =
            cxcursor |> list_of_children |> List.to_seq |> Queue.of_seq in
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
          let queue =
            cxcursor |> list_of_children |> List.to_seq |> Queue.of_seq in
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
          let queue =
            cxcursor |> list_of_children |> List.to_seq |> Queue.of_seq in
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
            | [body; cond] ->
                stmt_of_cursor body, expr_of_cursor cond
            | _ ->
                failwith "stmt_of_cursor (DoStmt)" in
          Do { body; cond }
      | ContinueStmt ->
          Continue
      | BreakStmt ->
          Break
      | DeclStmt ->
          let decl =
            match list_of_children cxcursor with
            | [decl] -> decl_desc_of_cursor decl
            | _ -> failwith "stmt_of_cursor (DeclStmt)" in
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
          | d -> Decl d in
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
