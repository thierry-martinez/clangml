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
    match get_cursor_kind cxcursor with
    | FunctionDecl -> function_decl_of_cursor cxcursor
    | VarDecl -> var_decl_of_cursor cxcursor
    | StructDecl -> struct_decl_of_cursor cxcursor
    | EnumDecl -> enum_decl_of_cursor cxcursor
    | TypedefDecl ->
        let name = get_cursor_spelling cxcursor in
        let underlying_type = cxcursor |>
          get_typedef_decl_underlying_type |> qual_type_of_cxtype in
        Typedef { cxcursor; name; underlying_type }
    | _ -> OtherDecl cxcursor

  and function_decl_of_cursor cxcursor =
    let result = cxcursor |>
      get_cursor_type |> get_result_type |> qual_type_of_cxtype in
    let name = get_cursor_spelling cxcursor in
    let _, stmt = list_last (list_of_children cxcursor) in
    let args = List.init (cursor_get_num_arguments cxcursor) @@ fun i ->
      let c = cursor_get_argument cxcursor i in
      get_cursor_spelling c, qual_type_of_cxtype (get_cursor_type c) in
    let stmt = stmt_of_cursor stmt in
    Function { cxcursor; result; name; args; stmt }

  and var_decl_of_cursor cxcursor =
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
    Var { cxcursor; name; qual_type; init }

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
            let value = get_enum_constant_decl_value cxcursor in
            { cxcursor; name; init; value }
        | _ -> failwith "enum_decl_of_cursor" in
    Enum { cxcursor; name; constants }

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
    Struct { cxcursor; name; fields }

  and stmt_of_cursor cxcursor =
    match get_cursor_kind cxcursor with
    | CompoundStmt ->
        Compound {
          cxcursor;
          items = cxcursor |> list_of_children |> List.map stmt_of_cursor }
    | ForStmt ->
        let arguments = ext_for_stmt_get_arguments cxcursor in
        let queue =
          cxcursor |> list_of_children |> List.to_seq |> Queue.of_seq in
        let init =
          if arguments land 1 <> 0 then
            Some (stmt_of_cursor (Queue.pop queue))
          else
            None in
        let condition_variable =
          if arguments land 2 <> 0 then
            Some (decl_of_cursor (Queue.pop queue))
          else
            None in
        let cond =
          if arguments land 4 <> 0 then
            Some (stmt_of_cursor (Queue.pop queue))
          else
            None in
        let inc =
          if arguments land 8 <> 0 then
            Some (stmt_of_cursor (Queue.pop queue))
          else
            None in
        let body = stmt_of_cursor (Queue.pop queue) in
        assert (Queue.is_empty queue);
        For { cxcursor; init; condition_variable; cond; inc; body }
    | DeclStmt ->
        let decl =
          match list_of_children cxcursor with
          | [decl] -> decl_of_cursor decl
          | _ -> failwith "stmt_of_cursor (DeclStmt)" in
        Decl decl
    | ReturnStmt ->
        let value =
          match list_of_children cxcursor with
          | [value] -> expr_of_cursor value
          | _ -> failwith "stmt_of_cursor (ReturnStmt)" in
        Return { cxcursor; value }
    | NullStmt ->
        Null
    | _ ->
        match decl_of_cursor cxcursor with
        | OtherDecl c -> Expr (expr_of_cursor c)
        | d -> Decl d

  and expr_of_cursor cxcursor =
    match get_cursor_kind cxcursor with
    | IntegerLiteral ->
        IntegerLiteral {
          cxcursor;
          s = ext_integer_literal_get_value_as_string cxcursor 10 true;
        }
    | UnaryOperator ->
        let operand =
          match list_of_children cxcursor with
          | [operand] -> expr_of_cursor operand
          | _ -> failwith "expr_of_cursor (UnaryOperator)" in
        let kind = ext_unary_operator_get_opcode cxcursor in
        UnaryOperator { cxcursor; kind; operand }
    | BinaryOperator ->
        let lhs, rhs =
          match list_of_children cxcursor with
          | [lhs; rhs] -> expr_of_cursor lhs, expr_of_cursor rhs
          | _ -> failwith "expr_of_cursor (BinaryOperator)" in
        let kind = ext_binary_operator_get_opcode cxcursor in
        BinaryOperator { cxcursor; lhs; kind; rhs }
    | DeclRefExpr -> DeclRef { cxcursor; s = get_cursor_spelling cxcursor }
    | CallExpr ->
        let f, args =
          match list_of_children cxcursor with
          | f :: args ->
              let f = f |> expr_of_cursor in
              let args = args |> List.map expr_of_cursor in
              f, args
          | _ -> failwith "expr_of_cursor (CallExpr)" in
        Call { cxcursor; f; args }
    | FirstExpr ->
        UnexposedExpr { cxcursor; s = get_cursor_spelling cxcursor }
    | _ -> OtherExpr cxcursor

  let translation_unit_of_cursor cxcursor =
    let filename = get_cursor_spelling cxcursor in
    let items = list_of_children cxcursor |> List.map decl_of_cursor in
    { cxcursor; filename; items }

  let of_cxtranslationunit tu =
    translation_unit_of_cursor (get_translation_unit_cursor tu)
end
