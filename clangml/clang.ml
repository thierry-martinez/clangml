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
    Some (ext_int_get_sext_value64 cxint)
  else
    None

let int64_of_cxint cxint =
  if ext_int_get_min_signed_bits cxint <= 64 then
    ext_int_get_sext_value64 cxint
  else
    failwith "int64_of_cxint"

let int_of_cxint_opt cxint =
  if ext_int_get_min_signed_bits cxint <= Sys.int_size then
    Some (ext_int_get_sext_value cxint)
  else
    None

let int_of_cxint cxint =
  if ext_int_get_min_signed_bits cxint <= Sys.int_size then
    ext_int_get_sext_value cxint
  else
    failwith "int_of_cxint"

let string_of_cxint cxint =
  ext_int_to_string cxint 10 true

let float_of_cxfloat cxfloat =
  ext_float_convert_to_double cxfloat

let string_of_cxfloat cxfloat =
  ext_float_to_string cxfloat

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

module Ast = struct
  include Clang__ast

  let node ?decoration ?cursor ?location ?qual_type desc =
    let decoration : decoration =
      match decoration, cursor, location, qual_type with
      | Some decoration, None, None, None -> decoration
      | None, Some cursor, None, None -> Cursor cursor
      | None, None, location, qual_type -> Custom { location; qual_type }
      | _ -> invalid_arg "node" in
    { decoration; desc }

  let cursor_of_decoration decoration =
    match decoration with
    | Cursor cursor -> cursor
    | Custom _ -> get_null_cursor ()

  let cursor_of_node node =
    cursor_of_decoration node.decoration

  let location_of_decoration decoration =
    match decoration with
    | Cursor cursor -> get_cursor_location cursor
    | Custom { location } ->
        match location with
        | Some location -> location
        | None -> get_cursor_location (get_null_cursor ())

  let location_of_node node =
    location_of_decoration node.decoration

  type concrete_location = {
      filename : string;
      line : int;
      column : int
    }

  let get_presumed_location source_location =
    let filename, line, column = get_presumed_location source_location in
    { filename; line; column }

  let get_expansion_location source_location =
    let file, line, column, _offset = get_expansion_location source_location in
    { filename = get_file_name file; line; column }

  let string_of_elaborated_type_keyword = ext_elaborated_type_get_keyword_spelling

  let string_of_unary_operator_kind = ext_unary_operator_get_opcode_spelling

  let string_of_binary_operator_kind = ext_binary_operator_get_opcode_spelling

  module Options = struct
    type t = {
        ignore_implicit_cast : bool [@default true];
        ignore_paren : bool [@default true];
        ignore_paren_in_types : bool [@default true];
      }
          [@@deriving make]
  end

  module type OptionsS = sig
    val options : Options.t
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
            IncompleteArray element
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
            FunctionType function_type
        | Complex ->
            let element_type = cxtype |> get_element_type |> of_cxtype in 
            Complex element_type
        | Unexposed ->
            begin
              match ext_get_type_kind cxtype with
              | Paren -> ParenType (cxtype |> ext_get_inner_type |> of_cxtype)
              | _ -> BuiltinType (get_type_kind cxtype)
            end
        | _ -> BuiltinType (get_type_kind cxtype) in
      match desc with
      | ParenType inner when Options.options.ignore_paren_in_types -> inner
      | _ ->
          { cxtype; desc;
            const = is_const_qualified_type cxtype;
            volatile = is_volatile_qualified_type cxtype;
            restrict = is_restrict_qualified_type cxtype; }

    and decl_of_cxcursor cursor =
      node ~cursor (decl_desc_of_cxcursor cursor)

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
            TypedefDecl { name; underlying_type }
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

    and var_decl_of_cxcursor cursor =
      node ~cursor (var_decl_desc_of_cxcursor cursor)

    and var_decl_desc_of_cxcursor cxcursor =
      let linkage = cxcursor |> get_cursor_linkage in
      let name = get_cursor_spelling cxcursor in
      let qual_type = of_cxtype (get_cursor_type cxcursor) in
      let init =
        if ext_var_decl_has_init cxcursor then
          begin
            let init_value = list_of_children cxcursor |> List.rev |> List.hd in
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
          | EnumConstantDecl -> enum_constant_of_cxcursor cxcursor
          | _ -> failwith "enum_decl_of_cxcursor" in
      EnumDecl { name; constants }

    and enum_constant_of_cxcursor cursor =
      let name = get_cursor_spelling cursor in
      let init =
        match list_of_children cursor with
        | [init] -> Some (expr_of_cxcursor init)
        | [] -> None
        | _ -> failwith "enum_constant_of_cxcursor (init)" in
      node ~cursor { name; init }

    and struct_decl_of_cxcursor cxcursor =
      let name = get_cursor_spelling cxcursor in
      let fields = fields_of_cxcursor cxcursor in
      Struct { name; fields }

    and union_decl_of_cxcursor cxcursor =
      let name = get_cursor_spelling cxcursor in
      let fields = fields_of_cxcursor cxcursor in
      Union { name; fields }

    and fields_of_cxcursor cxcursor =
      list_of_children cxcursor |> List.map field_of_cxcursor

    and field_of_cxcursor cursor =
      match get_cursor_kind cursor with
      | FieldDecl ->
          let name = get_cursor_spelling cursor in
          let qual_type = get_cursor_type cursor |> of_cxtype in
          let bitwidth =
            if cursor_is_bit_field cursor then
              match list_of_children cursor with
              | [bitwidth] -> Some (expr_of_cxcursor bitwidth)
              | _ -> failwith "field_of_cxcursor (bitwidth)"
            else
              None in
          node ~cursor (Named { name; qual_type; bitwidth })
      | UnionDecl ->
          node ~cursor (AnonymousUnion (fields_of_cxcursor cursor))
      | StructDecl ->
          node ~cursor (AnonymousStruct (fields_of_cxcursor cursor))
      | _ -> failwith "field_of_cxcursor"

    and stmt_of_cxcursor cursor =
      let desc =
        match get_cursor_kind cursor with
        | NullStmt ->
            Null
        | CompoundStmt ->
            let items =
              cursor |> list_of_children |> List.map stmt_of_cxcursor in
            Compound items
        | ForStmt ->
            let children_set = ext_for_stmt_get_children_set cursor in
            let queue = Queue.create () in
            cursor |> iter_children (fun cur -> Queue.add cur queue);
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
            let children_set = ext_if_stmt_get_children_set cursor in
            let queue = Queue.create () in
            cursor |> iter_children (fun cur -> Queue.add cur queue);
            let init =
              if children_set land 1 <> 0 then
                Some (ext_if_stmt_get_init cursor |> stmt_of_cxcursor)
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
            let children_set = ext_switch_stmt_get_children_set cursor in
            let queue = Queue.create () in
            cursor |> iter_children (fun cur -> Queue.add cur queue);
            let init =
              if children_set land 1 <> 0 then
                Some (ext_switch_stmt_get_init cursor |> stmt_of_cxcursor)
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
              match list_of_children cursor with
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
              match list_of_children cursor with
              | [body] -> stmt_of_cxcursor body
              | _ -> failwith "stmt_of_cxcursor (DefaultStmt)" in
            Default body
        | WhileStmt ->
            let children_set = ext_while_stmt_get_children_set cursor in
            let queue = Queue.create () in
            cursor |> iter_children (fun cur -> Queue.add cur queue);
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
              match list_of_children cursor with
              | [body; cond] -> stmt_of_cxcursor body, expr_of_cxcursor cond
              | _ -> failwith "stmt_of_cxcursor (DoStmt)" in
            Do { body; cond }
        | LabelStmt ->
            let label = cursor |> get_cursor_spelling in
            let body =
              match list_of_children cursor with
              | [body] -> stmt_of_cxcursor body
              | _ -> failwith "stmt_of_cxcursor (LabelStmt)" in
            Label { label; body }
        | GotoStmt ->
            let label =
              match list_of_children cursor with
              | [label] -> label |> get_cursor_spelling
              | _ -> failwith "stmt_of_cxcursor (GotoStmt)" in
            Goto label
        | IndirectGotoStmt ->
            let target =
              match list_of_children cursor with
              | [target] -> expr_of_cxcursor target
              | _ -> failwith "stmt_of_cxcursor (IndirectGotoStmt)" in
            IndirectGoto target
        | ContinueStmt ->
            Continue
        | BreakStmt ->
            Break
        | DeclStmt ->
            let decl = list_of_children cursor |> List.map decl_of_cxcursor in
            Decl decl
        | ReturnStmt ->
            let value =
              match list_of_children cursor with
              | [value] -> Some (expr_of_cxcursor value)
              | [] -> None
              | _ -> failwith "stmt_of_cxcursor (ReturnStmt)" in
            Return value
        | GCCAsmStmt ->
            let code = ext_asm_stmt_get_asm_string cursor in
            let parameters =
              list_of_children cursor |> List.map @@ fun cursor ->
                node ~cursor (get_cursor_spelling cursor) in
            GCCAsm (code, parameters)
        | MSAsmStmt ->
            MSAsm (ext_asm_stmt_get_asm_string cursor)
        | _ -> Decl [node ~cursor (decl_desc_of_cxcursor cursor)] in
      match desc with
      | Decl [{ desc = OtherDecl }] ->
          let expr = expr_of_cxcursor cursor in
          node ~decoration:expr.decoration (Expr expr.desc)
      | _ -> node ~cursor desc

    and expr_of_cxcursor cursor =
      match expr_desc_of_cxcursor cursor with
      | Paren subexpr when Options.options.ignore_paren -> subexpr
      | Cast { kind = Implicit; operand } when Options.options.ignore_implicit_cast ->
          operand
      | desc -> node ~cursor desc

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
      | CharacterLiteral ->
          let kind = ext_character_literal_get_character_kind cxcursor in
          let value = ext_character_literal_get_value cxcursor in
          CharacterLiteral { kind; value }
      | ImaginaryLiteral ->
          let sub_expr =
            match list_of_children cxcursor with
            | [operand] -> expr_of_cxcursor operand
            | _ -> failwith "expr_of_cxcursor (ImaginaryLiteral)" in
          ImaginaryLiteral sub_expr
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
          let callee, args =
            match list_of_children cxcursor with
            | callee :: args ->
                let callee = callee |> expr_of_cxcursor in
                let args = args |> List.map expr_of_cxcursor in
                callee, args
            | _ -> failwith "expr_of_cxcursor (CallExpr)" in
          Call { callee; args }
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
          let field = node ~cursor:field (get_cursor_spelling field) in
          let arrow = ext_member_ref_expr_is_arrow cxcursor in
          Member { base; arrow; field };
      | ArraySubscriptExpr ->
          let base, index =
            match list_of_children cxcursor with
            | [base; index] -> expr_of_cxcursor base, expr_of_cxcursor index
            | _ -> failwith "expr_of_cxcursor (ArraySubscriptExpr)" in
          ArraySubscript { base; index }
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
      | UnaryExpr ->
          let kind = cxcursor |> ext_unary_expr_get_kind in
          let argument =
            match list_of_children cxcursor with
            | [argument] -> ArgumentExpr (argument |> expr_of_cxcursor)
            | [] ->
                let qual_type =
                  cxcursor |> ext_unary_expr_get_argument_type |> of_cxtype in
                ArgumentType qual_type
            | _ -> failwith "expr_of_cxcursor (UnaryExpr)" in
          UnaryExpr { kind; argument }
      | UnexposedExpr ->
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

    let translation_unit_of_cxcursor cursor =
      let filename = get_cursor_spelling cursor in
      let items = list_of_children cursor |> List.map decl_of_cxcursor in
      node ~cursor { filename; items }

    let of_cxtranslationunit tu =
      translation_unit_of_cxcursor (get_translation_unit_cursor tu)
  end

  let of_cxtype ?(options = Options.make ()) tu =
    let module Convert = Converter (struct let options = options end) in
    Convert.of_cxtype tu

  let of_cxtranslationunit
      ?(options = Options.make ())
      tu =
    let module Convert = Converter (struct let options = options end) in
    Convert.of_cxtranslationunit tu

  let parse_file ?index ?command_line_args ?unsaved_files ?clang_options
      ?options filename =
    parse_file ?index ?command_line_args ?unsaved_files ?options:clang_options
      filename |>
    of_cxtranslationunit ?options

  let parse_string ?index ?filename ?command_line_args ?unsaved_files
      ?clang_options ?options string =
    parse_string ?index ?filename ?command_line_args ?unsaved_files
      ?options:clang_options string |>
    of_cxtranslationunit ?options
end

module Decl = struct
  module Self = struct
    type t = Ast.decl

    let equal = Ast.equal_decl

    let compare = Ast.compare_decl
  end

  include Self

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)

  let of_cxcursor ?(options = Ast.Options.make ()) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.decl_of_cxcursor cur

  let get_typedef_underlying_type ?options decl =
    decl |> Ast.cursor_of_node |>
    get_typedef_decl_underlying_type |> Ast.of_cxtype ?options
end

module Field = struct
  module Self = struct
    type t = Ast.field

    let equal = Ast.equal_field

    let compare = Ast.compare_field
  end

  include Self

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)

  let of_cxcursor ?(options = Ast.Options.make ()) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.field_of_cxcursor cur

  let get_bit_width field =
    field |> Ast.cursor_of_node |> get_field_decl_bit_width
end

module Type = struct
  module Self = struct
    type t = Ast.qual_type

    let equal = Ast.equal_qual_type

    let compare = Ast.compare_qual_type
  end

  include Self

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)

  let of_cxtype = Ast.of_cxtype

  let of_cursor ?options cursor =
    get_cursor_type cursor |> of_cxtype ?options

  let of_decoration ?options (decoration : Ast.decoration) =
    match decoration with
    | Cursor cursor -> of_cursor ?options cursor
    | Custom { qual_type } ->
        match qual_type with
        | Some qual_type -> qual_type
        | None -> invalid_arg "of_decoration"

  let of_node ?options (node : 'a Ast.node) =
    of_decoration ?options node.decoration

  let get_size_of (ty : t) = type_get_size_of ty.cxtype

  let get_align_of (ty : t) = type_get_size_of ty.cxtype

  let get_typedef_underlying_type ?options (qual_type : t) =
    Clang__bindings.get_type_declaration qual_type.cxtype |>
      Clang__bindings.get_typedef_decl_underlying_type |>
      of_cxtype ?options

  let get_declaration ?options (qual_type : t) =
    get_type_declaration qual_type.cxtype |> Decl.of_cxcursor ?options

  let iter_fields ?options f (qual_type : t) =
    qual_type.cxtype |> iter_type_fields @@ fun x ->
      f (Field.of_cxcursor ?options x)

  let list_of_fields ?options (qual_type : t) =
    qual_type.cxtype |> list_of_type_fields |> List.map @@
      Field.of_cxcursor ?options
end

module Expr = struct
  module Self = struct
    type t = Ast.expr

    let equal = Ast.equal_expr

    let compare = Ast.compare_expr
  end

  include Self

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)

  let of_cxcursor ?(options = Ast.Options.make ()) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.expr_of_cxcursor cur
end

module Stmt = struct
  module Self = struct
    type t = Ast.stmt

    let equal = Ast.equal_stmt

    let compare = Ast.compare_stmt
  end

  include Self

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)

  let of_cxcursor ?(options = Ast.Options.make ()) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.stmt_of_cxcursor cur
end

module Enum_constant = struct
  module Self = struct
    type t = Ast.enum_constant

    let equal = Ast.equal_enum_constant

    let compare = Ast.compare_enum_constant
  end

  include Self

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)

  let of_cxcursor ?(options = Ast.Options.make ()) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.enum_constant_of_cxcursor cur

  let get_value enum_constant =
    enum_constant |> Ast.cursor_of_node |> get_enum_constant_decl_value
end
