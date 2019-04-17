include Clang__bindings

include Clang__compat

include Clang__types

include Clang__utils

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
    | Cursor cursor -> Clang (get_cursor_location cursor)
    | Custom { location } ->
        match location with
        | Some location -> location
        | None -> Clang (get_cursor_location (get_null_cursor ()))

  let location_of_node node =
    location_of_decoration node.decoration

  include Clang__ast_utils

  module Options = struct
    type t = {
        ignore_implicit_cast : bool [@default true];
        ignore_paren : bool [@default true];
        ignore_paren_in_types : bool [@default true];
        convert_integer_literals : bool [@default true];
        convert_floating_literals : bool [@default true];
      }
          [@@deriving make]
  end

  module type OptionsS = sig
    val options : Options.t
  end

  module Converter (Options : OptionsS) = struct
    exception Invalid_structure

    let filter_attributes list =
      list |> List.filter @@ fun cursor ->
        match get_cursor_kind cursor with
        | UnexposedAttr -> false
        | _ -> true

    let make_integer_literal i =
      if Options.options.convert_integer_literals then
        match int_of_cxint_opt i with
        | None -> CXInt i
        | Some i -> Int i
      else
        CXInt i

    let rec make_template_name name =
      match ext_template_name_get_kind name with
      | Template ->
          NameTemplate (
            decl_of_cxcursor (ext_template_name_get_as_template_decl name))
      | OverloadedTemplate -> OverloadedTemplate
      | QualifiedTemplate -> QualifiedTemplate
      | DependentTemplate -> DependentTemplate
      | SubstTemplateTemplateParm -> SubstTemplateTemplateParm
      | SubstTemplateTemplateParmPack -> SubstTemplateTemplateParmPack
      | InvalidNameKind -> InvalidNameKind

    and make_template_argument argument =
      match ext_template_argument_get_kind argument with
      | Type ->
          Type (
            ext_template_argument_get_as_type argument |>
            of_cxtype)
      | Declaration ->
          ArgumentDecl (
            decl_of_cxcursor
              (ext_template_argument_get_as_decl argument))
      | NullPtr ->
          NullPtr (
            ext_template_argument_get_null_ptr_type argument |>
            of_cxtype)
      | Integral ->
          Integral {
            value =
              ext_template_argument_get_as_integral argument |>
              make_integer_literal;
            qual_type =
              ext_template_argument_get_integral_type argument |>
              of_cxtype }
      | Template ->
          TemplateTemplateArgument (
            ext_template_argument_get_as_template_or_template_pattern argument |>
            make_template_name)
      | TemplateExpansion ->
          TemplateExpansion (
            ext_template_argument_get_as_template_or_template_pattern argument |>
            make_template_name)
      | Expression ->
          ExprTemplateArgument (
            ext_template_argument_get_as_expr argument |>
            expr_of_cxcursor)
      | _ -> raise Invalid_structure

    and of_cxtype cxtype =
      let desc =
        match get_type_kind cxtype with
        | Invalid -> InvalidType
        | ConstantArray ->
            let element = cxtype |> get_array_element_type |> of_cxtype in
            let size = cxtype |> get_array_size in
            ConstantArray { element; size }
        | Vector ->
            let element = cxtype |> get_element_type |> of_cxtype in
            let size = cxtype |> get_num_elements in
            Vector { element; size }
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
        | LValueReference ->
            let pointee = cxtype |> get_pointee_type |> of_cxtype in
            LValueReference pointee
        | Enum ->
            Enum (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | Record ->
            Record (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | Typedef ->
            Typedef (Ident (cxtype |> get_type_declaration |> get_cursor_spelling))
        | FunctionProto
        | FunctionNoProto ->
            let function_type =
              cxtype |> function_type_of_cxtype (parameters_of_cxtype cxtype) in
            FunctionType function_type
        | Complex ->
            let element_type = cxtype |> get_element_type |> of_cxtype in
            Complex element_type
        | _ ->
            begin
              match ext_get_type_kind cxtype with
              | Paren -> ParenType (cxtype |> ext_get_inner_type |> of_cxtype)
              | Elaborated -> (* Here for Clang <3.9.0 *)
                  Elaborated {
                    keyword = ext_elaborated_type_get_keyword cxtype;
                    named_type = ext_type_get_named_type cxtype |> of_cxtype;
                  }
              | Attributed -> (* Here for Clang <8.0.0 *)
                  Attributed {
                    modified_type = type_get_modified_type cxtype |> of_cxtype;
                    attribute_kind = ext_type_get_attribute_kind cxtype;
                  }
              | TemplateTypeParm ->
                  TemplateTypeParm (get_type_spelling cxtype);
              | TemplateSpecialization ->
                  let name =
                    cxtype |>
                    ext_template_specialization_type_get_template_name |>
                    make_template_name in
                  let arguments =
                    List.init
                      (ext_template_specialization_type_get_num_args cxtype)
                    @@ fun i ->
                      ext_template_specialization_type_get_argument cxtype i |>
                      make_template_argument in
                  TemplateSpecialization { name; arguments }
              | Builtin -> BuiltinType (get_type_kind cxtype)
              | kind -> UnexposedType kind
            end in
      match desc with
      | ParenType inner when Options.options.ignore_paren_in_types ->
          { inner with cxtype }
      | _ ->
          { cxtype; desc;
            const = is_const_qualified_type cxtype;
            volatile = is_volatile_qualified_type cxtype;
            restrict = is_restrict_qualified_type cxtype; }

    and decl_of_cxcursor ?in_record cursor =
      node ~cursor (decl_desc_of_cxcursor ?in_record cursor)

    and decl_desc_of_cxcursor ?(in_record = false) cursor =
      try
        match get_cursor_kind cursor with
        | FunctionDecl ->
            function_decl_of_cxcursor cursor (list_of_children cursor)
        | FunctionTemplate ->
            cursor |> make_template @@ fun children ->
              cxxmethod_decl_of_cxcursor ~can_be_function:(not in_record)
                cursor children
        | CXXMethod ->
            cxxmethod_decl_of_cxcursor cursor (list_of_children cursor)
        | VarDecl -> Var (var_decl_desc_of_cxcursor cursor)
        | StructDecl -> record_decl_of_cxcursor Struct cursor
        | UnionDecl -> record_decl_of_cxcursor Union cursor
        | ClassDecl -> record_decl_of_cxcursor Class cursor
        | ClassTemplate ->
            let decl_cursor =
              ext_class_template_decl_get_templated_decl cursor in
            let keyword =
              match get_cursor_kind decl_cursor with
              | StructDecl -> Struct
              | UnionDecl -> Union
              | ClassDecl -> Class
              | kind -> raise Invalid_structure in
            make_template
              (record_decl_of_children keyword cursor)
              cursor
        | EnumDecl -> enum_decl_of_cxcursor cursor
        | TypedefDecl ->
            let name = get_cursor_spelling cursor in
            let underlying_type = cursor |>
            get_typedef_decl_underlying_type |> of_cxtype in
            TypedefDecl { name; underlying_type }
        | FieldDecl ->
            let name = get_cursor_spelling cursor in
            let qual_type = get_cursor_type cursor |> of_cxtype in
            let bitwidth =
              if cursor_is_bit_field cursor then
                match list_of_children cursor with
                | [bitwidth] -> Some (expr_of_cxcursor bitwidth)
                | _ -> raise Invalid_structure
              else
                None in
            let init = ext_field_decl_get_in_class_initializer cursor in
            let init : expr option =
              if get_cursor_kind init = InvalidCode then None
              else Some (init |> expr_of_cxcursor) in
            Field { name; qual_type; bitwidth; init }
        | CXXAccessSpecifier ->
            CXXAccessSpecifier (cursor |> get_cxxaccess_specifier)
        | Namespace ->
            let name = get_cursor_spelling cursor in
            let declarations =
              list_of_children cursor |> List.map decl_of_cxcursor in
            Namespace { name; declarations }
        | UsingDirective ->
            let namespace =
              match list_of_children cursor with
              | [namespace] -> namespace
              | _ -> raise Invalid_structure in
            let namespace = get_cursor_spelling namespace in
            Using { namespace; decl = None }
        | UsingDeclaration ->
            begin
              match list_of_children cursor with
              | [namespace_ref; decl_ref] when
                  get_cursor_kind namespace_ref = NamespaceRef &&
                  get_cursor_kind decl_ref = OverloadedDeclRef ->
                    Using {
                    namespace = get_cursor_spelling namespace_ref;
                    decl = Some (get_cursor_spelling decl_ref) }
              | _ -> raise Invalid_structure
            end
        | Constructor ->
            let children = list_of_children cursor in
            let rec extract_initializer_list children =
              match children with
              | member :: unexposed :: children when
                  get_cursor_kind member = MemberRef &&
                  get_cursor_kind unexposed = UnexposedExpr ->
                    let init_expr =
                      match list_of_children unexposed with
                      | [init_expr] -> expr_of_cxcursor init_expr
                      | _ -> raise Invalid_structure in
                    (get_cursor_spelling member, init_expr) ::
                    extract_initializer_list children
              | _ :: tl -> extract_initializer_list tl
              | [] -> [] in
            let body =
              match List.rev children with
              | body :: _ when get_cursor_kind body = CompoundStmt ->
                  Some (stmt_of_cxcursor body)
              | _ -> None in
            Constructor {
              class_name = get_cursor_spelling cursor;
              parameters = parameters_of_function_decl cursor;
              initializer_list = extract_initializer_list children;
              body;
              defaulted = ext_cxxmethod_is_defaulted cursor;
              deleted = ext_function_decl_is_deleted cursor;
              explicit = ext_cxxconstructor_is_explicit cursor;
            }
        | Destructor ->
            let children = list_of_children cursor in
            let body =
              match List.rev children with
              | body :: _ when get_cursor_kind body = CompoundStmt ->
                  Some (stmt_of_cxcursor body)
              | _ -> None in
            let destructor_name = get_cursor_spelling cursor in
            Destructor {
              class_name = String.sub destructor_name 1
                (String.length destructor_name - 1);
              body;
              defaulted = ext_cxxmethod_is_defaulted cursor;
              deleted = ext_function_decl_is_deleted cursor;
          }
        | TemplateTemplateParameter ->
            TemplateTemplateParameter (get_cursor_spelling cursor)
        | kind ->
            match ext_decl_get_kind cursor with
            | Empty -> EmptyDecl
            | LinkageSpec ->
                let languages =
                  languages_of_ids
                    (ext_linkage_spec_decl_get_language_ids cursor) in
                let decls =
                  list_of_children cursor |> List.map decl_of_cxcursor in
                LinkageSpec { languages; decls }
            | Friend -> (* No FriendDecl : cxcursortype in Clang <4.0.0 *)
                let friend_type = ext_friend_decl_get_friend_type cursor in
                if get_type_kind friend_type = Invalid then
                  let decl = ext_friend_decl_get_friend_decl cursor in
                  Friend (FriendDecl (decl_of_cxcursor decl))
                else
                  Friend (FriendType (of_cxtype friend_type))
            | ext_kind -> UnknownDecl (kind, ext_kind)
      with Invalid_structure ->
        UnknownDecl (get_cursor_kind cursor, ext_decl_get_kind cursor)

    and parameters_of_function_decl cursor =
      { non_variadic =
      List.init (ext_function_decl_get_num_params cursor) (fun i ->
        let param = ext_function_decl_get_param_decl cursor i in
        node ~cursor:param {
          name = get_cursor_spelling param;
          qual_type = get_cursor_type param |> of_cxtype;
          default =
            match list_of_children param with
            | [] -> None
            | [default] -> Some (default |> expr_of_cxcursor)
            | _ -> raise Invalid_structure });
      variadic = is_function_type_variadic (get_cursor_type cursor) }

    and parameters_of_function_decl_or_proto cursor =
      if cursor |> get_cursor_type |> get_type_kind = FunctionProto then
        Some (parameters_of_function_decl cursor)
      else
        None

    and function_type_of_decl cursor =
      cursor |> get_cursor_type |>
        function_type_of_cxtype (parameters_of_function_decl_or_proto cursor)

    and function_decl_of_cxcursor cursor children =
      let linkage = cursor |> get_cursor_linkage in
      let function_type = function_type_of_decl cursor in
      let name = get_cursor_spelling cursor in
      let body : stmt option =
        match List.rev children with
        | last :: _ when get_cursor_kind last = CompoundStmt ->
            Some (stmt_of_cxcursor last)
    | _ -> None in
      let deleted = ext_function_decl_is_deleted cursor in
      Clang__ast.Function { linkage; function_type; name; body; deleted }

    and cxxmethod_decl_of_cxcursor ?(can_be_function = false) cursor children =
      let function_type = function_type_of_decl cursor in
      let name = get_cursor_spelling cursor in
      let type_ref : qual_type option =
        match children with
        | type_ref :: _ when get_cursor_kind type_ref = TypeRef ->
            Some (type_ref |> get_cursor_type |> of_cxtype)
        | _ -> None in
      let body : stmt option =
        match children with
        | [] -> None
        | _ ->
            let last = List.hd (List.rev children) in
            if get_cursor_kind last = CompoundStmt then
              Some (stmt_of_cxcursor last)
            else
              None in
      let deleted = ext_function_decl_is_deleted cursor in
      if can_be_function && type_ref = None then
        let linkage = cursor |> get_cursor_linkage in
        Function { linkage; function_type; name; body; deleted }
      else
        CXXMethod {
          type_ref; function_type; name; body;
          defaulted = ext_cxxmethod_is_defaulted cursor;
          static = cxxmethod_is_static cursor;
          binding =
            if cxxmethod_is_pure_virtual cursor then
              PureVirtual
            else if cxxmethod_is_virtual cursor then
              Virtual
            else
              NonVirtual;
          const = ext_cxxmethod_is_const cursor;
          deleted
        }

    and parameters_of_cxtype cxtype =
      if cxtype |> get_type_kind = FunctionProto then
        let non_variadic =
          List.init (get_num_arg_types cxtype) @@ fun i ->
            node {
              name = "";
              qual_type = of_cxtype (get_arg_type cxtype i);
              default = None } in
        let variadic = is_function_type_variadic cxtype in
        Some { non_variadic; variadic }
      else
        None

    and function_type_of_cxtype parameters cxtype =
      let calling_conv = cxtype |> get_function_type_calling_conv in
      let result = cxtype |> get_result_type |> of_cxtype in
      { calling_conv; result; parameters }

    and var_decl_of_cxcursor cursor =
      node ~cursor (var_decl_desc_of_cxcursor cursor)

    and var_decl_desc_of_cxcursor cursor =
      let linkage = cursor |> get_cursor_linkage in
      let name = get_cursor_spelling cursor in
      let qual_type = of_cxtype (get_cursor_type cursor) in
      let init =
        if ext_var_decl_has_init cursor then
          begin
            let init_value = list_of_children cursor |> List.rev |> List.hd in
            Some (expr_of_cxcursor init_value)
          end
        else
          None in
      { linkage; name; qual_type; init }

    and enum_decl_of_cxcursor cursor =
      let name = get_cursor_spelling cursor in
      let constants =
        list_of_children cursor |> List.map @@ fun cursor ->
          match get_cursor_kind cursor with
          | EnumConstantDecl -> enum_constant_of_cxcursor cursor
          | _ -> raise Invalid_structure in
      EnumDecl { name; constants }

    and enum_constant_of_cxcursor cursor =
      let name = get_cursor_spelling cursor in
      let init =
        match filter_attributes (list_of_children cursor) with
        | [init] -> Some (expr_of_cxcursor init)
        | [] -> None
        | _ -> raise Invalid_structure in
      node ~cursor { name; init }

    and record_decl_of_cxcursor keyword cursor =
      record_decl_of_children keyword cursor (list_of_children cursor)

    and record_decl_of_children keyword cursor children =
      let name = get_cursor_spelling cursor in
      let fields = fields_of_children children in
      RecordDecl { keyword; name; fields }

    and fields_of_children children =
      children |> List.map (decl_of_cxcursor ~in_record:true)

    and stmt_of_cxcursor cursor =
      let desc =
        try
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
                  Some (expr_of_cxcursor (Queue.pop queue))
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
                    raise Invalid_structure in
              let lhs = expr_of_cxcursor lhs in
              let body = stmt_of_cxcursor body in
              Case { lhs; rhs; body }
          | DefaultStmt ->
              let body =
                match list_of_children cursor with
                | [body] -> stmt_of_cxcursor body
                | _ ->
                    raise Invalid_structure in
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
                | _ -> raise Invalid_structure in
              Do { body; cond }
          | LabelStmt ->
              let label = cursor |> get_cursor_spelling in
              let body =
                match list_of_children cursor with
                | [body] -> stmt_of_cxcursor body
                | _ -> raise Invalid_structure in
              Label { label; body }
          | GotoStmt ->
              let label =
                match list_of_children cursor with
                | [label] -> label |> get_cursor_spelling
                | _ -> raise Invalid_structure in
              Goto label
          | IndirectGotoStmt ->
              let target =
                match list_of_children cursor with
                | [target] -> expr_of_cxcursor target
                | _ -> raise Invalid_structure in
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
                | _ -> raise Invalid_structure in
              Return value
          | GCCAsmStmt ->
              let code = ext_asm_stmt_get_asm_string cursor in
              let parameters =
                list_of_children cursor |> List.map @@ fun cursor ->
                  node ~cursor (get_cursor_spelling cursor) in
              GCCAsm (code, parameters)
          | MSAsmStmt ->
              MSAsm (ext_asm_stmt_get_asm_string cursor)
          | _ -> Decl [node ~cursor (decl_desc_of_cxcursor cursor)]
        with Invalid_structure -> OtherStmt in
      match desc with
      | Decl [{ desc = UnknownDecl _ }] ->
          let expr = expr_of_cxcursor cursor in
          node ~decoration:expr.decoration (Expr expr)
      | _ -> node ~cursor desc

    and expr_of_cxcursor cursor =
      match expr_desc_of_cxcursor cursor with
      | Paren subexpr when Options.options.ignore_paren ->
          node ~cursor subexpr.desc
      | Cast { kind = Implicit; operand }
        when Options.options.ignore_implicit_cast ->
          node ~cursor operand.desc
      | desc -> node ~cursor desc

    and expr_desc_of_cxcursor cursor =
      let kind = get_cursor_kind cursor in
      try
        match kind with
        | IntegerLiteral ->
            let i = ext_integer_literal_get_value cursor in
            let literal = make_integer_literal i in
            IntegerLiteral literal
        | FloatingLiteral ->
            let f = ext_floating_literal_get_value cursor in
            let literal =
              if Options.options.convert_floating_literals then
                Float (float_of_cxfloat f)
              else CXFloat f in
            FloatingLiteral literal
        | StringLiteral ->
            StringLiteral (ext_string_literal_get_string cursor)
        | CharacterLiteral ->
            let kind = ext_character_literal_get_character_kind cursor in
            let value = ext_character_literal_get_value cursor in
            CharacterLiteral { kind; value }
        | ImaginaryLiteral ->
            let sub_expr =
              match list_of_children cursor with
              | [operand] -> expr_of_cxcursor operand
              | _ -> raise Invalid_structure in
            ImaginaryLiteral sub_expr
        | UnaryOperator ->
            let operand =
              match list_of_children cursor with
              | [operand] -> expr_of_cxcursor operand
              | _ -> raise Invalid_structure in
            let kind = ext_unary_operator_get_opcode cursor in
            UnaryOperator { kind; operand }
        | BinaryOperator | CompoundAssignOperator ->
            let lhs, rhs =
              match list_of_children cursor with
              | [lhs; rhs] -> expr_of_cxcursor lhs, expr_of_cxcursor rhs
              | _ -> raise Invalid_structure in
            let kind = ext_binary_operator_get_opcode cursor in
            BinaryOperator { lhs; kind; rhs }
        | DeclRefExpr -> DeclRef (ident_ref_of_cxcursor cursor)
        | CallExpr ->
            let callee, args =
              match list_of_children cursor with
              | callee :: args ->
                  let callee = callee |> expr_of_cxcursor in
                  let args = args |> List.map expr_of_cxcursor in
                  callee, args
              | _ -> raise Invalid_structure in
            Call { callee; args }
        | CStyleCastExpr ->
            let qual_type = get_cursor_type cursor |> of_cxtype in
            let operand =
              match list_of_children cursor with
              | [operand] | [_; operand] -> expr_of_cxcursor operand
              | _ -> raise Invalid_structure in
            Cast { kind = CStyle; qual_type; operand }
        | MemberRefExpr ->
            begin match list_of_children cursor with
            | [] -> MemberRef (get_cursor_spelling cursor)
            | [lhs] ->
                let base = expr_of_cxcursor lhs in
                let field = get_cursor_referenced cursor in
                let field = node ~cursor:field (get_cursor_spelling field) in
                let arrow = ext_member_ref_expr_is_arrow cursor in
                Member { base; arrow; field }
            | _ -> raise Invalid_structure
            end
        | ArraySubscriptExpr ->
            let base, index =
              match list_of_children cursor with
              | [base; index] -> expr_of_cxcursor base, expr_of_cxcursor index
              | _ -> raise Invalid_structure in
            ArraySubscript { base; index }
        | ConditionalOperator ->
            let cond, then_branch, else_branch =
              match list_of_children cursor |> List.map expr_of_cxcursor with
              | [cond; then_branch; else_branch] ->
                  cond, Some then_branch, else_branch
              | _ -> raise Invalid_structure in
            ConditionalOperator { cond; then_branch; else_branch }
        | ParenExpr ->
            let subexpr =
              match list_of_children cursor with
              | [subexpr] -> expr_of_cxcursor subexpr
              | _ -> raise Invalid_structure in
            Paren subexpr
        | AddrLabelExpr ->
            let label =
              match list_of_children cursor with
              | [label] -> get_cursor_spelling label
              | _ -> raise Invalid_structure in
            AddrLabel label
        | InitListExpr ->
            InitList (list_of_children cursor |> List.map expr_of_cxcursor)
        | CompoundLiteralExpr ->
            let qual_type = cursor |> get_cursor_type |> of_cxtype in
            let init =
              match list_of_children cursor with
              | [init] -> init |> expr_of_cxcursor
              | _ -> raise Invalid_structure in
            CompoundLiteral { qual_type; init }
        | UnaryExpr ->
            unary_expr_of_cxcursor cursor
        | GenericSelectionExpr ->
            begin
              let controlling_expr, assocs =
                match list_of_children cursor with
                | [] -> raise Invalid_structure
                | controlling_expr :: assocs ->
                    controlling_expr |> expr_of_cxcursor,
                    assocs |> List.mapi @@ fun i sub_cursor ->
                      let ty =
                        ext_generic_selection_expr_get_assoc_type cursor i in
                      let ty : qual_type option =
                        if get_type_kind ty = Invalid then
                          None
                        else
                          Some (ty |> of_cxtype) in
                      (ty, sub_cursor |> expr_of_cxcursor) in
              GenericSelection { controlling_expr; assocs }
            end
        | UnexposedExpr ->
            begin
              match ext_get_cursor_kind cursor with
              | ImplicitCastExpr ->
                  let operand =
                    match list_of_children cursor with
                    | [operand] | [_; operand] -> expr_of_cxcursor operand
                    | _ -> raise Invalid_structure in
                  let qual_type = get_cursor_type cursor |> of_cxtype in
                  Cast { kind = Implicit; qual_type; operand }
              | BinaryConditionalOperator ->
                  let cond, else_branch =
                    match
                      list_of_children cursor |> List.map expr_of_cxcursor with
                    | [_; cond; _; else_branch] ->
                        cond, else_branch
                    | _ ->
                        raise Invalid_structure in
                  ConditionalOperator { cond; then_branch = None; else_branch }
              | UnaryExprOrTypeTraitExpr -> (* for Clang 3.8.1 *)
                  unary_expr_of_cxcursor cursor
              | _ -> UnexposedExpr { s = get_cursor_spelling cursor }
            end
        | _ -> UnknownExpr kind
      with Invalid_structure -> UnknownExpr kind

    and unary_expr_of_cxcursor cursor =
      let kind = cursor |> ext_unary_expr_get_kind in
      let argument =
        match list_of_children cursor with
        | [argument] -> ArgumentExpr (argument |> expr_of_cxcursor)
        | [] ->
            let qual_type =
              cursor |> ext_unary_expr_get_argument_type |> of_cxtype in
            ArgumentType qual_type
        | _ -> raise Invalid_structure in
      UnaryExpr { kind; argument }

    and make_template make_body cursor =
      let rec extract_template_parameters accu children =
        match
          match children with
          | [] -> (None : template_parameter option), []
          | cursor :: tl ->
              match
                match get_cursor_kind cursor with
                | TemplateTypeParameter ->
                    let default =
                      ext_template_type_parm_decl_get_default_argument cursor in
                    let default : qual_type option =
                      match get_type_kind default with
                      | Invalid -> None
                      | _ -> Some (default |> of_cxtype) in
                    Some (Class { default } : template_parameter_kind)
                | NonTypeTemplateParameter ->
                    let qual_type = get_cursor_type cursor |> of_cxtype in
                    let default : expr option =
                      match list_of_children cursor with
                      | [] -> None
                      | [default] -> Some (default |> expr_of_cxcursor)
                      | _ -> raise Invalid_structure in
                    Some (NonType { qual_type; default })
                | TemplateTemplateParameter ->
                    let parameters, others =
                      extract_template_parameters []
                        (list_of_children cursor) in
                    let default : string option =
                      match others with
                      | [] -> None
                      | [default] -> Some (get_cursor_spelling default)
                      | _ -> raise Invalid_structure in
                    Some (Template { parameters; default })
                | _ -> None
              with
              | None -> None, children
              | Some kind ->
                  let name = get_cursor_spelling cursor in
                  let parameter_pack =
                    ext_template_parm_is_parameter_pack cursor in
                  Some (node ~cursor { name; kind; parameter_pack}), tl
        with
        | None, tl -> List.rev accu, tl
        | Some parameter, tl ->
            extract_template_parameters (parameter :: accu) tl in
      let parameters, others =
        extract_template_parameters [] (list_of_children cursor) in
      let decl = make_body others in
      match parameters with
      | [] -> decl
      | _ -> TemplateDecl { parameters; decl = node ~cursor decl }

    and ident_ref_of_cxcursor cursor =
      let ident = get_cursor_spelling cursor in
      let rec pop_ref list ident =
        match
          match list with
          | hd :: tl -> Some (get_cursor_kind hd, hd, tl)
          | [] -> None
        with
        | Some (TypeRef, hd, tl) ->
            let ty = get_cursor_type hd in
            let decl = get_type_declaration ty in
            let type_ref = pop_ref tl (get_cursor_spelling decl) in
            let qual_type = of_cxtype ty in
            TypeRef { type_ref; qual_type; ident }
        | Some (NamespaceRef, hd, tl) ->
            let namespace_ref = pop_ref tl (get_cursor_spelling hd) in
            NamespaceRef { namespace_ref; ident }
        | _ -> Ident ident in
      pop_ref (List.rev (list_of_children cursor)) ident

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

  let parse_file_res ?index ?command_line_args ?unsaved_files ?clang_options
      ?options filename =
    parse_file_res ?index ?command_line_args ?unsaved_files
      ?options:clang_options filename |>
    Result.map @@ of_cxtranslationunit ?options

  let parse_file ?index ?command_line_args ?unsaved_files ?clang_options
      ?options filename =
    parse_file ?index ?command_line_args ?unsaved_files ?options:clang_options
      filename |>
    of_cxtranslationunit ?options

  let parse_string_res ?index ?filename ?command_line_args ?language
      ?unsaved_files ?clang_options ?options string =
    parse_string_res ?index ?filename ?command_line_args ?language
      ?unsaved_files ?options:clang_options string |>
    Result.map @@ of_cxtranslationunit ?options

  let parse_string ?index ?filename ?command_line_args ?language ?unsaved_files
      ?clang_options ?options string =
    parse_string ?index ?filename ?command_line_args ?language ?unsaved_files
      ?options:clang_options string |>
    of_cxtranslationunit ?options

  let get_presumed_location source_location =
    match source_location with
    | Clang source_location ->
        let filename, line, column = get_presumed_location source_location in
        { filename; line; column }
    | Concrete concrete_location -> concrete_location

  let get_expansion_location source_location =
    match source_location with
    | Clang source_location ->
        let file, line, column, _offset =
          get_expansion_location source_location in
        { filename = get_file_name file; line; column }
    | Concrete concrete_location -> concrete_location
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

  let get_field_bit_width field =
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

  let make ?(const = false) ?(volatile = false) ?(restrict = false) desc : t =
    { cxtype = get_cursor_type (get_null_cursor ());
      const; volatile; restrict; desc }

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

  let get_align_of (ty : t) = type_get_align_of ty.cxtype

  let get_typedef_underlying_type ?options (qual_type : t) =
    Clang__bindings.get_type_declaration qual_type.cxtype |>
      Clang__bindings.get_typedef_decl_underlying_type |>
      of_cxtype ?options

  let get_declaration ?options (qual_type : t) =
    get_type_declaration qual_type.cxtype |> Decl.of_cxcursor ?options

  let iter_fields ?options f (qual_type : t) =
    qual_type.cxtype |> iter_type_fields @@ fun x ->
      f (Decl.of_cxcursor ?options x)

  let list_of_fields ?options (qual_type : t) =
    qual_type.cxtype |> list_of_type_fields |> List.map @@
      Decl.of_cxcursor ?options
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

module Printer = struct
  include Clang__printer
end
