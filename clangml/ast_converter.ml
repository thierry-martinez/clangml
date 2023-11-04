[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]
[%%metaload "config/clangml_config.cmxs"]

open Clang__bindings
open Clang__utils
open Special_bindings

let rec filter_out_prefix_from_list p list =
  match list with
  | hd :: tl when p hd -> filter_out_prefix_from_list p tl
  | _ -> list

module Init_list = struct
  let syntactic_form cursor =
    let result = ext_init_list_expr_get_syntactic_form cursor in
    match get_cursor_kind result with
    | InvalidCode -> cursor
    | _ -> result

  let semantic_form cursor =
    let result = ext_init_list_expr_get_semantic_form cursor in
    match get_cursor_kind result with
    | InvalidCode -> cursor
    | _ -> result

  let get_form (form : Clang__ast_options.init_list_form) cursor =
    match form with
    | Syntactic -> syntactic_form cursor
    | Semantic -> semantic_form cursor
end

module Make (Node : Clang__ast.NodeS) = struct
[%%meta Metapp.Stri.of_list ((new Metapp.filter)#structure [%str
  include Clang__ast.Common

  include Clang__ast.Custom (Node)

  let node ?decoration ?cursor ?location ?qual_type desc =
    let decoration : decoration =
      match decoration, cursor, location, qual_type with
      | Some decoration, None, None, None -> decoration
      | None, Some cursor, None, None -> Cursor cursor
      | None, None, location, qual_type -> Custom { location; qual_type }
      | _ -> invalid_arg "node" in
    { decoration; desc }

  let var
      ?(linkage = NoLinkage) ?(storage : storage_class = None) ?var_init
      ?(constexpr = false)
      ?(attributes = []) var_name var_type =
    { linkage; storage; var_name; var_type; var_init; constexpr; attributes }

  let function_decl
      ?(linkage = NoLinkage) ?(storage : storage_class = None) ?body
      ?(deleted = false) ?(constexpr = false)
      ?(inline_specified = false) ?(inlined = false) ?nested_name_specifier
      ?(attributes = []) ?(has_written_prototype = false)
      function_type name =
    { linkage; storage; body; deleted; constexpr; inline_specified; inlined;
      function_type; nested_name_specifier; name; attributes;
      has_written_prototype }

  let function_type ?(calling_conv = (C : cxcallingconv)) ?parameters
        ?exception_spec ?(ref_qualifier = (None : cxrefqualifierkind)) result =
    { calling_conv; parameters; result; exception_spec; ref_qualifier }

  let parameters ?(variadic = false) non_variadic =
    { variadic; non_variadic }

  let parameter ?default qual_type name =
    { default; qual_type; name }

  let ident_ref ?nested_name_specifier ?(template_arguments = [])
      name : ident_ref =
    { nested_name_specifier; name; template_arguments }

  let identifier_name ?nested_name_specifier ?template_arguments name =
    ident_ref ?nested_name_specifier ?template_arguments (IdentifierName name)

  let constant_array ?size_as_expr element size =
    ConstantArray { element; size; size_as_expr }

  let if_ ?init ?condition_variable ?else_branch cond then_branch =
    If { init; condition_variable; cond; then_branch; else_branch }

  let new_instance ?(placement_args = []) ?array_size ?init ?args qual_type =
    let init =
      match init, args with
      | Some _, Some _ ->
          invalid_arg
          "Clang.Ast.new_instance: ~init and ~args are mutually exclusive"
      | init, None -> init
      | None, Some args ->
          Some (node (Node.from_val (Construct { qual_type; args; }))) in
    New { placement_args; qual_type; array_size; init }

  let delete ?(global_delete = false) ?(array_form = false) argument =
    Delete { global_delete; array_form; argument }

  let enum_decl ?(complete_definition = true) ?(attributes = []) name
      constants =
    EnumDecl { name; constants; complete_definition; attributes }

  let cursor_of_decoration decoration =
    match decoration with
    | Cursor cursor -> cursor
    | Custom _ -> get_null_cursor ()

  let cursor_of_node node =
    cursor_of_decoration node.decoration

  let location_of_decoration decoration =
    match decoration with
    | Cursor cursor -> Clang (get_cursor_location cursor)
    | Custom { location; _ } ->
        match location with
        | Some location -> location
        | None -> Clang (get_cursor_location (get_null_cursor ()))

  let location_of_node node =
    location_of_decoration node.decoration

  let tokens_of_node node =
    let cursor = cursor_of_node node in
    let tu = cursor_get_translation_unit cursor in
    Array.map (get_token_spelling tu) (tokenize tu (get_cursor_extent cursor))

  include Clang__ast_utils

  module Options = Clang__ast_options

  module type OptionsS = sig
    val options : Options.t
  end

  let attribute_of_cxtype cxtype =
    let attribute_desc : attribute_desc =
      Other (ext_attributed_type_get_attr_kind cxtype) in
    node (Node.from_val attribute_desc)

  module Converter (Options : OptionsS) = struct
    let options = Options.options

    exception Invalid_structure

    (* Hack for having current function declaration to provide function name
       for predefined identifiers on Clang 3.4 and Clang 3.5. *)
    let current_decl = ref (get_null_cursor ())

    let is_not_ref cursor =
      match get_cursor_kind cursor with
      | TypeRef | NamespaceRef | ParmDecl -> false
      | _ -> true

    let make_integer_literal (i : cxint) (ty : cxtypekind) =
      match
        if options.convert_integer_literals then
          int_of_cxint_opt ~signed:(is_signed_integer ty) i
        else
          None
      with
      | None -> CXInt i
      | Some i -> Int i

    let is_template_parameter cursor =
      match get_cursor_kind cursor with
      | TemplateTypeParameter
      | NonTypeTemplateParameter
      | TemplateTemplateParameter -> true
      | _ -> false

    let rec convert_nested_name_specifier
        (name : clang_ext_nestednamespecifier)
        : nested_name_specifier option =
      let rec enumerate accu name =
        let component = convert_nested_name_specifier_component name in
        match component with
        | Some component ->
            let name = ext_nested_name_specifier_get_prefix name in
            enumerate (component :: accu) name
        | None -> accu in
      match ext_nested_name_specifier_get_kind name with
      | InvalidNestedNameSpecifier -> None
      | _ -> Some (enumerate [] name)

    and convert_nested_name_specifier_component
        (name : clang_ext_nestednamespecifier) =
      match ext_nested_name_specifier_get_kind name with
      | Identifier ->
          let ident =
            ext_nested_name_specifier_get_as_identifier name in
          Some (NestedIdentifier ident)
      | Namespace ->
          let decl = ext_nested_name_specifier_get_as_namespace name in
          Some (NamespaceName (get_cursor_spelling decl))
      | NamespaceAlias ->
          let decl = ext_nested_name_specifier_get_as_namespace name in
          Some (NamespaceAliasName (get_cursor_spelling decl))
      | TypeSpec ->
          let ty = ext_nested_name_specifier_get_as_type name in
          Some (TypeSpec (ty |> of_cxtype))
      | TypeSpecWithTemplate ->
          let ty = ext_nested_name_specifier_get_as_type name in
          Some (TypeSpecWithTemplate (ty |> of_cxtype))
      | Global -> Some Global
      | InvalidNestedNameSpecifier -> None
      | Super -> raise Invalid_structure

    and convert_nested_name_specifier_loc
        (name : clang_ext_nestednamespecifierloc)
        : nested_name_specifier option =
      let rec enumerate accu name =
        let ns = ext_nested_name_specifier_loc_get_nested_name_specifier name in
        let component =
          match ext_nested_name_specifier_get_kind ns with
          | TypeSpec ->
              let ty = ext_nested_name_specifier_loc_get_as_type_loc name in
              Some (TypeSpec (ty |> of_type_loc))
          | TypeSpecWithTemplate ->
              let ty = ext_nested_name_specifier_loc_get_as_type_loc name in
              Some (TypeSpecWithTemplate (ty |> of_type_loc))
          | _ -> convert_nested_name_specifier_component ns in
        match component with
        | None -> accu
        | Some component ->
            let name = ext_nested_name_specifier_loc_get_prefix name in
            enumerate (component :: accu) name in
      match ext_nested_name_specifier_get_kind
          (ext_nested_name_specifier_loc_get_nested_name_specifier name) with
      | InvalidNestedNameSpecifier -> None
      | _ -> Some (enumerate [] name)

    and declaration_name_of_cxcursor cursor =
      let name = ext_decl_get_name cursor in
      convert_declaration_name name

    and convert_declaration_name name =
      match ext_declaration_name_get_kind name with
      | Identifier ->
          IdentifierName (ext_declaration_name_get_as_identifier name)
      | CXXConstructorName ->
          ConstructorName
            (ext_declaration_name_get_cxxname_type name |> of_cxtype)
      | CXXDestructorName ->
          DestructorName
            (ext_declaration_name_get_cxxname_type name |> of_cxtype)
      | CXXConversionFunctionName ->
          ConversionFunctionName
            (ext_declaration_name_get_cxxname_type name |> of_cxtype)
      | CXXDeductionGuideName ->
          DeductionGuideName
            (ext_declaration_name_get_cxxdeduction_guide_template name |>
             decl_of_cxcursor)
      | CXXOperatorName ->
          OperatorName (ext_declaration_name_get_cxxoverloaded_operator name)
      | CXXLiteralOperatorName ->
          LiteralOperatorName
            (ext_declaration_name_get_cxxliteral_identifier name)
      | _ -> raise Invalid_structure

    and ident_ref_of_cxcursor cursor =
      let nested_name_specifier =
        cursor |> ext_decl_get_nested_name_specifier_loc |>
        convert_nested_name_specifier_loc in
      let name = declaration_name_of_cxcursor cursor in
      let template_arguments = extract_template_arguments cursor in
      { nested_name_specifier; name; template_arguments }

    and make_template_name name =
      match ext_template_name_get_kind name with
      | Template ->
          NameTemplate (
            ext_template_name_get_as_template_decl name |> get_cursor_spelling)
      | OverloadedTemplate -> OverloadedTemplate
      | AssumedTemplate -> AssumedTemplate
      | QualifiedTemplate -> QualifiedTemplate
      | DependentTemplate -> DependentTemplate
      | SubstTemplateTemplateParm -> SubstTemplateTemplateParm
      | SubstTemplateTemplateParmPack -> SubstTemplateTemplateParmPack
      | InvalidNameKind -> InvalidNameKind

    and make_template_argument argument : template_argument =
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
          let qual_type =
            of_cxtype (ext_template_argument_get_integral_type argument) in
          Integral {
            value =
              make_integer_literal
                (ext_template_argument_get_as_integral argument)
                (get_type_kind qual_type.cxtype);
            qual_type }
      | Template ->
          TemplateTemplateArgument (
            ext_template_argument_get_as_template_or_template_pattern
              argument |>
            make_template_name)
      | TemplateExpansion ->
          TemplateExpansion (
            ext_template_argument_get_as_template_or_template_pattern
              argument |>
            make_template_name)
      | Expression ->
          ExprTemplateArgument (
            ext_template_argument_get_as_expr argument |>
            expr_of_cxcursor)
      | Pack ->
          Pack (
              List.init
                (ext_template_argument_get_pack_size argument)
                begin fun i ->
                  ext_template_argument_get_pack_argument argument i |>
                  make_template_argument
                end
          )
      | _ -> raise Invalid_structure

    and of_type_loc (type_loc : clang_ext_typeloc) =
      let unqualify type_loc =
        match ext_type_loc_get_class type_loc with
        | Qualified -> ext_qualified_type_loc_get_unqualified_loc type_loc
        | _ -> type_loc in
      let cxtype = ext_type_loc_get_type type_loc in
      let type_loc = unqualify type_loc in
      let make_qual_type cxtype type_loc desc =
        { cxtype; type_loc = Some type_loc; desc;
          const = is_const_qualified_type cxtype;
          volatile = is_volatile_qualified_type cxtype;
          restrict = is_restrict_qualified_type cxtype; } in
      let type_class = ext_type_loc_get_class type_loc in
      let make_paren, type_loc, cxtype =
        match type_class with
        | Paren ->
            let type_loc' = ext_paren_type_loc_get_inner_loc type_loc in
            let cxtype' = ext_type_loc_get_type type_loc' in
            let type_loc' = unqualify type_loc' in
            let make_paren ty =
              if options.ignore_paren_in_types then
                ty
              else
                make_qual_type cxtype type_loc
                  (Node.from_val (ParenType ty)) in
            make_paren, type_loc', cxtype'
        | _ -> Fun.id, type_loc, cxtype in
      let desc () =
        match type_class with
        | Attributed ->
            let attribute =
              if Clangml_config.version.major >= 8 then
                ext_attributed_type_loc_get_attr type_loc |>
                attribute_of_cxcursor
              else
                attribute_of_cxtype cxtype in
            Attributed {
              modified_type =
                ext_attributed_type_loc_get_modified_loc type_loc |>
                of_type_loc;
              attribute
            }
        |_ ->
        match get_type_kind cxtype with
        | Invalid -> InvalidType
        | ConstantArray ->
            let size = cxtype |> get_array_size in
            let element =
              ext_array_type_loc_get_element_loc type_loc |> of_type_loc in
            let size_as_expr =
              ext_array_type_loc_get_size_expr type_loc |>
              expr_of_cxcursor in
            constant_array element size ~size_as_expr
        | Vector ->
            let element = cxtype |> get_element_type |> of_cxtype in
            let size = cxtype |> get_num_elements in
            Vector { element; size }
        | IncompleteArray ->
            let element =
              ext_array_type_loc_get_element_loc type_loc |> of_type_loc in
            IncompleteArray element
        | VariableArray ->
            let element =
              ext_array_type_loc_get_element_loc type_loc |> of_type_loc in
            let size =
              cxtype |> ext_variable_array_type_get_size_expr |>
              expr_of_cxcursor in
            VariableArray { element; size }
        | Pointer ->
            let pointee =
              ext_pointer_like_type_loc_get_pointee_loc type_loc |>
              of_type_loc in
            Pointer pointee
        | LValueReference ->
            let pointee =
              ext_pointer_like_type_loc_get_pointee_loc type_loc |>
              of_type_loc in
            LValueReference pointee
        | RValueReference ->
            let pointee =
              ext_pointer_like_type_loc_get_pointee_loc type_loc |>
              of_type_loc in
            RValueReference pointee
        | Enum ->
            Enum (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | Record ->
            Record (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | Typedef ->
            Typedef (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | FunctionProto
        | FunctionNoProto ->
            let function_type =
              type_loc |>
              function_type_of_type_loc (parameters_of_type_loc type_loc) in
            FunctionType function_type
        | Complex ->
            let element_type = cxtype |> get_element_type |> of_cxtype in
            Complex element_type
        | MemberPointer ->
            let class_ =
              ext_member_pointer_type_loc_get_class_loc type_loc |>
              of_type_loc in
            let pointee =
              ext_pointer_like_type_loc_get_pointee_loc type_loc |>
              of_type_loc in
            MemberPointer { pointee; class_ }
        | _ ->
            begin
              match ext_type_get_kind cxtype with
              | Elaborated -> (* Here for Clang <3.9.0 *)
                  let nested_name_specifier =
                    ext_type_loc_get_qualifier_loc type_loc |>
                    convert_nested_name_specifier_loc in
                  let keyword = ext_elaborated_type_get_keyword cxtype in
                  let named_type =
                    ext_elaborated_type_loc_get_named_type_loc type_loc |>
                    of_type_loc in
                  Elaborated { keyword; nested_name_specifier; named_type }
              | PackExpansion ->
                  let pattern =
                    ext_pack_expansion_type_loc_get_pattern_loc type_loc |>
                    of_type_loc in
                  PackExpansion pattern
              | TypeOf ->
                  TypeOf (ArgumentType (of_type_loc
                    (ext_type_of_type_loc_get_underlying_type type_loc)))
              | _ -> of_ext_type_kind cxtype
            end in
      match type_class with
      | Elaborated when
          options.ignore_no_keyword &&
          ext_elaborated_type_get_keyword cxtype = NoKeyword ->
        ext_elaborated_type_loc_get_named_type_loc type_loc |>
        of_type_loc
      | _ ->
        make_paren (make_qual_type cxtype type_loc (Node.from_fun desc))

    and of_ext_type_kind cxtype =
      match ext_type_get_kind cxtype with
      | Paren -> ParenType (cxtype |> ext_get_inner_type |> of_cxtype)
      | Elaborated -> (* Here for Clang <3.9.0 *)
          let nested_name_specifier =
            ext_type_get_qualifier cxtype |>
            convert_nested_name_specifier in
          let keyword = ext_elaborated_type_get_keyword cxtype in
          let named_type = ext_type_get_named_type cxtype |> of_cxtype in
          Elaborated { keyword; nested_name_specifier; named_type }
      | Attributed -> (* Here for Clang <8.0.0 *)
          Attributed {
            modified_type =
              ext_attributed_type_get_modified_type cxtype |> of_cxtype;
            attribute = attribute_of_cxtype cxtype;
          }
      | TemplateTypeParm ->
          TemplateTypeParm
            (cxtype |> ext_type_get_unqualified_type |>
              get_type_spelling)
      | SubstTemplateTypeParm ->
          SubstTemplateTypeParm
            (cxtype |> ext_type_get_unqualified_type |>
              get_type_spelling)
      | TemplateSpecialization ->
          let name =
            cxtype |>
            ext_template_specialization_type_get_template_name |>
            make_template_name in
          let args =
            List.init
              (ext_template_specialization_type_get_num_args cxtype)
            @@ fun i ->
              ext_template_specialization_type_get_argument cxtype i |>
              make_template_argument in
          TemplateSpecialization { name; args }
      | Builtin -> BuiltinType (get_type_kind cxtype)
      | Auto -> Auto
      | PackExpansion ->
          let pattern =
            ext_pack_expansion_get_pattern cxtype |> of_cxtype in
          PackExpansion pattern
      | Decltype ->
          let sub =
            ext_decltype_type_get_underlying_expr cxtype |>
            expr_of_cxcursor in
          Decltype sub
      | InjectedClassName ->
          let sub =
      ext_injected_class_name_type_get_injected_specialization_type
              cxtype |>
            of_cxtype in
          InjectedClassName sub
      | Using
          [@if [%meta Metapp.Exp.of_bool
            (Clangml_config.version.major >= 14)]] ->
          let sub = of_cxtype (ext_type_desugar cxtype) in
          if options.ignore_using_types then
            Node.force sub.desc
          else
            Using sub
      | Atomic ->
          Atomic (of_cxtype (ext_atomic_type_get_value_type cxtype))
      | TypeOfExpr ->
          TypeOf (ArgumentExpr (expr_of_cxcursor
            (ext_type_of_expr_type_get_underlying_expr cxtype)))
      | TypeOf ->
          TypeOf (ArgumentType (of_cxtype
            (ext_type_of_type_get_underlying_type cxtype)))
      | kind -> UnexposedType kind

    and of_cxtype cxtype =
      let desc () =
        match get_type_kind cxtype with
        | Invalid -> InvalidType
        | ConstantArray ->
            let element = cxtype |> get_array_element_type |> of_cxtype in
            let size = cxtype |> get_array_size in
            constant_array element size
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
        | RValueReference ->
            let pointee = cxtype |> get_pointee_type |> of_cxtype in
            RValueReference pointee
        | Enum ->
            Enum (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | Record ->
            Record (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | Typedef ->
            Typedef (cxtype |> get_type_declaration |> ident_ref_of_cxcursor)
        | FunctionProto
        | FunctionNoProto ->
            let function_type =
              cxtype |> function_type_of_cxtype (parameters_of_cxtype cxtype) in
            FunctionType function_type
        | Complex ->
            let element_type = cxtype |> get_element_type |> of_cxtype in
            Complex element_type
        | MemberPointer ->
            let pointee = cxtype |> get_pointee_type |> of_cxtype in
            let class_ = cxtype |> type_get_class_type |> of_cxtype in
            MemberPointer { pointee; class_ }
        | _ ->
            of_ext_type_kind cxtype in
      match ext_type_get_kind cxtype with
      | Paren when options.ignore_paren_in_types ->
        let inner = cxtype |> ext_get_inner_type |> of_cxtype in
        { inner with cxtype }
      | Elaborated when
          options.ignore_no_keyword &&
          ext_elaborated_type_get_keyword cxtype = NoKeyword ->
        ext_type_get_named_type cxtype |> of_cxtype
      | _ ->
        { cxtype; type_loc = None; desc = Node.from_fun desc;
          const = is_const_qualified_type cxtype;
          volatile = is_volatile_qualified_type cxtype;
          restrict = is_restrict_qualified_type cxtype; }

    and decl_of_cxcursor cursor =
      node ~cursor
        (Node.from_fun (fun () -> decl_desc_of_cxcursor cursor))

    and decl_desc_of_cxcursor cursor =
      try
        match get_cursor_kind cursor with
        | FunctionDecl ->
            Function (function_decl_of_cxcursor cursor)
        | FunctionTemplate ->
            make_template cursor begin
              decl_desc_of_cxcursor
                (ext_template_decl_get_templated_decl cursor)
            end
        | CXXMethod
        | ConversionFunction -> cxxmethod_decl_of_cxcursor cursor
        | VarDecl -> Var (var_decl_desc_of_cxcursor cursor)
        | StructDecl ->
            record_decl_of_cxcursor (Struct : clang_ext_elaboratedtypekeyword)
              cursor
        | UnionDecl ->
            record_decl_of_cxcursor (Union : clang_ext_elaboratedtypekeyword)
              cursor
        | ClassDecl ->
            record_decl_of_cxcursor (Class : clang_ext_elaboratedtypekeyword)
              cursor
        | ClassTemplate ->
            let cursor' = ext_template_decl_get_templated_decl cursor in
            let keyword =
              cursor' |>
              ext_tag_decl_get_tag_kind in
            make_template cursor
              (record_decl_of_cxcursor keyword cursor')
        | ClassTemplatePartialSpecialization ->
            let keyword = ext_tag_decl_get_tag_kind cursor in
            let decl () = record_decl_of_cxcursor keyword cursor in
            let result =
            TemplatePartialSpecialization
              { parameters = extract_template_parameters cursor;
                arguments = extract_template_arguments cursor;
                decl = node ~cursor (Node.from_fun decl) } in
            result
        | EnumDecl -> enum_decl_of_cxcursor cursor
        | TypedefDecl ->
            let name = get_cursor_spelling cursor in
            let underlying_type = cursor |>
              get_typedef_decl_underlying_type |> of_cxtype in
            TypedefDecl { name; underlying_type }
        | FieldDecl ->
            let name = get_cursor_spelling cursor in
            let qual_type =
              ext_declarator_decl_get_type_loc cursor |> of_type_loc in
            let bitwidth =
              if cursor_is_bit_field cursor then
                match last_child cursor with
                | Some bitwidth -> Some (expr_of_cxcursor bitwidth)
                | None -> raise Invalid_structure
              else
                None in
            let init =
              ext_field_decl_get_in_class_initializer cursor |>
              option_cursor_map expr_of_cxcursor in
            let attributes = attributes_of_decl cursor in
            Field { name; qual_type; bitwidth; init; attributes }
        | CXXAccessSpecifier ->
            AccessSpecifier (cursor |> get_cxxaccess_specifier)
        | UsingDirective ->
            let nested_name_specifier =
              cursor |> ext_decl_get_nested_name_specifier_loc |>
              convert_nested_name_specifier_loc in
            let namespace =
              ext_using_directive_decl_get_nominated_namespace cursor |>
              decl_of_cxcursor in
            UsingDirective { nested_name_specifier; namespace }
        | UsingDeclaration ->
            UsingDeclaration (ident_ref_of_cxcursor cursor)
        | Constructor ->
            let initializer_list =
              cursor |> list_of_cxxconstructor_initializers_map (fun init :
                   constructor_initializer ->
                let kind : constructor_initializer_kind =
                  if ext_cxxctor_initializer_is_base_initializer init then
                    Base {
                      qual_type = of_type_loc
                        (ext_cxxctor_initializer_get_type_source_info init);
                      pack_expansion =
                        ext_cxxctor_initializer_is_pack_expansion init;
                    }
                  else if
                    ext_cxxctor_initializer_is_delegating_initializer init then
                    Delegating (of_type_loc
                      (ext_cxxctor_initializer_get_type_source_info init))
                  else
                    let cursor = ext_cxxctor_initializer_get_member init in
                    Member {
                      indirect =
                        ext_cxxctor_initializer_is_indirect_member_initializer
                          init;
                      field =
                        node ~cursor
                          (Node.from_fun (fun () -> get_cursor_spelling cursor))
                    } in
                { kind;
                  init = expr_of_cxcursor
                    (ext_cxxctor_initializer_get_init init) }) in
            Constructor {
              class_name = get_cursor_spelling cursor;
              parameters = parameters_of_function_decl cursor;
              initializer_list;
              body = function_body_of_cxcursor cursor;
              defaulted = ext_cxxmethod_is_defaulted cursor;
              deleted = ext_function_decl_is_deleted cursor;
              implicit = ext_decl_is_implicit cursor;
              explicit = ext_cxxconstructor_is_explicit cursor;
              constexpr = ext_function_decl_is_constexpr cursor;
            }
        | Destructor ->
            let destructor_name = get_cursor_spelling cursor in
            Destructor {
              class_name = String.sub destructor_name 1
                (String.length destructor_name - 1);
              body = function_body_of_cxcursor cursor;
              defaulted = ext_cxxmethod_is_defaulted cursor;
              deleted = ext_function_decl_is_deleted cursor;
              exception_spec = extract_exception_spec (get_cursor_type cursor);
          }
        | TemplateTemplateParameter ->
            TemplateTemplateParameter (get_cursor_spelling cursor)
        | NamespaceAlias ->
            let alias = ident_ref_of_cxcursor cursor in
            let original =
              ident_ref_of_cxcursor (get_cursor_definition cursor) in
            NamespaceAlias { alias; original }
        | TypeAliasDecl ->
            let ident_ref = ident_ref_of_cxcursor cursor in
            let qual_type =
              get_typedef_decl_underlying_type cursor |> of_cxtype in
            TypeAlias { ident_ref; qual_type }
        | kind ->
            match ext_decl_get_kind cursor with
            | Empty -> EmptyDecl
            | LinkageSpec ->
                let language =
                  language_of_ids
                    (ext_linkage_spec_decl_get_language_ids cursor) in
                let decls =
                  list_of_children_map decl_of_cxcursor cursor in
                LinkageSpec { language; decls }
            | Friend -> (* No FriendDecl : cxcursortype in Clang <4.0.0 *)
                let friend_type = ext_friend_decl_get_friend_type cursor in
                if get_type_kind friend_type = Invalid then
                  let decl = ext_friend_decl_get_friend_decl cursor in
                  Friend (FriendDecl (decl_of_cxcursor decl))
                else
                  Friend (FriendType (of_cxtype friend_type))
            | Namespace ->
                let name = get_cursor_spelling cursor in
                let declarations =
                  list_of_children_map decl_of_cxcursor cursor in
                let inline = ext_namespace_decl_is_inline cursor in
                Namespace { name; declarations; inline }
            | StaticAssert ->
                (* No StaticAssert : cxcursortype in Clang <6.0.1 *)
                let constexpr, message =
                  match list_of_children cursor with
                  | [constexpr; message] ->
                      constexpr |> expr_of_cxcursor,
                      Some (message |> expr_of_cxcursor)
                  | [constexpr] ->
                      constexpr |> expr_of_cxcursor, None
                  | _ -> raise Invalid_structure in
                StaticAssert { constexpr; message }
            | VarTemplate ->
                make_template cursor begin
                  Var (var_decl_desc_of_cxcursor
                    (cursor |> ext_template_decl_get_templated_decl))
                end
            | TypeAliasTemplate ->
                (* No TypeAliasTemplateDecl : cxcursortype in Clang <3.8.0 *)
                make_template cursor begin
                  let ident_ref = ident_ref_of_cxcursor cursor in
                  let qual_type =
                    cursor |> ext_template_decl_get_templated_decl |>
                    get_typedef_decl_underlying_type |> of_cxtype in
                  TypeAlias { ident_ref; qual_type }
                end
            | IndirectField ->
                IndirectField (
                  (list_of_indirect_field_decl_chain_map decl_of_cxcursor
                     cursor))
            | Decomposition
                [@if [%meta Metapp.Exp.of_bool
                  (Clangml_config.version.major >= 4)]] ->
                let init =
                  match list_of_children cursor with
                  | [sub] -> Some (sub |> expr_of_cxcursor)
                  | [] -> None
                  | _ -> raise Invalid_structure in
                Decomposition {
                  bindings = List.init
                    (ext_decomposition_decl_get_bindings_count cursor)
                    begin fun i ->
                      ext_decomposition_decl_get_bindings cursor i |>
                      declaration_name_of_cxcursor
                    end;
                  init;
                }
            | Concept
                [@if [%meta Metapp.Exp.of_bool
                  (Clangml_config.version.major >= 9)]] ->
                let parameters = extract_template_parameters cursor in
                let name = declaration_name_of_cxcursor cursor in
                let constraint_expr =
                  cursor |> ext_concept_decl_get_constraint_expr |>
                  expr_of_cxcursor in
                Concept { parameters; name; constraint_expr }
            | Export
                [@if [%meta Metapp.Exp.of_bool
                  (Clangml_config.version.major >= 4)]] ->
                Export (list_of_decl_context_map decl_of_cxcursor cursor)
            | ext_kind -> UnknownDecl (kind, ext_kind)
      with Invalid_structure ->
        UnknownDecl (get_cursor_kind cursor, ext_decl_get_kind cursor)

    and function_body_of_cxcursor cursor : stmt option =
      if ext_function_decl_does_this_declaration_have_abody cursor then
        let cursor = ext_function_decl_get_body cursor in
        match get_cursor_kind cursor with
        | CompoundStmt
        | CXXTryStmt ->
            Some (stmt_of_cxcursor cursor)
        | _ -> None
      else
        None

    and parameters_of_function_decl cursor =
      { non_variadic =
      List.init (ext_function_decl_get_num_params cursor) (fun i ->
        parameter_of_cxcursor (ext_function_decl_get_param_decl cursor i));
      variadic = is_function_type_variadic (get_cursor_type cursor) }

    and parameters_of_function_decl_or_proto cursor =
      if cursor |> get_cursor_type |> get_type_kind = FunctionProto then
        Some (parameters_of_function_decl cursor)
      else
        None

    and parameter_of_cxcursor cursor =
      let desc () =
        (*
        let _namespaces, others =  |>
          extract begin fun c : string option ->
            match get_cursor_kind c with
            | NamespaceRef -> Some (get_cursor_spelling c)
            | _ -> None
          end in
        *)
        let type_loc = cursor |> ext_declarator_decl_get_type_loc in
        let qual_type =
          if type_loc |> ext_type_loc_get_class = InvalidTypeLoc then
            cursor |> get_cursor_type |> of_cxtype
          else
            type_loc|> of_type_loc in
        { name = cursor |> get_cursor_spelling;
          qual_type;
          default =
            if ext_var_decl_has_init cursor then
              let default = Option.get (last_child cursor) in
              Some (default |> expr_of_cxcursor)
            else
              None } in
      node ~cursor (Node.from_fun desc)

    and function_type_of_decl cursor =
      let parameters = parameters_of_function_decl_or_proto cursor in
      let type_loc = ext_declarator_decl_get_type_loc cursor in
      if ext_type_loc_get_class type_loc = InvalidTypeLoc then
        function_type_of_cxtype parameters (get_cursor_type cursor)
      else
        function_type_of_type_loc parameters type_loc

    and function_decl_of_cxcursor cursor =
      let cursor =
        match get_cursor_kind cursor with
        | FunctionTemplate ->
            ext_template_decl_get_templated_decl cursor
        | _ ->
            cursor in
      (* Hack for Clang 3.4 and 3.5! See current_decl declaration. *)
      current_decl := cursor;
      let nested_name_specifier =
        cursor |> ext_decl_get_nested_name_specifier_loc |>
        convert_nested_name_specifier_loc in
      let name = declaration_name_of_cxcursor cursor in
      let body = function_body_of_cxcursor cursor in
      let function_type = function_type_of_decl cursor in
      {
        linkage = get_cursor_linkage cursor;
        storage = ext_decl_get_storage_class cursor;
        function_type;
        nested_name_specifier; name; body;
        deleted = ext_function_decl_is_deleted cursor;
        constexpr = ext_function_decl_is_constexpr cursor;
        attributes = attributes_of_decl cursor;
        inline_specified = ext_function_decl_is_inline_specified cursor;
        inlined = ext_function_decl_is_inlined cursor;
        has_written_prototype = ext_function_decl_has_written_prototype cursor;
      }

    and attributes_of_decl cursor =
      if ext_decl_has_attrs cursor then
        List.init (ext_decl_get_attr_count cursor)
          (fun i -> attribute_of_cxcursor (ext_decl_get_attr cursor i))
      else
        []

    and cxxmethod_decl_of_cxcursor cursor =
      let type_ref =
        ext_cxxmethod_decl_get_parent cursor |>
        get_cursor_type |> of_cxtype in
      let function_decl = function_decl_of_cxcursor cursor in
      let num_templates =
        ext_function_decl_get_num_template_parameter_lists cursor in
      let templates =
        List.init num_templates (fun index ->
          extract_template_parameter_list
            (ext_function_decl_get_template_parameter_list cursor index)) in
      let add_template (decl : decl_desc) parameters : decl_desc =
        TemplateDecl { parameters; decl = node ~cursor (Node.from_val decl) } in
      List.fold_left add_template (CXXMethod {
        type_ref; function_decl;
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
        implicit = ext_decl_is_implicit cursor;
      }) templates

    and parameter_of_cxtype cxtype =
      let qual_type = of_cxtype cxtype in
      let desc () = { name = ""; qual_type; default = None } in
      node ~qual_type (Node.from_fun desc)

    and parameters_of_cxtype cxtype =
      if cxtype |> get_type_kind = FunctionProto then
        let non_variadic =
          List.init (get_num_arg_types cxtype) @@ fun i ->
            parameter_of_cxtype (get_arg_type cxtype i) in
        let variadic = is_function_type_variadic cxtype in
        Some { non_variadic; variadic }
      else
        None

    and parameters_of_type_loc type_loc =
      let cxtype = ext_type_loc_get_type type_loc in
      if cxtype |> get_type_kind = FunctionProto then
        let non_variadic =
          List.init (get_num_arg_types cxtype) @@ fun i ->
            let cursor = ext_function_type_loc_get_param type_loc i in
            if get_cursor_kind cursor = InvalidCode then
              parameter_of_cxtype (get_arg_type cxtype i)
            else
              parameter_of_cxcursor cursor in
        let variadic = is_function_type_variadic cxtype in
        Some { non_variadic; variadic }
      else
        None

    and function_type_of_cxtype parameters cxtype =
      let result = cxtype |> get_result_type |> of_cxtype in
      function_type_of_cxtype_result parameters cxtype result

    and function_type_of_cxtype_result parameters cxtype result =
      let calling_conv = cxtype |> get_function_type_calling_conv in
      let exception_spec : exception_spec option =
        extract_exception_spec cxtype in
      let ref_qualifier = cxtype |> type_get_cxxref_qualifier in
      { calling_conv; result; parameters; exception_spec; ref_qualifier }

    and function_type_of_type_loc parameters type_loc =
      let cxtype = ext_type_loc_get_type type_loc in
      let result =
        type_loc |> ext_function_type_loc_get_return_loc |> of_type_loc in
      function_type_of_cxtype_result parameters cxtype result

    and extract_exception_spec cxtype =
      match ext_function_proto_type_get_exception_spec_type cxtype with
      | NoExceptionSpecification -> None
      | DynamicNone -> Some (Throw [])
      | Dynamic ->
          let throws =
            List.init (ext_function_proto_type_get_num_exceptions cxtype)
              begin fun index ->
                ext_function_proto_type_get_exception_type cxtype index |>
                of_cxtype
              end in
          Some (Throw throws)
      | BasicNoexcept -> Some (Noexcept { expr = None; evaluated = None })
      | DependentNoexcept ->
          let expr =
            ext_function_proto_type_get_noexcept_expr cxtype |>
            expr_of_cxcursor in
          Some (Noexcept { expr = Some expr; evaluated = None })
      | NoexceptFalse ->
          let expr = ext_function_proto_type_get_noexcept_expr cxtype |>
            expr_of_cxcursor in
          Some (Noexcept { expr = Some expr; evaluated = Some false })
      | NoexceptTrue ->
          let expr = ext_function_proto_type_get_noexcept_expr cxtype |>
            expr_of_cxcursor in
          Some (Noexcept { expr = Some expr; evaluated = Some true })
      | other -> Some (Other other)

    and var_decl_of_cxcursor cursor =
      node ~cursor (Node.from_fun (fun () -> var_decl_desc_of_cxcursor cursor))

    and var_decl_desc_of_cxcursor cursor =
      let linkage = get_cursor_linkage cursor in
      let storage = ext_decl_get_storage_class cursor in
      let var_name = get_cursor_spelling cursor in
      let var_type = of_type_loc (ext_declarator_decl_get_type_loc cursor) in
      let var_init : 'a option =
        if ext_var_decl_has_init cursor then
          begin
            let init_value = Option.get (last_child cursor) in
            option_call_expr_of_cxcursor init_value
          end
        else
          None in
      { linkage; storage; var_name; var_type; var_init;
        constexpr = ext_var_decl_is_constexpr cursor;
        attributes = attributes_of_decl cursor; }

    and enum_decl_of_cxcursor cursor =
      let name = get_cursor_spelling cursor in
      let constants =
        cursor |> list_of_children_filter_map (fun cursor ->
          match get_cursor_kind cursor with
          | EnumConstantDecl -> Some (enum_constant_of_cxcursor cursor)
          | _ -> None) in
      let complete_definition = ext_tag_decl_is_complete_definition cursor in
      let attributes = attributes_of_decl cursor in
      EnumDecl { name; constants; complete_definition; attributes }

    and enum_constant_of_cxcursor cursor =
      let desc () =
        let constant_name = get_cursor_spelling cursor in
        let constant_init =
          match last_child cursor with
          | Some init -> Some (expr_of_cxcursor init)
          | None -> None in
        { constant_name; constant_init } in
      node ~cursor (Node.from_fun desc)

    and base_specifier_of_cxcursor cursor =
      { qual_type = of_type_loc (ext_cursor_get_type_loc cursor);
        virtual_base = is_virtual_base cursor;
        access_specifier = get_cxxaccess_specifier cursor;
      }

    and attribute_of_cxcursor (cursor : cxcursor) : attribute =
      let desc () =
        Attributes.convert cursor expr_of_cxcursor decl_of_cxcursor of_type_loc
          convert_declaration_name Fun.id Fun.id in
      node ~cursor (Node.from_fun desc)

    and record_decl_of_cxcursor (keyword : clang_ext_elaboratedtypekeyword)
        cursor =
      let attributes =
        list_of_iter_map attribute_of_cxcursor
          (fun f -> iter_decl_attributes f cursor) in
      let name = get_cursor_spelling cursor in
      let nested_name_specifier =
        cursor |> ext_decl_get_nested_name_specifier_loc |>
        convert_nested_name_specifier_loc in
      let final =
        attributes |> List.exists (fun (attr : attribute) ->
          Node.force attr.desc = (Other Final : attribute_desc)) in
      let bases =
        list_of_iter_map base_specifier_of_cxcursor
          (fun f -> iter_cxxrecorddecl_bases f cursor) in
      let fields =
        list_of_iter_filter_map (fun cursor ->
            if ext_decl_is_implicit cursor &&
               match ext_decl_get_kind cursor with
               | CXXConstructor
               | CXXDestructor -> options.ignore_implicit_constructors
               | Field when get_cursor_spelling cursor = "" ->
                   options.ignore_anonymous_fields
               | IndirectField -> options.ignore_indirect_fields
               | CXXRecord
                    when ext_record_decl_is_injected_class_name cursor ->
                   options.ignore_injected_class_names
               | CXXMethod -> options.ignore_implicit_methods
               | _ -> false then
              None
            else
              Some (decl_of_cxcursor cursor))
          (fun f -> iter_decl_context f cursor) in
      let complete_definition = ext_tag_decl_is_complete_definition cursor in
      let is_injected_class_name =
        ext_record_decl_is_injected_class_name cursor in
      RecordDecl {
        keyword; attributes; nested_name_specifier; name; bases; fields; final;
        complete_definition; is_injected_class_name; }

    and stmt_of_cxcursor cursor =
      let desc () =
        try
          match get_cursor_kind cursor with
          | NullStmt ->
              Null
          | CompoundStmt ->
              let items = list_of_children_map stmt_of_cxcursor cursor in
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
              let init =
                ext_if_stmt_get_init cursor |>
                option_cursor_map stmt_of_cxcursor in
              let condition_variable =
                ext_if_stmt_get_condition_variable cursor |>
                option_cursor_map var_decl_of_cxcursor in
              let cond =
                ext_if_stmt_get_cond cursor |>
                expr_of_cxcursor in
              let then_branch =
                ext_if_stmt_get_then cursor |>
                stmt_of_cxcursor in
              let else_branch =
                ext_if_stmt_get_else cursor |>
                option_cursor_map stmt_of_cxcursor in
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
              let decl = list_of_children_map decl_of_cxcursor cursor in
              Decl decl
          | ReturnStmt ->
              let value =
                match list_of_children cursor with
                | [value] -> Some (expr_of_cxcursor value)
                | [] -> None
                | _ -> raise Invalid_structure in
              Return value
          | GCCAsmStmt ->
              Asm (asm_of_cxcursor GCC cursor)
          | MSAsmStmt ->
              Asm (asm_of_cxcursor MS cursor)
          | CXXForRangeStmt ->
              ForRange {
                var =
                  ext_cxxfor_range_stmt_get_loop_variable cursor |>
                  var_decl_of_cxcursor;
                range =
                  ext_cxxfor_range_stmt_get_range_init cursor |>
                  expr_of_cxcursor;
                body =
                  ext_cxxfor_range_stmt_get_body cursor |>
                  stmt_of_cxcursor;
              }
          | CXXTryStmt ->
              let try_block, handlers =
                match list_of_children cursor with
                | try_block :: handlers ->
                    try_block |> stmt_of_cxcursor,
                    handlers |> List.map catch_of_cxcursor
                | _ -> raise Invalid_structure in
              Try { try_block; handlers }
          | UnexposedStmt ->
              begin
                match ext_stmt_get_kind cursor with
                | AttributedStmt ->
                    let attributes =
                      list_of_iter (ext_attributed_stmt_get_attrs cursor) |>
                      List.map attribute_of_cxcursor in
                    AttributedStmt {
                      attributes;
                      sub_stmts =
                        list_of_children_map stmt_of_cxcursor cursor;
                    }
                | _ -> raise Invalid_structure
              end
          | _ ->
              let decl_desc = decl_desc_of_cxcursor cursor in
              match decl_desc with
              | UnknownDecl _ ->
                  Expr (expr_of_cxcursor cursor)
              | _ ->
                  Decl [node ~cursor (Node.from_val decl_desc)]
        with Invalid_structure ->
          UnknownStmt (get_cursor_kind cursor, ext_stmt_get_kind cursor) in
      node ~cursor (Node.from_fun desc)

    and catch_of_cxcursor cursor : catch =
      match list_of_children cursor with
      | [block] ->
          { parameter = None;
            block = block |> stmt_of_cxcursor;
          }
      | [var; block] ->
          { parameter =
              Some (get_cursor_spelling var, get_cursor_type var |> of_cxtype);
            block = block |> stmt_of_cxcursor;
          }
      | _ -> raise Invalid_structure

    and asm_of_cxcursor asm_compiler_extension cursor =
      let asm_outputs =
        List.init (ext_asm_stmt_get_num_outputs cursor) (fun i -> {
          asm_constraint = ext_asm_stmt_get_output_constraint cursor i;
          asm_expr = ext_asm_stmt_get_output_expr cursor i |> expr_of_cxcursor;
        }) in
      let asm_inputs =
        List.init (ext_asm_stmt_get_num_inputs cursor) (fun i -> {
          asm_constraint = ext_asm_stmt_get_input_constraint cursor i;
          asm_expr = ext_asm_stmt_get_input_expr cursor i |> expr_of_cxcursor;
        }) in {
      asm_compiler_extension; asm_inputs; asm_outputs;
      asm_string = ext_asm_stmt_get_asm_string cursor;
    }

    and expr_of_cxcursor cursor =
      let desc () =
        match expr_desc_of_cxcursor cursor with
        | Paren subexpr when options.ignore_paren ->
            Node.force subexpr.desc
        | Cast { kind = Implicit; operand; _ }
          when options.ignore_implicit_cast ->
            Node.force operand.desc
        | desc -> desc in
      node ~cursor (Node.from_fun desc)

    and expr_desc_of_cxcursor cursor =
      let kind = get_cursor_kind cursor in
      try
        match kind with
        | IntegerLiteral ->
            let i = ext_integer_literal_get_value cursor in
            let literal =
              make_integer_literal i (get_type_kind (get_cursor_type cursor)) in
            IntegerLiteral literal
        | FloatingLiteral ->
            let f = ext_floating_literal_get_value cursor in
            let literal =
              match
                if options.convert_floating_literals then
                  float_of_cxfloat_opt f
                else
                  None
              with
              | None -> CXFloat f
              | Some f -> Float f in
            FloatingLiteral literal
        | StringLiteral ->
            StringLiteral {
              bytes = ext_string_literal_get_bytes cursor;
              byte_width = ext_string_literal_get_char_byte_width cursor;
              string_kind = ext_string_literal_get_kind cursor;
            }
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
        | CXXBoolLiteralExpr ->
            BoolLiteral (ext_cxxbool_literal_expr_get_value cursor)
        | CXXNullPtrLiteralExpr ->
            NullPtrLiteral
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
        | DeclRefExpr ->
            begin match ext_stmt_get_kind cursor with
            | SubstNonTypeTemplateParmExpr ->
                SubstNonTypeTemplateParm
                  (ext_subst_non_type_template_parm_expr_get_replacement cursor
                    |> expr_of_cxcursor)
            | _ ->
              DeclRef (ident_ref_of_cxcursor cursor)
            end
        | CallExpr ->
            begin
            let list_args cursor =
              cursor |> list_of_children_filter_map (fun cursor ->
                if is_not_ref cursor then
                  Some (expr_of_cxcursor cursor)
                else
                  None) in
            match ext_stmt_get_kind cursor with
            | CXXConstructExpr ->
                Construct {
                  qual_type = cursor |> get_cursor_type |> of_cxtype;
                  args = list_args cursor;
                }
            | CXXTemporaryObjectExpr ->
                TemporaryObject {
                  qual_type = cursor |> get_cursor_type |> of_cxtype;
                  args = list_args cursor;
                }
            | CXXUnresolvedConstructExpr ->
                UnresolvedConstruct {
                  qual_type = cursor |> get_cursor_type |> of_cxtype;
                  args = list_args cursor;
                }
            | _ ->
                let callee = ext_call_expr_get_callee cursor in
                if get_cursor_kind callee = InvalidCode then
                  raise Invalid_structure
                else
                  let callee = callee |> expr_of_cxcursor in
                  Call {
                    callee;
                    args = List.init (ext_call_expr_get_num_args cursor) begin
                      fun i ->
                        ext_call_expr_get_arg cursor i |> expr_of_cxcursor
                    end
                  }
            end
        | CStyleCastExpr ->
            cast_of_cxcursor CStyle cursor
        | MemberRefExpr ->
            let base =
              match
                list_of_children cursor |> filter_out_prefix_from_list begin
                  fun cursor ->
                    match get_cursor_kind cursor with
                    | TypeRef | OverloadedDeclRef -> true
                    | _ -> false
                end
              with
              | lhs :: _ -> Some (lhs |> expr_of_cxcursor)
              | [] -> None in
            let field = field_of_cxcursor cursor in
            let arrow = ext_member_ref_expr_is_arrow cursor in
            Member { base; arrow; field }
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
            let cursor = Init_list.get_form options.init_list_form cursor in
            let inits =
              List.init (ext_init_list_expr_get_num_inits cursor)
                (fun i -> ext_init_list_expr_get_init cursor i) in
            InitList (inits |> List.map expr_of_cxcursor)
        | CompoundLiteralExpr ->
            let qual_type = cursor |> get_cursor_type |> of_cxtype in
            let init =
              match list_of_children cursor |> filter_out_prefix_from_list begin
                  fun cursor ->
                    match get_cursor_kind cursor with
                    | TypeRef -> true
                    | _ -> false
                end with
              | [init] -> init |> expr_of_cxcursor
              | _ -> raise Invalid_structure in
            CompoundLiteral { qual_type; init }
        | UnaryExpr ->
            begin
              match ext_stmt_get_kind cursor with
              | CXXNoexceptExpr ->
                  let sub =
                    match list_of_children cursor with
                    | [sub] -> sub |> expr_of_cxcursor
                    | _ -> raise Invalid_structure in
                  NoexceptExpr sub
              | _ ->
                  unary_expr_of_cxcursor cursor
            end
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
        | LambdaExpr ->
            lambda_expr_of_cxcursor cursor
        | CXXThisExpr -> This
        | CXXNewExpr ->
            begin
              let placement_args =
                List.init (cursor |> ext_cxxnew_expr_get_num_placement_args)
                  begin fun i ->
                    ext_cxxnew_expr_get_placement_arg cursor i |>
                    expr_of_cxcursor
                  end in
              let qual_type =
                cursor |> ext_cxxnew_expr_get_allocated_type_loc |>
                of_type_loc in
              let array_size =
                cursor |> ext_cxxnew_expr_get_array_size |>
                option_cursor_map expr_of_cxcursor in
              let init =
                cursor |> ext_cxxnew_expr_get_initializer |>
                option_call_expr_of_cxcursor in
              New { placement_args; qual_type; array_size; init }
            end
        | CXXDeleteExpr ->
            let argument =
              match list_of_children cursor with
              | [operand] -> operand |> expr_of_cxcursor
              | _ -> raise Invalid_structure in
            Delete { argument;
              global_delete = ext_cxxdelete_expr_is_global_delete cursor;
              array_form = ext_cxxdelete_expr_is_array_form cursor; }
        | CXXTypeidExpr ->
            let argument =
              if ext_cxxtypeid_expr_is_type_operand cursor then
                ArgumentType (ext_cxxtypeid_expr_get_type_operand cursor |>
                  of_type_loc)
              else
                ArgumentExpr (ext_cxxtypeid_expr_get_expr_operand cursor |>
                  expr_of_cxcursor) in
            Typeid argument
        | PackExpansionExpr ->
            let sub =
              match list_of_children cursor with
              | [sub] -> expr_of_cxcursor sub
              | _ -> raise Invalid_structure in
            PackExpansionExpr sub
        | SizeOfPackExpr ->
            SizeOfPack (
              ext_size_of_pack_expr_get_pack cursor |> ident_ref_of_cxcursor)
        | CXXFunctionalCastExpr ->
            cast_of_cxcursor Functional cursor
        | CXXStaticCastExpr ->
            cast_of_cxcursor Static cursor
        | CXXDynamicCastExpr ->
            cast_of_cxcursor Dynamic cursor
        | CXXConstCastExpr ->
            cast_of_cxcursor Const cursor
        | CXXThrowExpr ->
            let sub =
              match list_of_children cursor with
              | [sub] -> Some (expr_of_cxcursor sub)
              | [] -> None
              | _ -> raise Invalid_structure in
            ThrowExpr sub
        | TemplateRef ->
            TemplateRef (ident_ref_of_cxcursor cursor)
        | OverloadedDeclRef ->
            OverloadedDeclRef (ident_ref_of_cxcursor cursor)
        | StmtExpr ->
            let sub =
              match list_of_children cursor with
              | [sub] -> sub
              | _ -> raise Invalid_structure in
            StmtExpr (stmt_of_cxcursor sub)
        | _ ->
            begin
              match ext_stmt_get_kind cursor with
              | ImplicitCastExpr ->
                  cast_of_cxcursor Implicit cursor
              | BinaryConditionalOperator ->
                  let cond, else_branch =
                    match
                      list_of_children_map expr_of_cxcursor cursor with
                    | [_; cond; _; else_branch] ->
                        cond, else_branch
                    | _ ->
                        raise Invalid_structure in
                  ConditionalOperator { cond; then_branch = None; else_branch }
              | UnaryExprOrTypeTraitExpr -> (* for Clang 3.8.1 *)
                  unary_expr_of_cxcursor cursor
              | PredefinedExpr ->
                  let kind = ext_predefined_expr_get_ident_kind cursor in
                  let function_name =
                    predefined_expr_get_function_name cursor !current_decl in
                  Predefined { kind; function_name }
              | ExprWithCleanups ->
                  let sub =
                    match list_of_children cursor with
                    | [sub] -> expr_of_cxcursor sub
                    | _ -> raise Invalid_structure in
                  if options.ignore_expr_with_cleanups then
                    Node.force sub.desc
                  else
                    ExprWithCleanups sub
              | MaterializeTemporaryExpr ->
                  let sub =
                    match list_of_children cursor with
                    | [sub] -> expr_of_cxcursor sub
                    | _ -> raise Invalid_structure in
                  if options.ignore_materialize_temporary_expr then
                    Node.force sub.desc
                  else
                    MaterializeTemporaryExpr sub
              | CXXBindTemporaryExpr ->
                  let sub =
                    match list_of_children cursor with
                    | [sub] -> expr_of_cxcursor sub
                    | _ -> raise Invalid_structure in
                  if options.ignore_bind_temporary_expr then
                    Node.force sub.desc
                  else
                    BindTemporaryExpr sub
              | CXXDefaultArgExpr ->
                  DefaultArg
              | CXXStdInitializerListExpr ->
                  StdInitializerList
                    (list_of_children_map expr_of_cxcursor cursor)
              | ImplicitValueInitExpr ->
                  ImplicitValueInit
                    (cursor |> get_cursor_type |> of_cxtype)
              | DesignatedInitExpr ->
                  let designators =
                    List.init (ext_designated_init_expr_size cursor) (fun i ->
                      match ext_designated_init_expr_get_kind cursor i with
                      | FieldDesignator ->
                          let field =
                            ext_designated_init_expr_get_field cursor i in
                          FieldDesignator (get_cursor_spelling field)
                      | ArrayDesignator ->
                          let index =
                            ext_designated_init_expr_get_array_index cursor i in
                          ArrayDesignator (expr_of_cxcursor index)
                      | ArrayRangeDesignator ->
                          let range_start =
                            ext_designated_init_expr_get_array_range_start
                              cursor i in
                          let range_end =
                            ext_designated_init_expr_get_array_range_end
                              cursor i in
                          ArrayRangeDesignator (
                            expr_of_cxcursor range_start,
                            expr_of_cxcursor range_end)) in
                  let init =
                    ext_designated_init_expr_get_init cursor |>
                    expr_of_cxcursor in
                  DesignatedInit { designators; init }
              | CXXFoldExpr
                [@if [%meta Metapp.Exp.of_bool
                  (Clangml_config.version >=
                    { major = 3; minor = 6; subminor = 0 })]] ->
                  let lhs, rhs =
                    match list_of_children cursor with
                    | [lhs; rhs] ->
                        Some (expr_of_cxcursor lhs),
                        Some (expr_of_cxcursor rhs)
                    | [sub] ->
                        if ext_cxxfold_expr_is_right_fold cursor then
                          Some (expr_of_cxcursor sub), None
                        else
                          None, Some (expr_of_cxcursor sub)
                    | _ -> raise Invalid_structure in
                  let operator = ext_cxxfold_expr_get_operator cursor in
                  Fold { lhs; operator; rhs }
              | ArrayInitLoopExpr
                [@if [%meta Metapp.Exp.of_bool
                  (Clangml_config.version.major >= 4)]] ->
                  let common_expr, sub_expr =
                    match list_of_children cursor with
                    | [common_expr; sub_expr] ->
                        common_expr |> expr_of_cxcursor,
                        sub_expr |> expr_of_cxcursor
                      | _ -> raise Invalid_structure in
                  ArrayInitLoop { common_expr; sub_expr }
              | ArrayInitIndexExpr
                [@if [%meta Metapp.Exp.of_bool
                  (Clangml_config.version.major >= 4)]] ->
                  ArrayInitIndex
              | RequiresExpr
                [@if [%meta Metapp.Exp.of_bool
                  (Clangml_config.version.major >= 10)]] ->
    let extract_expr_requirement requirement =
      let return_type_type_constraint =
        ext_expr_requirement_return_type_get_type_constraint requirement |>
        option_cursor_map expr_of_cxcursor in
      { expr = ext_expr_requirement_get_expr requirement |> expr_of_cxcursor;
        return_type_type_constraint =
          return_type_type_constraint |> Option.map (fun type_constraint ->
            { type_constraint;
              parameters =
ext_expr_requirement_return_type_get_type_constraint_template_parameter_list
                requirement |> extract_template_parameter_list; })} in

    let extract_requirement requirement : requirement =
      match ext_requirement_get_kind requirement with
      | Type -> Type (ext_type_requirement_get_type requirement |> of_type_loc)
      | Simple -> Simple (extract_expr_requirement requirement)
      | Compound -> Compound (extract_expr_requirement requirement)
      | Nested ->
          let expr =
            ext_nested_requirement_get_constraint_expr requirement |>
            expr_of_cxcursor in
          Nested expr in

                  Requires {
                    local_parameters =
                      List.init
                        (ext_requires_expr_get_local_parameter_count cursor)
                        (fun i ->
                          ext_requires_expr_get_local_parameter cursor i |>
                          parameter_of_cxcursor);
                    requirements =
                      List.init
                        (ext_requires_expr_get_requirement_count cursor)
                        (fun i ->
                          ext_requires_expr_get_requirement cursor i |>
                          extract_requirement);
                  }
              | ParenListExpr ->
                  let exprs =
                    List.map expr_of_cxcursor (list_of_children cursor) in
                  ParenList exprs
              | AtomicExpr ->
                  let op = ext_atomic_expr_get_op cursor in
                  let args =
                    List.map expr_of_cxcursor (list_of_children cursor) in
                  Atomic { op; args }
              | kind -> UnexposedExpr kind
            end
      with Invalid_structure ->
        UnknownExpr (kind, ext_stmt_get_kind cursor)

    and field_of_cxcursor cursor =
      match ext_stmt_get_kind cursor with
      | CXXPseudoDestructorExpr ->
          PseudoDestructor {
            nested_name_specifier =
              ext_decl_get_nested_name_specifier_loc cursor |>
              convert_nested_name_specifier_loc;
            qual_type =
              cursor |>
              ext_cxxpseudo_destructor_expr_get_destroyed_type_loc |>
              of_type_loc }
      | CXXDependentScopeMemberExpr ->
          DependentScopeMember {
            ident_ref = ident_ref_of_cxcursor cursor;
            template_arguments = extract_template_arguments cursor;
          }
      | UnresolvedMemberExpr ->
          UnresolvedMember (ident_ref_of_cxcursor cursor)
      | _ ->
          let field = get_cursor_referenced cursor in
          let ident () = ident_ref_of_cxcursor field in
          FieldName (node ~cursor:field (Node.from_fun ident))

    and cast_of_cxcursor kind cursor =
      let operand = expr_of_cxcursor (Option.get (last_child cursor)) in
      let qual_type =
        match kind with
        | Implicit -> get_cursor_type cursor |> of_cxtype
        | _ -> ext_cursor_get_type_loc cursor |> of_type_loc in
      Cast { kind; qual_type; operand }

    and option_call_expr_of_cxcursor cursor =
      cursor |> option_cursor_bind begin fun init ->
        if get_cursor_kind init = CallExpr &&
          list_of_children init = [] then
          None
        else
          Some (expr_of_cxcursor init)
      end

    and lambda_expr_of_cxcursor cursor =
      let captures =
        list_of_iter_map lambda_capture_of_capture
          (ext_lambda_expr_get_captures cursor) in
      let parameters =
        if ext_lambda_expr_has_explicit_parameters cursor then
          Some (cursor |> list_of_children_filter_map (fun cursor ->
            if get_cursor_kind cursor = ParmDecl then
              Some (parameter_of_cxcursor cursor)
            else
              None))
        else
          None in
      let result_type =
        if ext_lambda_expr_has_explicit_result_type cursor then
          Some (
            cursor |> ext_lambda_expr_get_call_operator |>
              ext_declarator_decl_get_type_loc |>
              ext_function_type_loc_get_return_loc |> of_type_loc)
        else
          None in
      let body = stmt_of_cxcursor (Option.get (last_child cursor)) in
      Lambda {
        captures; body; parameters; result_type;
        capture_default = ext_lambda_expr_get_capture_default cursor;
        is_mutable = ext_lambda_expr_is_mutable cursor; }

    and lambda_capture_of_capture capture =
      let capture_kind = ext_lambda_capture_get_kind capture in
      let captured_var_name : string option =
        match capture_kind with
        | ByCopy | ByRef ->
            capture |> ext_lambda_capture_get_captured_var |>
            option_cursor_map get_cursor_spelling
        | This | StarThis | VLAType -> None in
      { implicit = ext_lambda_capture_is_implicit capture;
        capture_kind;
        captured_var_name;
        pack_expansion = ext_lambda_capture_is_pack_expansion capture;
      }

    and unary_expr_of_cxcursor cursor =
      let kind = cursor |> ext_unary_expr_get_kind in
      let argument =
        if cursor |> ext_unary_expr_is_argument_type then
          let qual_type =
            cursor |> ext_unary_expr_get_argument_type_loc |> of_type_loc in
          ArgumentType qual_type
        else
          match list_of_children cursor with
          | [argument] -> ArgumentExpr (argument |> expr_of_cxcursor)
          | _ -> raise Invalid_structure in
      UnaryExpr { kind; argument }

    and template_parameter_of_cxcursor cursor : template_parameter =
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
            let parameter_type = get_cursor_type cursor |> of_cxtype in
            let default =
              ext_non_type_template_parm_decl_get_default_argument cursor in
            let default : expr option =
              match get_cursor_kind default with
              | InvalidCode -> None
              | _ -> Some (expr_of_cxcursor default) in
            Some (NonType { parameter_type; default })
        | TemplateTemplateParameter ->
            let parameters = extract_template_parameters cursor in
            let default : string option =
              match last_child cursor with
              | None -> None
              | Some cursor ->
                  if is_template_parameter cursor then
                    None
                  else
                    Some (get_cursor_spelling cursor) in
            Some (Template { parameters; default })
        | _ -> None
      with
      | None -> raise Invalid_structure
      | Some parameter_kind ->
          let desc () =
            let parameter_name = get_cursor_spelling cursor in
            let parameter_pack =
              ext_template_parm_is_parameter_pack cursor in
            { parameter_name; parameter_kind; parameter_pack} in
          node ~cursor (Node.from_fun desc)

    and extract_template_parameters cursor =
      extract_template_parameter_list
        (ext_template_decl_get_template_parameters cursor)

    and extract_template_parameter_list list = {
      list =
        List.init (ext_template_parameter_list_size list) begin fun i ->
          ext_template_parameter_list_get_param list i |>
          template_parameter_of_cxcursor
        end;
      requires_clause =
        ext_template_parameter_list_get_requires_clause list |>
        option_cursor_map expr_of_cxcursor;
    }

    and extract_template_arguments cursor =
      List.init (ext_cursor_get_num_template_args cursor) begin fun i ->
        ext_cursor_get_template_arg cursor i |>
        make_template_argument
      end

    and make_template ?(optional = false) cursor body =
      let parameters = extract_template_parameters cursor in
      match parameters with
      | { list = []; requires_clause = None } when optional -> body
      | _ ->
          TemplateDecl
            { parameters; decl = node ~cursor (Node.from_val body) }

    let translation_unit_of_cxcursor cursor =
      let desc () =
        let filename = get_cursor_spelling cursor in
        let items = list_of_children_map decl_of_cxcursor cursor in
        { filename; items } in
      node ~cursor (Node.from_fun desc)

    let of_cxtranslationunit tu =
      translation_unit_of_cxcursor (get_translation_unit_cursor tu)

    let rec type_loc_of_typeloc typeloc =
      let desc =
        match ext_type_loc_get_class typeloc with
        | Builtin ->
            BuiltinTypeLoc (ext_type_loc_get_type typeloc |> get_type_kind)
        | Typedef ->
            TypedefTypeLoc (
              ext_type_loc_get_type typeloc |> get_type_declaration |>
              ident_ref_of_cxcursor)
        | Pointer ->
            let pointee =
              ext_pointer_like_type_loc_get_pointee_loc typeloc |>
              type_loc_of_typeloc in
            PointerTypeLoc { pointee }
        | BlockPointer ->
            let pointee =
              ext_pointer_like_type_loc_get_pointee_loc typeloc |>
              type_loc_of_typeloc in
            BlockPointerTypeLoc { pointee }
        | MemberPointer ->
            let class_ =
              ext_member_pointer_type_loc_get_class_loc typeloc |>
              type_loc_of_typeloc in
            let pointee =
              ext_pointer_like_type_loc_get_pointee_loc typeloc |>
              type_loc_of_typeloc in
            MemberPointerTypeLoc { class_; pointee }
        | ConstantArray ->
            let size =
              ext_array_type_loc_get_size_expr typeloc |>
              expr_of_cxcursor in
            let element =
              ext_array_type_loc_get_element_loc typeloc |>
              type_loc_of_typeloc in
            ConstantArrayTypeLoc { size; element }
        | VariableArray ->
            let size =
              ext_array_type_loc_get_size_expr typeloc |>
              expr_of_cxcursor in
            let element =
              ext_array_type_loc_get_element_loc typeloc |>
              type_loc_of_typeloc in
            VariableArrayTypeLoc { size; element }
        | IncompleteArray ->
            let element =
              ext_array_type_loc_get_element_loc typeloc |>
              type_loc_of_typeloc in
            IncompleteArrayTypeLoc { element }
        | FunctionProto
        | FunctionNoProto ->
            let result =
              ext_function_type_loc_get_return_loc typeloc |>
              type_loc_of_typeloc in
            let parameters =
              List.init
                (ext_function_type_loc_get_num_params typeloc)
                (fun i ->
                  ext_function_type_loc_get_param typeloc i |>
                  parameter_of_cxcursor) in
            FunctionTypeLoc { result; parameters }
        | Paren ->
            (ext_paren_type_loc_get_inner_loc typeloc |>
            type_loc_of_typeloc).desc
        | Elaborated ->
            ElaboratedTypeLoc (
              ext_type_loc_get_type typeloc |> ext_type_get_named_type |>
              of_cxtype)
        | Record ->
            RecordTypeLoc (
              ext_type_loc_get_type typeloc |> get_type_declaration |>
              ident_ref_of_cxcursor)
        | Enum ->
            EnumTypeLoc (
              ext_type_loc_get_type typeloc |> get_type_declaration |>
              ident_ref_of_cxcursor)
        | Qualified ->
            QualifiedTypeLoc (
              ext_qualified_type_loc_get_unqualified_loc typeloc |>
              type_loc_of_typeloc)
        | c -> UnknownTypeLoc c in
      { typeloc = Some typeloc; desc }
  end

  let of_cxtype ?(options = Options.default) tu =
    let module Convert = Converter (struct let options = options end) in
    Convert.of_cxtype tu

  let of_type_loc ?(options = Options.default) tu =
    let module Convert = Converter (struct let options = options end) in
    Convert.of_type_loc tu

  let of_cxtranslationunit ?(options = Options.default) tu =
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

  let parse_string_res ?index ?filename ?command_line_args
      ?unsaved_files ?clang_options ?options string =
    parse_string_res ?index ?filename ?command_line_args
      ?unsaved_files ?options:clang_options string |>
    Result.map @@ of_cxtranslationunit ?options

  let parse_string ?index ?filename ?command_line_args ?unsaved_files
      ?clang_options ?options string =
    parse_string ?index ?filename ?command_line_args ?unsaved_files
      ?options:clang_options string |>
    of_cxtranslationunit ?options

  let to_cxtranslationunit tu =
    tu |> cursor_of_node |> cursor_get_translation_unit

  let seq_of_diagnostics tu =
    seq_of_diagnostics (tu |> to_cxtranslationunit)

  let format_diagnostics ?pp filter format tu =
    format_diagnostics ?pp filter format (tu |> to_cxtranslationunit)

  let has_severity filter tu =
    has_severity filter (tu |> to_cxtranslationunit)])]
end
