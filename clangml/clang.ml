module Bindings = Clang__bindings

include Bindings

include Clang__compat

module Types = Clang__types

include Types

include Clang__utils

module Command_line = Clang__command_line

let includedir =
  List.fold_left Filename.concat Clangml_config.includedir
    [".."; "lib"; "clang"; Clangml_config.version; "include"]

let option_cursor_bind f cursor : 'a option =
  if get_cursor_kind cursor = InvalidCode then
    None
  else
    f cursor

let option_cursor f cursor : 'a option =
  option_cursor_bind (fun x -> Some (f x)) cursor

let rec extract_prefix_from_list'
    (p : 'a -> 'b option) (accu : 'b list) (list : 'a list)
    : 'b list * 'a list =
  match
    match list with
    | [] -> (None : _ option), []
    | hd :: tl ->
        match p hd with
        | None -> None, list
        | (Some _) as y -> y, tl
  with
  | Some x, tl -> extract_prefix_from_list' p (x :: accu) tl
  | None, tl -> List.rev accu, tl

let extract_prefix_from_list p list =
  extract_prefix_from_list' p [] list

let string_chop_prefix_opt prefix s =
  let prefix_length = String.length prefix in
  let length = String.length s in
  if prefix_length <= length then
    if String.sub s 0 prefix_length = prefix then
      Some (String.sub s prefix_length (length - prefix_length))
    else
      None
  else
    None

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

  let var
      ?(linkage = NoLinkage) ?var_init ?(constexpr = false) var_name var_type =
    { linkage; var_name; var_type; var_init; constexpr }

  let function_decl
      ?(linkage = NoLinkage) ?body ?(deleted = false) ?(constexpr = false)
      function_type name =
    { linkage; body; deleted; constexpr; function_type; name }

  let function_type ?(calling_conv = (C : cxcallingconv)) ?parameters result =
    { calling_conv; parameters; result }

  let parameters ?(variadic = false) non_variadic =
    { variadic; non_variadic }

  let parameter ?default qual_type name =
    { default; qual_type; name }

  let new_instance ?(placement_args = []) ?array_size ?init ?args qual_type =
    let init =
      match init, args with
      | Some _, Some _ ->
          invalid_arg
          "Clang.Ast.new_instance: ~init and ~args are mutually exclusive"
      | init, None -> init
      | None, Some args ->
          Some (node (Construct {
            name = get_type_spelling qual_type.cxtype;
            args; })) in
    New { placement_args; qual_type; array_size; init }

  let delete ?(global_delete = false) ?(array_form = false) argument =
    Delete { global_delete; array_form; argument }

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

  module Options = Clang__ast_options

  module type OptionsS = sig
    val options : Options.t
  end

  module Converter (Options : OptionsS) = struct
    let options = Options.options

    exception Invalid_structure

    (* Hack for having current function declaration to provide function name
       for predefined identifiers on Clang 3.4 and Clang 3.5. *)
    let current_decl = ref (get_null_cursor ())

    let filter_out_attributes list =
      list |> List.filter begin fun cursor ->
        match get_cursor_kind cursor with
        | UnexposedAttr -> false
        | _ -> true
      end

    let filter_out_typeref list =
      list |> List.filter begin fun cursor ->
        match get_cursor_kind cursor with
        | TypeRef -> false
        | _ -> true
      end

    let make_integer_literal i =
      match
        if options.convert_integer_literals then
          int_of_cxint_opt i
        else
          None
      with
      | None -> CXInt i
      | Some i -> Int i

    let ident_of_string s =
      match s with
      | "operator==" -> BinaryOperatorRef EQ
      | _ ->
          match string_chop_prefix_opt "operator " s with
          | Some target -> ConversionOperatorRef target
          | _ -> Ident s

    let ident_ref_of_namespaces namespaces =
      match List.rev namespaces with
      | [] -> raise Invalid_structure
      | hd :: tl ->
          tl |> List.fold_left begin fun namespace_ref ident ->
            NamespaceRef { namespace_ref; ident }
          end (Ident hd)

    let extract (f : 'a -> 'b option) (list : 'a list) : 'b list * 'a list =
      let acc_match, acc_others =
        list |> List.fold_left begin fun (acc_match, acc_others) item ->
          match f item with
          | None -> (acc_match, item :: acc_others)
          | Some x -> (x :: acc_match, acc_others)
        end ([], []) in
      List.rev acc_match, List.rev acc_others

    let rec ident_ref_of_cxcursor cursor =
      let ident = get_cursor_spelling cursor in
      let rec pop_ref list ident =
        match
          match list with
          | hd :: tl ->
              Some (get_cursor_kind hd, hd, tl)
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
        | Some (OverloadedDeclRef, hd, _) ->
            ident_of_string (get_cursor_spelling hd)
        | _ -> ident_of_string ident in
      pop_ref (List.rev (list_of_children cursor)) ident

    and make_template_name name =
      match ext_template_name_get_kind name with
      | Template ->
          NameTemplate (
            ext_template_name_get_as_template_decl name |> get_cursor_spelling)
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
            begin
              match ext_type_get_kind cxtype with
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
              | TemplateTypeParm->
                  TemplateTypeParm (get_type_spelling cxtype);
              | SubstTemplateTypeParm->
                  SubstTemplateTypeParm (get_type_spelling cxtype);
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
              | kind -> UnexposedType kind
            end in
      match desc with
      | ParenType inner when options.ignore_paren_in_types ->
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
        | CXXMethod
        | ConversionFunction ->
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
            let init =
              ext_field_decl_get_in_class_initializer cursor |>
              option_cursor expr_of_cxcursor in
            Field { name; qual_type; bitwidth; init }
        | CXXAccessSpecifier ->
            AccessSpecifier (cursor |> get_cxxaccess_specifier)
        | UsingDirective ->
            let namespaces =
              list_of_children cursor |> List.map begin fun c ->
                  match get_cursor_kind c with
                  | NamespaceRef -> get_cursor_spelling c
                  | _ -> raise Invalid_structure
              end in
            let namespace = ident_ref_of_namespaces namespaces in
            Using { namespace; decl = None }
        | UsingDeclaration ->
            let namespaces, others =
              list_of_children cursor |> extract begin fun c ->
                  match get_cursor_kind c with
                  | NamespaceRef -> Some (get_cursor_spelling c)
                  | _ -> None
              end in
            begin
              match others with
              | [decl_ref] when
                  get_cursor_kind decl_ref = OverloadedDeclRef ->
                    Using {
                    namespace = ident_ref_of_namespaces namespaces;
                    decl = Some (get_cursor_spelling decl_ref) }
              | _ -> raise Invalid_structure
            end
        | Constructor ->
            let children = list_of_children cursor in
            let rec extract_initializer_list children =
              match children with
              | member :: value :: children when
                  get_cursor_kind member = MemberRef ->
                    let init_expr = expr_of_cxcursor value in
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
              constexpr = ext_function_decl_is_constexpr cursor;
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
        | NamespaceAlias ->
            let alias, original =
              match ident_ref_of_cxcursor cursor with
              | NamespaceRef { namespace_ref; ident } -> ident, namespace_ref
              | _ -> raise Invalid_structure in
            NamespaceAlias { alias; original }
        | kind ->
            match ext_decl_get_kind cursor with
            | Empty -> EmptyDecl
            | LinkageSpec ->
                let language =
                  language_of_ids
                    (ext_linkage_spec_decl_get_language_ids cursor) in
                let decls =
                  list_of_children cursor |> List.map decl_of_cxcursor in
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
                  list_of_children cursor |> List.map decl_of_cxcursor in
                let inline = ext_namespace_decl_is_inline cursor in
                Namespace { name; declarations; inline }
            | ext_kind -> UnknownDecl (kind, ext_kind)
      with Invalid_structure ->
        UnknownDecl (get_cursor_kind cursor, ext_decl_get_kind cursor)

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
      let namespaces, others = list_of_children cursor |>
        extract begin fun c : string option ->
          match get_cursor_kind c with
          | NamespaceRef -> Some (get_cursor_spelling c)
          | _ -> None
        end in
      let others = others |> filter_out_typeref in
      let qual_type = cursor |> get_cursor_type |> of_cxtype in
      let qual_type =
        match namespaces, qual_type with
        | _ :: _, { desc = Elaborated { keyword; named_type }} ->
            let namespace_ref = ident_ref_of_namespaces namespaces in
            let add_namespace ident_ref =
              match ident_ref with
              | Ident ident ->
                  NamespaceRef {
                    namespace_ref;
                    ident }
              | _ -> raise Invalid_structure in
            let desc =
              match named_type.desc with
              | Record ident_ref -> Record (add_namespace ident_ref)
              | Enum ident_ref -> Enum (add_namespace ident_ref)
              | _ -> named_type.desc in
            { qual_type with
              desc = Elaborated { keyword;
                named_type = { named_type with desc }}}
        | _ -> qual_type in
      node ~cursor {
          name = cursor |> get_cursor_spelling;
          qual_type;
          default =
            match others with
            | [] -> None
            | [default] -> Some (default |> expr_of_cxcursor)
            | _ -> raise Invalid_structure }

    and function_type_of_decl cursor =
      (* Hack for Clang 3.4 and 3.5! See current_decl declaration. *)
      current_decl := cursor;
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
      Clang__ast.Function { linkage; function_type; name; body;
        deleted = ext_function_decl_is_deleted cursor;
        constexpr = ext_function_decl_is_constexpr cursor; }

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
      let constexpr = ext_function_decl_is_constexpr cursor in
      if can_be_function && type_ref = None then
        let linkage = cursor |> get_cursor_linkage in
        Function { linkage; function_type; name; body; deleted; constexpr }
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
          deleted; constexpr
        }

    and parameters_of_cxtype cxtype =
      if cxtype |> get_type_kind = FunctionProto then
        let non_variadic =
          List.init (get_num_arg_types cxtype) @@ fun i ->
            let qual_type = of_cxtype (get_arg_type cxtype i) in
            node ~qual_type {
              name = ""; qual_type; default = None } in
        let variadic = is_function_type_variadic cxtype in
        Some { non_variadic; variadic }
      else
        None

    and function_type_of_cxtype parameters cxtype =
      let calling_conv = cxtype |> get_function_type_calling_conv in
      let result = cxtype |> get_result_type |> of_cxtype in
      { calling_conv; result; parameters; }

    and var_decl_of_cxcursor cursor =
      node ~cursor (var_decl_desc_of_cxcursor cursor)

    and var_decl_desc_of_cxcursor cursor =
      let linkage = cursor |> get_cursor_linkage in
      let var_name = get_cursor_spelling cursor in
      let var_type = of_cxtype (get_cursor_type cursor) in
      let var_init : 'a option =
        if ext_var_decl_has_init cursor then
          begin
            let init_value = list_of_children cursor |> List.rev |> List.hd in
            option_call_expr_of_cxcursor init_value
          end
        else
          None in
      { linkage; var_name; var_type; var_init;
        constexpr = ext_var_decl_is_constexpr cursor }

    and enum_decl_of_cxcursor cursor =
      let name = get_cursor_spelling cursor in
      let constants =
        list_of_children cursor |> List.map @@ fun cursor ->
          match get_cursor_kind cursor with
          | EnumConstantDecl -> enum_constant_of_cxcursor cursor
          | _ -> raise Invalid_structure in
      EnumDecl { name; constants }

    and enum_constant_of_cxcursor cursor =
      let constant_name = get_cursor_spelling cursor in
      let constant_init =
        match filter_out_attributes (list_of_children cursor) with
        | [init] -> Some (expr_of_cxcursor init)
        | [] -> None
        | _ -> raise Invalid_structure in
      node ~cursor { constant_name; constant_init }

    and record_decl_of_cxcursor keyword cursor =
      record_decl_of_children keyword cursor (list_of_children cursor)

    and base_specifier_of_cxcursor_opt cursor =
      match get_cursor_kind cursor with
      | CXXBaseSpecifier -> Some (base_specifier_of_cxcursor cursor)
      | _ -> None

    and base_specifier_of_cxcursor cursor =
      { ident = get_cursor_spelling cursor;
        virtual_base = is_virtual_base cursor;
        access_specifier = get_cxxaccess_specifier cursor;
      }

    and record_decl_of_children keyword cursor children =
      let name = get_cursor_spelling cursor in
      let bases, children =
        extract_prefix_from_list base_specifier_of_cxcursor_opt children in
      let fields = fields_of_children children in
      RecordDecl { keyword; name; bases; fields }

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
      | Paren subexpr when options.ignore_paren ->
          node ~cursor subexpr.desc
      | Cast { kind = Implicit; operand }
        when options.ignore_implicit_cast ->
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
        | DeclRefExpr -> DeclRef (ident_ref_of_cxcursor cursor)
        | CallExpr ->
            begin match ext_stmt_get_kind cursor with
            | CXXConstructExpr ->
                Construct {
                  name = get_cursor_spelling cursor;
                  args = list_of_children cursor |> List.map expr_of_cxcursor;
                }
            | _ ->
                Call {
                  callee = ext_call_expr_get_callee cursor |> expr_of_cxcursor;
                  args = List.init (ext_call_expr_get_num_args cursor) begin
                    fun i ->
                      ext_call_expr_get_arg cursor i |> expr_of_cxcursor
                  end
                }
            end
        | CStyleCastExpr ->
            cast_of_cxcursor CStyle cursor
        | MemberRefExpr ->
            begin match list_of_children cursor |> filter_out_typeref with
            | [] -> MemberRef (get_cursor_spelling cursor)
            | [lhs] ->
                let base = expr_of_cxcursor lhs in
                let field = get_cursor_referenced cursor in
                let ident = ident_of_string (get_cursor_spelling field) in
                let field = node ~cursor:field ident in
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
                cursor |> ext_cxxnew_expr_get_allocated_type |> of_cxtype in
              let array_size =
                cursor |> ext_cxxnew_expr_get_array_size |>
                option_cursor expr_of_cxcursor in
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
                  of_cxtype)
              else
                ArgumentExpr (ext_cxxtypeid_expr_get_expr_operand cursor |>
                  expr_of_cxcursor) in
            Typeid argument
        | UnexposedExpr ->
            begin
              match ext_stmt_get_kind cursor with
              | ImplicitCastExpr ->
                  cast_of_cxcursor Implicit cursor
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
                    sub.desc
                  else
                    ExprWithCleanups sub
              | MaterializeTemporaryExpr ->
                  let sub =
                    match list_of_children cursor with
                    | [sub] -> expr_of_cxcursor sub
                    | _ -> raise Invalid_structure in
                  if options.ignore_materialize_temporary_expr then
                    sub.desc
                  else
                    MaterializeTemporaryExpr sub
              | CXXStaticCastExpr ->
                  cast_of_cxcursor Static cursor
              | CXXDynamicCastExpr ->
                  cast_of_cxcursor Dynamic cursor
              | kind ->
                  match compat_stmt_kind kind with
                  | CXXFoldExpr ->
                      let lhs, rhs =
                        match list_of_children cursor with
                        | [lhs; rhs] ->
                            expr_of_cxcursor lhs, expr_of_cxcursor rhs
                        | _ -> raise Invalid_structure in
                      let operator = ext_cxxfold_expr_get_operator cursor in
                      Fold { lhs; operator; rhs }
                  | _ ->
                      UnexposedExpr kind
            end
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
        | _ -> UnknownExpr kind
      with Invalid_structure -> UnknownExpr kind

    and cast_of_cxcursor kind cursor =
      let operand =
        match List.rev (list_of_children cursor) with
        | operand :: _ -> expr_of_cxcursor operand
        | _ -> raise Invalid_structure in
      let qual_type = get_cursor_type cursor |> of_cxtype in
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
        List.init (ext_lambda_expr_get_capture_count cursor)
          begin fun i ->
            ext_lambda_expr_get_capture cursor i |>
            lambda_capture_of_capture
          end in
      let children = list_of_children cursor in
      let parameters, children =
        if ext_lambda_expr_has_explicit_parameters cursor then
          let parameters, children = extract_parameters children in
          Some parameters, children
        else
          None, children in
      let result_type =
        if ext_lambda_expr_has_explicit_result_type cursor then
          Some (
            cursor |> ext_lambda_expr_get_call_operator |> get_cursor_type |>
              get_result_type |> of_cxtype)
        else
          None in
      let body =
        match List.rev children with
        | body :: _ -> stmt_of_cxcursor body
        | _ -> raise Invalid_structure in
      Lambda {
        captures; body; parameters; result_type;
        capture_default = ext_lambda_expr_get_capture_default cursor;
        is_mutable = ext_lambda_expr_is_mutable cursor; }

    and extract_parameters children =
      let rec extract accu children =
        match children with
        | child :: tail when get_cursor_kind child = ParmDecl ->
            extract (parameter_of_cxcursor child :: accu) tail
        | _ -> List.rev accu, children in
      extract [] children

    and lambda_capture_of_capture capture =
      let capture_kind = ext_lambda_capture_get_kind capture in
      let captured_var_name : string option =
        match capture_kind with
        | ByCopy | ByRef ->
            capture |> ext_lambda_capture_get_captured_var |>
            option_cursor get_cursor_spelling
        | This | StarThis | VLAType -> None in
      { implicit = ext_lambda_capture_is_implicit capture;
        capture_kind;
        captured_var_name;
        pack_expansion = ext_lambda_capture_is_pack_expansion capture;
      }

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

    and template_parameter_of_cxcursor_opt cursor : template_parameter option =
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
            let default : expr option =
              match list_of_children cursor |> filter_out_typeref with
              | [] -> None
              | [default] -> Some (default |> expr_of_cxcursor)
              | _ -> raise Invalid_structure in
            Some (NonType { parameter_type; default })
        | TemplateTemplateParameter ->
            let parameters, others = extract_template_parameters cursor in
            let default : string option =
              match others with
              | [] -> None
              | [default] -> Some (get_cursor_spelling default)
              | _ -> raise Invalid_structure in
            Some (Template { parameters; default })
        | _ -> None
      with
      | None -> None
      | Some parameter_kind ->
          let parameter_name = get_cursor_spelling cursor in
          let parameter_pack =
            ext_template_parm_is_parameter_pack cursor in
          let node =
            node ~cursor
              { parameter_name; parameter_kind; parameter_pack} in
          Some node

    and extract_template_parameters cursor =
      extract_prefix_from_list template_parameter_of_cxcursor_opt
        (list_of_children cursor)

    and make_template make_body cursor =
      let parameters, others = extract_template_parameters cursor in
      let decl = make_body others in
      match parameters with
      | [] -> decl
      | _ -> TemplateDecl { parameters; decl = node ~cursor decl }

    let translation_unit_of_cxcursor cursor =
      let filename = get_cursor_spelling cursor in
      let items = list_of_children cursor |> List.map decl_of_cxcursor in
      node ~cursor { filename; items }

    let of_cxtranslationunit tu =
      translation_unit_of_cxcursor (get_translation_unit_cursor tu)
  end

  let of_cxtype ?(options = Options.default) tu =
    let module Convert = Converter (struct let options = options end) in
    Convert.of_cxtype tu

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
    has_severity filter (tu |> to_cxtranslationunit)

  let concrete_of_cxsourcelocation kind location =
    match kind with
    | Presumed ->
        let filename, line, column = get_presumed_location location in
        { filename; line; column }
    | Expansion ->
        let file, line, column, _offset = get_expansion_location location in
        { filename = get_file_name file; line; column }

  let concrete_of_source_location kind location =
    match location with
    | Clang location -> concrete_of_cxsourcelocation kind location
    | Concrete location -> location
end

module Decl = struct
  type t = Ast.decl

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.decl_of_cxcursor cur

  let get_typedef_underlying_type ?options decl =
    decl |> Ast.cursor_of_node |>
    get_typedef_decl_underlying_type |> Ast.of_cxtype ?options

  let get_field_bit_width field =
    field |> Ast.cursor_of_node |> get_field_decl_bit_width
end

module Type = struct
  type t = Ast.qual_type

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
  type t = Ast.expr

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.expr_of_cxcursor cur
end

module Stmt = struct
  type t = Ast.stmt

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.stmt_of_cxcursor cur
end

module Enum_constant = struct
  type t = Ast.enum_constant

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.enum_constant_of_cxcursor cur

  let get_value enum_constant =
    enum_constant |> Ast.cursor_of_node |> get_enum_constant_decl_value
end

module Translation_unit = struct
  type t = Ast.translation_unit

  let make ?(filename = "") items : Ast.translation_unit_desc =
    { filename; items }
end
