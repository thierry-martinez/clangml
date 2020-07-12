module HashtblExt (Key : Hashtbl.HashedType) = struct
  include Hashtbl.Make (Key)

  let find_default ~default tbl key =
    match find_opt tbl key with
    | None ->
        let value = default () in
        add tbl key value;
        value
    | Some value -> value
end

module StringHashtbl = HashtblExt (struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end)

module TypeHashtbl = HashtblExt (Clang.Lazy.Type)

let has_suffix ~suffix s =
  let suffix_len = String.length suffix in
  let len = String.length s in
  suffix_len <= len && String.sub s (len - suffix_len) suffix_len = suffix

let elaborated ?nested_name_specifier keyword named_type =
  Clang.Type.make (Elaborated { keyword; nested_name_specifier; named_type })

let function_decl fun_decl =
  Clang.Ast.node (Clang.Ast.Function fun_decl)

let record ident_ref =
  Clang.Type.make (Record ident_ref)

let type_def name = Clang.Type.make (Typedef name)

let cxcursor = type_def (Clang.Ast.identifier_name "CXCursor")

let int = Clang.Type.make (BuiltinType Int)

let unsigned_int = Clang.Type.make (BuiltinType UInt)

let bool = Clang.Type.make (BuiltinType Bool)

let auto = Clang.Type.make Auto

let index = "index"

let call callee args = Clang.Ast.node (Clang.Ast.Call { callee; args })

let decl_ref decl_ref = Clang.Ast.node (Clang.Ast.DeclRef decl_ref)

let string s =
  Clang.Ast.node (Clang.Ast.StringLiteral (Clang.Ast.literal_of_string s))

let const_int i = Clang.Ast.node (Clang.Ast.IntegerLiteral (Int i))

let const_bool b = Clang.Ast.node (Clang.Ast.BoolLiteral b)

let compound list = Clang.Ast.node (Clang.Ast.Compound list)

let decl list = Clang.Ast.node (Clang.Ast.Decl list)

let member ?base ?(arrow = false) field =
  Clang.Ast.node (Clang.Ast.Member { base; arrow; field })

let arrow base field =
  member ~base ~arrow:true field

let field_name s =
  Clang.Ast.FieldName (Clang.Ast.node s)

let field_of_string s =
  field_name (Clang.Ast.identifier_name s)

let var ~init qual_type name =
  Clang.Ast.node (Clang.Ast.Var (Clang.Ast.var ~var_init:init name qual_type))

let const (qual_type : Clang.Type.t) =
  { qual_type with const = true }

let lvalue_reference qual_type =
  Clang.Type.make (LValueReference qual_type)

let return e =
  Clang.Ast.node (Clang.Ast.Return e)

let array_subscript base index =
  Clang.Ast.node (Clang.Ast.ArraySubscript { base; index })

let conditional_operator cond ?then_branch else_branch =
  Clang.Ast.node (
    Clang.Ast.ConditionalOperator { cond; then_branch; else_branch })

let parameter qual_type name =
  Clang.Ast.node (Clang.Ast.parameter qual_type name)

let switch ?init ?condition_variable cond body =
  Clang.Ast.node (Clang.Ast.Switch { init; condition_variable; cond; body })

let case ?rhs lhs body =
  Clang.Ast.node (Clang.Ast.Case { lhs; rhs; body })

let default stmt =
  Clang.Ast.node (Clang.Ast.Default stmt)

let break =
  Clang.Ast.node (Clang.Ast.Break)

let null_stmt =
  Clang.Ast.node (Clang.Ast.Null)

let enum_decl ?(complete_definition = false) name constants =
  Clang.Ast.node (Clang.Ast.EnumDecl { name; constants; complete_definition })

let enum_constant ?constant_init constant_name : Clang.Ast.enum_constant =
  Clang.Ast.node
    ({ constant_name; constant_init } : Clang.Ast.enum_constant_desc)

type type_info = {
    ocaml_type : Parsetree.core_type;
    interface_type : Clang.Type.t;
    multiple : bool;
    access : Clang.Expr.t -> Clang.Expr.t;
    default : Clang.Expr.t;
  }

let rec get_type_info (qual_type : Clang.Lazy.Type.t) : type_info =
  let get_cursor_tu =
    lazy (call (decl_ref (Clang.Ast.identifier_name "getCursorTU"))
      [decl_ref (Clang.Ast.identifier_name "cursor")]) in
  match Lazy.force qual_type.desc with
  | Pointer { desc = lazy (Record { name = IdentifierName "Expr"})} ->
      { ocaml_type = [%type: 'expr];
        interface_type = cxcursor;
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "MakeCXCursor"))
            [e; Lazy.force get_cursor_tu]);
        default =
          call (decl_ref (Clang.Ast.identifier_name "MakeCXCursorInvalid"))
            [decl_ref (Clang.Ast.identifier_name "CXCursor_InvalidCode");
              Lazy.force get_cursor_tu]}
  | Record { name = IdentifierName "VersionTuple" } ->
      { ocaml_type = [%type: Clang__bindings.clang_ext_versiontuple];
        interface_type =
          elaborated Struct
            (record (Clang.Ast.identifier_name "clang_ext_VersionTuple"));
        multiple = false;
        access = (fun e ->
          call (decl_ref
              (Clang.Ast.identifier_name "makeVersionTuple")) [e]);
        default = decl_ref (Clang.Ast.identifier_name "zeroVersionTuple")}
  | Record { name = IdentifierName "StringRef" }
  | Pointer { desc = lazy (BuiltinType Char_S) } ->
      { ocaml_type = [%type: string];
        interface_type =
          type_def (Clang.Ast.identifier_name "CXString");
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createDup"))
            [e]);
        default =
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createRef"))
            [string ""] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "IdentifierInfo" }) } ->
      { ocaml_type = [%type: string];
        interface_type =
          type_def (Clang.Ast.identifier_name "CXString");
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createDup"))
            [call (arrow e (field_name
              (Clang.Ast.identifier_name "getName"))) []]);
        default =
          call (decl_ref (Clang.Ast.identifier_name "cxstring_createRef"))
            [string ""] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "FunctionDecl" }) } ->
      { ocaml_type = [%type: 'declaration_name];
        interface_type =
          elaborated Struct
            (record (Clang.Ast.identifier_name "clang_ext_DeclarationName"));
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "MakeDeclarationName"))
            [call (arrow e (field_name
              (Clang.Ast.identifier_name "getDeclName"))) [];
             Lazy.force get_cursor_tu]);
        default =
          call (decl_ref
                  (Clang.Ast.identifier_name "MakeDeclarationNameInvalid"))
            [Lazy.force get_cursor_tu] }
  | Pointer { desc = lazy (
        Record { name = IdentifierName "TypeSourceInfo" }) } ->
      { ocaml_type = [%type: 'qual_type];
        interface_type =
          elaborated Struct
            (record (Clang.Ast.identifier_name "clang_ext_TypeLoc"));
        multiple = false;
        access = (fun e ->
          call (decl_ref (Clang.Ast.identifier_name "MakeTypeLoc"))
            [call (arrow e (field_name
              (Clang.Ast.identifier_name "getTypeLoc"))) [];
             Lazy.force get_cursor_tu]);
        default =
          call (decl_ref
                  (Clang.Ast.identifier_name "MakeTypeLocInvalid"))
            [Lazy.force get_cursor_tu] }
  | Record { name = IdentifierName "ParamIdx" } ->
      { ocaml_type =  [%type: int];
        interface_type = unsigned_int;
        multiple = false;
        access = (fun e -> conditional_operator
          (call (member ~base:e (field_name (Clang.Ast.identifier_name "isValid"))) [])
          ~then_branch:(call (member ~base:e (field_name (Clang.Ast.identifier_name "getSourceIndex"))) [])
          (const_int 0));
        default = const_int 0; }
  | BuiltinType Int ->
      { ocaml_type =  [%type: int];
        interface_type = int;
        multiple = false;
        access = Fun.id;
        default = const_int 0; }
  | BuiltinType UInt ->
      { ocaml_type =  [%type: int];
        interface_type = unsigned_int;
        multiple = false;
        access = Fun.id;
        default = const_int 0; }
  | BuiltinType Bool ->
      { ocaml_type =  [%type: bool];
        interface_type = bool;
        multiple = false;
        access = Fun.id;
        default = const_bool false; }
  | Pointer qual_type ->
      let type_info = get_type_info qual_type in
      { ocaml_type = [%type: [%t type_info.ocaml_type] list];
        interface_type = type_info.interface_type;
        multiple = true;
        access = (fun e -> (type_info.access (array_subscript e
            (decl_ref (Clang.Ast.identifier_name index)))));
        default = type_info.default;
      }
  | _ ->
      Format.eprintf "Unsupported type %a@."
        (Refl.pp [%refl:Clang.Lazy.Ast.qual_type] []) qual_type;
      { ocaml_type =  [%type: bool];
        interface_type = bool;
        multiple = false;
        access = Fun.id;
        default = const_bool false; }

type argument_decl = {
    name : string;
    qual_type : Clang.Lazy.Type.t;
    type_info : type_info;
  }

let make_argument_decl name qual_type =
  { name; qual_type; type_info = get_type_info qual_type }

type argument_attribute = {
    attribute_name : string;
    getter : string;
    getter_result_type : Clang.Lazy.Type.t;
  }

type argument_desc = {
    type_info : type_info;
    mutable attributes : argument_attribute list;
  }

type context = {
    argument_table : argument_desc TypeHashtbl.t StringHashtbl.t;
    mutable constructors : Parsetree.constructor_declaration list;
    mutable decls : Clang.Decl.t list;
    mutable protos : Clang.Decl.t list;
    mutable cases : Parsetree.case list;
  }

let register_argument context attribute_name public_methods
    (argument : argument_decl) =
  let type_table =
    StringHashtbl.find_default context.argument_table argument.name
      ~default:(fun () -> TypeHashtbl.create 17) in
  let desc =
    TypeHashtbl.find_default type_table argument.qual_type
      ~default:(fun () ->
        { type_info = argument.type_info; attributes = [] }) in
  match
    StringHashtbl.find_opt public_methods (String.lowercase argument.name)
  with
  | None ->
      Format.fprintf Format.err_formatter
        "No getter for %s in %s@." argument.name attribute_name
  | Some (getter, getter_result_type) ->
      desc.attributes <-
        { attribute_name; getter; getter_result_type } :: desc.attributes

type annotated_field = Clang.Ast.cxx_access_specifier * Clang.Lazy.Ast.decl

let annotate_access_specifier
    (default_specifier : Clang.Ast.cxx_access_specifier)
    (fields : Clang.Lazy.Ast.decl list) : annotated_field list =
  let annotate_field (specifier, rev) (field : Clang.Lazy.Ast.decl) =
    match Lazy.force field.desc with
    | AccessSpecifier specifier -> (specifier, rev)
    | _ -> (specifier, (specifier, field) :: rev) in
  let _specifier, rev =
    List.fold_left annotate_field (default_specifier, []) fields in
  List.rev rev

let find_spelling (fields : annotated_field list) : string list option =
  let get_spelling ((specifier, field) : annotated_field) =
    match specifier with
    | CXXPublic ->
        begin match Lazy.force field.desc with
        | EnumDecl { name = "Spelling"; constants; _ } ->
            Some (List.map (
            fun ({ desc = lazy constant } : Clang.Lazy.Ast.enum_constant) ->
              constant.constant_name)
              constants)
        | _ -> None
        end
    | _ -> None in
  List.find_map get_spelling fields

let enumerate_public_methods (fields : annotated_field list) :
    (string * Clang.Lazy.Type.t) StringHashtbl.t =
  let table = StringHashtbl.create 17 in
  let add_field ((specifier, field) : annotated_field) =
    match specifier with
    | CXXPublic ->
        begin match Lazy.force field.desc with
        | CXXMethod {
          function_decl = { name = IdentifierName name; function_type }; _ } ->
            let normalized_name =
              String.lowercase name |>
              Stubgen_common.option_apply
                (Stubgen_common.string_remove_prefix ~prefix:"get") |>
              Stubgen_common.option_apply
                (Stubgen_common.string_remove_suffix ~suffix:"loc") in
            StringHashtbl.add table normalized_name (name, function_type.result)
        | _ -> ()
        end
    | _ -> () in
  fields |> List.iter add_field;
  table

let get_reduced_attribute_name attribute =
  Option.get (Stubgen_common.string_remove_suffix attribute ~suffix:"Attr")

let remove_trailing_underscore argument =
  Stubgen_common.option_apply
    (Stubgen_common.string_remove_suffix ~suffix:"_") argument

let get_type_spelling_name name =
  Printf.sprintf "clang_ext_%s_spelling" name

let cursor = "cursor"

let attr = "attr"

let qual_attr = "qual_attr"

let parameter_cursor = parameter cxcursor cursor

let get_cursor_attr =
  Clang.Ast.node (Clang.Ast.Decl [
    var auto attr ~init:(call
      (decl_ref (Clang.Ast.identifier_name "GetCursorAttr"))
      [decl_ref (Clang.Ast.identifier_name cursor)])])

let cast attr qual_attr class_name body =
  Clang.Ast.node (Clang.Ast.if_
        ~condition_variable:(Clang.Ast.node (Clang.Ast.var
          qual_attr auto
          ~var_init:(call
             (Clang.Ast.node (Clang.Ast.DeclRef
               (Clang.Ast.identifier_name "dyn_cast_or_null"
                 ~nested_name_specifier:[Clang.Ast.NamespaceName "llvm"]
                 ~template_arguments:[
                   Type (Clang.Type.make (Clang.Ast.Record
                     (Clang.Ast.identifier_name class_name
                     ~nested_name_specifier:[
                       Clang.Ast.NamespaceName "clang"])))])))
             [decl_ref (Clang.Ast.identifier_name attr)])))
        (Clang.Ast.node (Clang.Ast.DeclRef
          (Clang.Ast.identifier_name qual_attr))) body)

let namespace_clang = Clang.Ast.NamespaceName "clang"

let add_fun_decl context fun_decl =
  context.decls <- function_decl fun_decl :: context.decls;
  context.protos <-
    function_decl { fun_decl with body = None } :: context.protos

let unkeyword name =
  match name with
  | "type"
  | "module" -> name ^ "_"
  | _ -> name

let generate_attribute context name public_methods spelling arguments =
  let reduced_name = get_reduced_attribute_name name in
  let arguments =
    arguments |> List.map @@ fun argument ->
      { argument with name = remove_trailing_underscore argument.name } in
  let make_ocaml_argument argument =
    if has_suffix ~suffix:"_Size" argument.name
        || has_suffix ~suffix:"Length" argument.name then
      None
    else
      let name =
        argument.name |>
        Stubgen_common.option_apply
          (Stubgen_common.string_remove_suffix ~suffix:"Param") |>
        Stubgen_common.uncamelcase |>
        unkeyword in
      Some (name, argument.type_info.ocaml_type) in
  let ocaml_arguments =
    List.filter_map make_ocaml_argument arguments in
  let spelling =
    spelling |> Option.map (fun spelling ->
      (spelling, get_type_spelling_name reduced_name)) in
  let ocaml_arguments =
    match spelling with
    | None -> ocaml_arguments
    | Some (_, type_spelling_name) ->
      ("spelling", Ast_helper.Typ.constr
        (Metapp.mklid ~prefix:(Lident "Clang__bindings")
           (String.lowercase type_spelling_name)) [])
        :: ocaml_arguments in
  let args : Parsetree.constructor_arguments =
    match ocaml_arguments with
    | [(_, ty)] ->
        Pcstr_tuple [ty]
    | _ ->
        Pcstr_record (ocaml_arguments |> List.map
          (fun (name, ty) -> Ast_helper.Type.field (Metapp.mkloc name) ty)) in
  let constructor =
    Ast_helper.Type.constructor (Metapp.mkloc reduced_name) ~args in
  List.iter (register_argument context name public_methods) arguments;
  context.constructors <- constructor :: context.constructors;
  spelling |> Option.iter (fun (spelling, type_spelling_name) ->
    let constant_names = spelling |> List.map (fun constant ->
      constant, Printf.sprintf "clang_ext_%s_%s" reduced_name constant) in
    let last_constant = snd (List.hd (List.rev constant_names)) in
    let enum_constants = constant_names |> List.map (fun (_, constant) ->
      enum_constant constant) in
    let spelling_enum = enum_decl type_spelling_name enum_constants in
    let spelling_getter_name =
      Printf.sprintf "clang_ext_%s_getSpelling" reduced_name in
    let cases =
      constant_names |> List.map (fun (orig, prefixed) ->
        case (decl_ref (Clang.Ast.identifier_name orig
          ~nested_name_specifier:[
            namespace_clang;
            TypeSpec (record (Clang.Ast.identifier_name name));
            TypeSpec (record (Clang.Ast.identifier_name "Spelling"))]))
          (return (Some (decl_ref (Clang.Ast.identifier_name prefixed))))) in
    let switch =
      cast attr qual_attr name
        (switch (call (arrow (decl_ref (Clang.Ast.identifier_name qual_attr))
          (field_name (Clang.Ast.identifier_name "getSemanticSpelling"))) [])
          (compound cases)) in
    let return_default =
      return (Some (decl_ref (Clang.Ast.identifier_name last_constant))) in
    let list = [get_cursor_attr; switch; return_default] in
    let result =
      elaborated Enum (record (Clang.Ast.identifier_name type_spelling_name)) in
    let spelling_getter =
      Clang.Ast.function_decl (Clang.Ast.function_type
        ~parameters:(Clang.Ast.parameters [parameter_cursor]) result)
        (IdentifierName spelling_getter_name) ~body:(compound list) in
    context.protos <- spelling_enum :: context.protos;
    add_fun_decl context spelling_getter)

let is_parameter_base_class name =
  match name with
  | "Attr" | "TypeAttr" | "StmtAttr" | "InheritableAttr"
  | "InheritableParamAttr" | "ParameterABIAttr" -> true
  | _ -> false

let do_decl context (decl : Clang.Lazy.Decl.t) =
  match Lazy.force decl.desc with
  | RecordDecl {
        keyword = Class; name; fields = fields; bases = [
          { qual_type = { desc = lazy (Record {
              name = IdentifierName base_class; _ }); _}; _}]; _} when
    is_parameter_base_class base_class && not (is_parameter_base_class name) ->
      let extract_simple_field
          (field : Clang.Lazy.Decl.t) : argument_decl option =
        match Lazy.force field.desc with
        | Field { name; qual_type; _ } ->
            Some (make_argument_decl name qual_type)
        | _ -> None in
      let arguments, fields =
        match name with
        | "AlignedAttr" ->
            let expr_type =
              Clang.Lazy.Type.make (lazy (Pointer (
                Clang.Lazy.Type.make (lazy (Record (
                  Clang.Lazy.Ast.identifier_name "Expr")))))) in
            [make_argument_decl "alignmentExpr" expr_type], fields
        | _ ->
            Clang.extract_prefix_from_list extract_simple_field fields in
      let annotated_fields = annotate_access_specifier CXXPrivate fields in
      let spelling = find_spelling annotated_fields in
      if arguments <> [] || spelling <> None then
        let public_methods = enumerate_public_methods annotated_fields in
        generate_attribute context name public_methods spelling arguments
  | _ -> ()

let do_namespace context (decl : Clang.Lazy.Decl.t) =
  match Lazy.force decl.desc with
  | Namespace { name = "clang"; declarations; _ } ->
      List.iter (do_decl context) declarations
  | _ ->
      ()

let rec partition_map_aux (accu_some : 'b list) (accu_none : 'a list)
    (f : 'a -> 'b option) (l : 'a list) : 'b list * 'a list =
  match l with
  | [] -> List.rev accu_some, List.rev accu_none
  | head :: tail ->
      match f head with
      | None -> partition_map_aux accu_some (head :: accu_none) f tail
      | Some image -> partition_map_aux (image :: accu_some) accu_none f tail

let partition_map f l =
  partition_map_aux [] [] f l

let filter_singleton (key, desc) =
  match desc.attributes with
  | [attribute] -> Some (key, attribute, desc.type_info)
  | _ -> None

let generate_code context argument type_name_attr attributes ty type_info =
  let parameter_list = [parameter_cursor] in
  let parameter_list =
    if type_info.multiple then
      parameter_list @ [parameter unsigned_int index]
    else
      parameter_list in
  let parameters = Clang.Ast.parameters parameter_list in
  let result = type_info.interface_type in
  let argument_name =
    if type_info.multiple then
      Stubgen_common.option_apply
        (Stubgen_common.string_remove_suffix ~suffix:"s")
        argument
    else
      argument in
  let name =
    Printf.sprintf "clang_ext_%s_get%s" type_name_attr
      (String.capitalize argument_name) in
  let param = "param" in
  let make_attribute_cast attribute =
    let init =
      call (arrow (decl_ref (Clang.Ast.identifier_name qual_attr)) (FieldName
        (Clang.Ast.node (Clang.Ast.identifier_name attribute.getter)))) [] in
    let init =
      match Lazy.force attribute.getter_result_type.desc with
      | Elaborated {
          keyword = NoKeyword;
          nested_name_specifier = Some [NamespaceName "llvm"];
          named_type = {
            desc = lazy (
              TemplateSpecialization {
                name = NameTemplate "iterator_range"; _ }); _}; _} ->
          call (member ~base:init (field_of_string "begin")) []
      | _ -> init in
    cast attr qual_attr attribute.attribute_name
      (compound [decl [var (lvalue_reference (const auto)) param ~init];
       return (Some (type_info.access (decl_ref (Clang.Ast.identifier_name param))))]) in
  let switch =
    match attributes with
    | [attribute] -> make_attribute_cast attribute
    | _ ->
        let make_case attribute =
          [case (decl_ref
             (Clang.Ast.identifier_name
                (get_reduced_attribute_name attribute.attribute_name)
                 ~nested_name_specifier:[
                   namespace_clang;
                   Clang.Ast.NamespaceName "attr"]))
             (make_attribute_cast attribute); break] in
        let cases =
          compound
            (List.concat_map make_case attributes @ [default null_stmt]) in
        switch (call (arrow (decl_ref (Clang.Ast.identifier_name attr))
          (field_name (Clang.Ast.identifier_name "getKind"))) []) cases in
  let list = [
    get_cursor_attr;
    switch;
    return (Some type_info.default)] in
  let fun_decl =
    Clang.Ast.function_decl (Clang.Ast.function_type ~parameters result)
      (IdentifierName name) ~body:(compound list) in
  add_fun_decl context fun_decl

let main cflags llvm_config prefix =
  let command_line_args, _llvm_version =
    Stubgen_common.prepare_clang_options cflags llvm_config in
  let tu =
    Clang.Lazy.Ast.parse_string ~filename:"string.cpp" ~command_line_args {|
#include <clang/AST/Attr.h>
class Test {};
|} in
  Clang.Lazy.Ast.format_diagnostics Clang.warning_or_error Format.err_formatter tu;
  assert (not (Clang.Lazy.Ast.has_severity Clang.error tu));
  let context = {
    argument_table = StringHashtbl.create 17;
    constructors = [];
    decls = [];
    protos = [];
    cases = [];
  } in
  List.iter (do_namespace context) (Lazy.force tu.desc).items;
  context.argument_table |> StringHashtbl.iter (fun argument types ->
    let singletons, multiples =
      partition_map filter_singleton (List.of_seq (TypeHashtbl.to_seq types)) in
    singletons |> List.iter (fun (ty, singleton, type_info) ->
      generate_code context argument singleton.attribute_name
        [singleton] ty type_info);
    match multiples with
    | [] -> ()
    | [(key, argument_desc)] ->
        generate_code context argument "Attrs" argument_desc.attributes key
          argument_desc.type_info
    | _ ->
        multiples |> List.iter (fun (key, argument_desc) ->
          let type_attr_name =
            (List.hd (List.rev argument_desc.attributes)).attribute_name in
          generate_code context argument type_attr_name argument_desc.attributes
            key argument_desc.type_info));
  let other =
    Ast_helper.Type.constructor (Metapp.mkloc "Other")
      ~args:(Pcstr_tuple [[%type: Clang__bindings.clang_ext_attrkind]]) in
  let constructors = List.rev (other :: context.constructors) in
  let ty =
    Ast_helper.Type.mk (Metapp.mkloc "t")
      ~kind:(Ptype_variant constructors)
      ~params:[[%type: 'expr], Invariant; [%type: 'qual_type], Invariant;
        [%type: 'declaration_name], Invariant]
      ~attrs:[Metapp.Attr.mk (Metapp.mkloc "deriving") (PStr [%str refl])] in
  let type_decl =
    Ast_helper.Str.type_ Recursive [ty] in
  let convert =
    let cases =
      Ast_helper.Exp.case [%pat? other] [%expr Other other] :: context.cases in
    let pattern_matching =
      Ast_helper.Exp.match_ [%expr Clang__bindings.ext_attr_get_kind cursor]
        (List.rev cases) in
    [%stri
       let convert
           cursor expr_of_cxcursor of_type_loc declaration_name_of_cxcursor =
         [%e pattern_matching]] in
  let chan = open_out (prefix ^ "attributes.ml") in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
    let fmt = Format.formatter_of_out_channel chan in
    Format.fprintf fmt "%a@." Pprintast.structure
      [type_decl; convert]);
  let chan = open_out (prefix ^ "libclang_extensions_attrs.inc") in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
    let fmt = Format.formatter_of_out_channel chan in
    Format.fprintf fmt "%a@." Clang.Printer.decls (List.rev context.decls));
  let chan = open_out (prefix ^ "libclang_extensions_attrs_headers.inc") in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
    let fmt = Format.formatter_of_out_channel chan in
    Format.fprintf fmt "%a@." Clang.Printer.decls (List.rev context.protos))

let info =
  let doc = "generate stubs for ClangML attributes" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "stubgen" ~doc ~exits:Cmdliner.Term.default_exits ~man

let () =
  Cmdliner.Term.exit (Cmdliner.Term.eval (Stubgen_common.options main, info))
