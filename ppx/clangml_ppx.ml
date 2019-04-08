[@@@warning "-30"]

open Clang.Ast

module From_OCaml_406 =
  Migrate_parsetree.Convert (Migrate_parsetree.OCaml_406)
    (Migrate_parsetree.OCaml_current)

module To_OCaml_406 =
  Migrate_parsetree.Convert (Migrate_parsetree.OCaml_current)
    (Migrate_parsetree.OCaml_406)

let quote_type_desc_forward =
  ref (fun ?loc:_ _ -> failwith "uninitialized quote_type_desc_forward")

let quote_qual_type ?loc (qual_type : qual_type) =
  let quote_bool ?loc b =
    To_OCaml_406.copy_expression ([%derive.quote: bool] ?loc b) in
  let const = quote_bool ?loc qual_type.const in
  let volatile = quote_bool ?loc qual_type.volatile in
  let restrict = quote_bool ?loc qual_type.restrict in
  From_OCaml_406.copy_expression
    [%expr Clang.Type.make ~const:[%e const] ~volatile:[%e volatile]
       ~restrict:[%e restrict]
       [%e To_OCaml_406.copy_expression
          (!quote_type_desc_forward ?loc qual_type.desc)]]

let quote_open_node quote_desc _quote_qual_type ?loc (node : 'a node) =
  From_OCaml_406.copy_expression
    [%expr Clang.Ast.node
       [%e To_OCaml_406.copy_expression (quote_desc ?loc node.desc)]]

module Clang__bindings = struct
  include (Clang__bindings :
    module type of struct include Clang__bindings end with
    type clang_ext_declkind := Clang__bindings.clang_ext_declkind and
    type clang_ext_typekind := Clang__bindings.clang_ext_typekind and
    type cxcursorkind := Clang__bindings.cxcursorkind and
    type clang_ext_unaryexpr := Clang__bindings.clang_ext_unaryexpr and
    type clang_ext_characterkind := Clang__bindings.clang_ext_characterkind and
    type clang_ext_unaryoperatorkind :=
      Clang__bindings.clang_ext_unaryoperatorkind and
    type clang_ext_binaryoperatorkind :=
      Clang__bindings.clang_ext_binaryoperatorkind and
    type clang_ext_elaboratedtypekeyword :=
      Clang__bindings.clang_ext_elaboratedtypekeyword and
    type cx_cxxaccessspecifier := Clang__bindings.cx_cxxaccessspecifier and
    type cxlinkagekind := Clang__bindings.cxlinkagekind and
    type cxtypekind := Clang__bindings.cxtypekind and
    type clang_ext_attrkind := Clang__bindings.clang_ext_attrkind and
    type cxcallingconv := Clang__bindings.cxcallingconv)

  type cxcursorkind = [%import: Clang__bindings.cxcursorkind]
      [@@deriving quote]

  let quote_cxint _ =
    failwith "Cannot quote cxint"

  let quote_cxfloat _ =
    failwith "Cannot quote cxfloat"

  let quote_cxtype _ =
    failwith "Cannot quote cxtype"

  type clang_ext_declkind = [%import: Clang__bindings.clang_ext_declkind]
      [@@deriving quote]

  type clang_ext_typekind = [%import: Clang__bindings.clang_ext_typekind]
      [@@deriving quote]

  type clang_ext_unaryexpr = [%import: Clang__bindings.clang_ext_unaryexpr]
      [@@deriving quote]

  type clang_ext_characterkind =
      [%import: Clang__bindings.clang_ext_characterkind]
      [@@deriving quote]

  type clang_ext_unaryoperatorkind =
      [%import: Clang__bindings.clang_ext_unaryoperatorkind]
      [@@deriving quote]

  type clang_ext_binaryoperatorkind =
      [%import: Clang__bindings.clang_ext_binaryoperatorkind]
      [@@deriving quote]

  type clang_ext_elaboratedtypekeyword =
      [%import: Clang__bindings.clang_ext_elaboratedtypekeyword]
      [@@deriving quote]

  type cx_cxxaccessspecifier = [%import: Clang__bindings.cx_cxxaccessspecifier]
      [@@deriving quote]

  type cxlinkagekind = [%import: Clang__bindings.cxlinkagekind]
      [@@deriving quote]

  type cxtypekind = [%import: Clang__bindings.cxtypekind]
      [@@deriving quote]

  type clang_ext_attrkind = [%import: Clang__bindings.clang_ext_attrkind]
      [@@deriving quote]

  type cxcallingconv = [%import: Clang__bindings.cxcallingconv]
      [@@deriving quote]
end

type expr = [%import: Clang.Ast.expr]
and expr_desc = [%import: Clang.Ast.expr_desc]
and decl = [%import: Clang.Ast.decl]
and decl_desc = [%import: Clang.Ast.decl_desc]
and stmt = [%import: Clang.Ast.stmt]
and stmt_desc = [%import: Clang.Ast.stmt_desc]
and translation_unit = [%import: Clang.Ast.translation_unit]
and translation_unit_desc = [%import: Clang.Ast.translation_unit_desc]
and type_desc = [%import: Clang.Ast.type_desc]
and unary_expr_or_type_trait = [%import: Clang.Ast.unary_expr_or_type_trait]
and unary_expr_kind = [%import: Clang.Ast.unary_expr_kind]
and cast_kind = [%import: Clang.Ast.cast_kind]
and unary_operator_kind = [%import: Clang.Ast.unary_operator_kind]
and binary_operator_kind = [%import: Clang.Ast.binary_operator_kind]
and character_kind = [%import: Clang.Ast.character_kind]
and integer_literal = [%import: Clang.Ast.integer_literal]
and floating_literal = [%import: Clang.Ast.floating_literal]
and languages = [%import: Clang.Ast.languages]
and parameters = [%import: Clang.Ast.parameters]
and parameter = [%import: Clang.Ast.parameter]
and parameter_desc = [%import: Clang.Ast.parameter_desc]
and cxx_access_specifier = [%import: Clang.Ast.cxx_access_specifier]
and elaborated_type_keyword = [%import: Clang.Ast.elaborated_type_keyword]
and enum_constant = [%import: Clang.Ast.enum_constant]
and enum_constant_desc = [%import: Clang.Ast.enum_constant_desc]
and var_decl_desc = [%import: Clang.Ast.var_decl_desc]
and cxx_method_binding_kind = [%import: Clang.Ast.cxx_method_binding_kind]
and function_type = [%import: Clang.Ast.function_type]
and linkage_kind = [%import: Clang.Ast.linkage_kind]
and template_parameter = [%import: Clang.Ast.template_parameter]
and template_parameter_desc = [%import: Clang.Ast.template_parameter_desc]
and template_parameter_kind = [%import: Clang.Ast.template_parameter_kind]
and template_name = [%import: Clang.Ast.template_name]
and template_argument = [%import: Clang.Ast.template_argument]
and label_ref = [%import: Clang.Ast.label_ref]
and var_decl = [%import: Clang.Ast.var_decl]
and builtin_type = [%import: Clang.Ast.builtin_type]
and attribute_kind = [%import: Clang.Ast.attribute_kind]
and calling_conv = [%import: Clang.Ast.calling_conv]
and friend_decl = [%import: Clang.Ast.friend_decl]
      [@@deriving quote]

let () =
  quote_type_desc_forward := quote_type_desc

let expr_mapper (mapper : Ast_mapper.mapper) (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_extension ({ loc; txt = "c" }, payload) ->
      begin
        match payload with
        | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc =
              Pexp_constant (Pconst_string (s, _)); _}, _); _}] ->
            let ast = Clang.Ast.parse_string s in
            quote_translation_unit ast
        | _ ->
            raise (Location.Error (Location.error ~loc
              "invalid payload"))
      end
  | _ ->
      Ast_mapper.default_mapper.expr mapper expr

let ppx_pattern_mapper = {
  Ast_mapper.default_mapper with
  expr = expr_mapper
}

let () =
  Migrate_parsetree.Driver.register ~name:"clangml.ppx"
    (module Migrate_parsetree.OCaml_current)
    (fun _ _ -> ppx_pattern_mapper)
