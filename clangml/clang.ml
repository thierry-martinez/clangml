[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]
[%%metaload "config/clangml_config.cmxs"]

module type S = Ast_sig.S

[%%metadef
let node_module s = Ppxlib.Ast_helper.Mod.structure [%str
  module Self = struct
    [%%meta Metapp.Stri.of_list s]

    let compare = Refl.compare [%refl: t] []

    let equal = Refl.equal [%refl: t] []

    let hash = Refl.hash [%refl: t] []

    let pp = Refl.pp [%refl: t] []

    let show = Refl.show [%refl: t] []
  end

  include Self

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)

  module Hashtbl = Hashtbl.Make (Self)
]]

module Bindings = struct
  include Clang__bindings

  include Special_bindings
end

include Bindings

external compare_cursors :
  cxcursor -> cxcursor -> int = "clang_ext_compare_cursor_boxed"

module Cursor = struct
  module Self = struct
    type t = cxcursor

    let compare = compare_cursors

    let equal = equal_cursors

    let hash = hash_cursor
  end

  include Self

  module Hashtbl = Hashtbl.Make (Self)

  module Set = Set.Make (Self)

  module Map = Map.Make (Self)
end

module Types = Clang__types

include Types

include Clang__utils

module Standard = Standard

module Command_line = Clang__command_line

let version () =
  ext_get_version ()

let make_include_dir path =
  List.fold_left Filename.concat Clangml_config.includedir path

let includedir =
  make_include_dir
    [Filename.parent_dir_name; "lib"; "clang"; Clangml_config.version_string;
     "include"]

let default_include_directories () =
  (*let cpp_lib = make_include_dir ["c++"; "v1"] in*)
  let macos_sdk =
    "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/" in
  let gentoo_dir =
    "/usr/lib/clang/" ^ Clangml_config.version_string ^ "/include/" in
  let centos_dir =
    "/usr/lib64/clang/" ^ Clangml_config.version_string ^ "/include/" in
  [macos_sdk; (*cpp_lib;*) includedir; gentoo_dir; centos_dir]

(*
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
*)

let rec get_typedef_underlying_type ?(recursive = false) (t : cxtype) =
  if get_type_kind t = Typedef then
    let result = get_typedef_decl_underlying_type (get_type_declaration t) in
    if recursive then
      get_typedef_underlying_type ~recursive:true result
    else
      result
  else
    t

let rec get_typedef_underlying_type_loc ?(recursive = false)
    (t : clang_ext_typeloc) =
  if ext_type_loc_get_class t = Typedef then
    let result =
      ext_typedef_decl_get_underlying_type_loc
        (get_type_declaration (ext_type_loc_get_type t)) in
    if recursive then
      get_typedef_underlying_type_loc ~recursive:true result
    else
      result
  else
    t

module Custom (Node : Clang__ast.NodeS) = struct
module Printer = Printer.Make (Node)

module Ast = Ast_converter.Make (Node)

module Expr = [%meta node_module [%str
  type t = Ast.expr [@@deriving refl]

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.expr_of_cxcursor cur

  let get_definition e =
    e |> Ast.cursor_of_node |> get_cursor_definition

  type radix = Decimal | Octal | Hexadecimal | Binary [@@deriving refl]

  let radix_of_string s =
    if s.[0] = '0' then
      if String.length s > 1 then
        match s.[1] with
        | 'x' | 'X' -> Hexadecimal
        | 'b' | 'B' -> Binary
        | _ -> Octal
      else
        Octal
    else
      Decimal

  let radix_of_integer_literal (expr : t) : radix option =
    let cursor = Ast.cursor_of_node expr in
    let start = get_range_start (get_cursor_extent cursor) in
    let tu = cursor_get_translation_unit cursor in
    let tokens = tokenize tu (get_range start start) in
    (* [tokens] should be an array of length 1: however, with Clang <7,
       [tokens] include the token next to the range. *)
    if Array.length tokens >= 1 then
      Some (radix_of_string (get_token_spelling tu tokens.(0)))
    else
      None

  let parse_string ?index ?clang_options ?options ?(filename = "<string>")
      ?(line = 1) ?(context = [])
      (s : string) : t option * Ast.translation_unit =
    let code = Format.asprintf {|
void f(void) {
  %a
#pragma clang diagnostic ignored "-Wunused-value"
#line %d "%s"
%s;
}
      |} Printer.decls context line filename s in
    let ast = Ast.parse_string ?index ?clang_options ?options code in
    let expr =
      match (Node.force ast.desc).items with
      | [{ desc; _}] ->
          begin match Node.force desc with
          | Function { body = Some { desc; _}; _} ->
              begin match Node.force desc with
              | Compound stmts ->
                  begin match List.rev stmts with
                  | { desc; _} :: _ ->
                      begin match Node.force desc with
                      | Expr e -> Some e
                      | _ -> None
                      end
                  | _ -> None
                  end
              | _ -> None
              end
          | _ -> None
          end
      | _ -> None in
    (expr, ast)
]]

module Type_loc = [%meta node_module [%str
  type t = Ast.type_loc [@@deriving refl]

  let to_qual_type ?options (t : t) =
    match t.typeloc with
    | Some tl -> Ast.of_type_loc ?options tl
    | None -> get_cursor_type (get_null_cursor ()) |> Ast.of_cxtype ?options

  let of_typeloc ?(options = Ast.Options.default) typeloc =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.type_loc_of_typeloc typeloc
]]

module Decl = [%meta node_module [%str
  type t = Ast.decl [@@deriving refl]

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.decl_of_cxcursor cur

  let get_typedef_underlying_type ?options ?(recursive = false) decl =
    let result =
      decl |> Ast.cursor_of_node |> ext_typedef_decl_get_underlying_type_loc in
    let result =
      if recursive then
        get_typedef_underlying_type_loc ~recursive:true result
      else
        result in
    Ast.of_type_loc ?options result

  let get_field_bit_width field =
    field |> Ast.cursor_of_node |> get_field_decl_bit_width

  let get_size_expr ?options decl =
    decl |> Ast.cursor_of_node |>
    ext_declarator_decl_get_size_expr |> Expr.of_cxcursor ?options

  let get_type_loc ?options decl =
    decl |> Ast.cursor_of_node |>
    ext_declarator_decl_get_type_loc |> Type_loc.of_typeloc ?options

  let get_canonical decl =
    decl |> Ast.cursor_of_node |> get_canonical_cursor

  type annotated_field = {
      specifier : Ast.cxx_access_specifier;
      decl : Ast.decl;
    }

  let annotate_access_specifier
      (default_specifier : Ast.cxx_access_specifier)
      (fields : Ast.decl list) : annotated_field list =
    let annotate_field (specifier, rev) (decl : Ast.decl) =
      match Node.force decl.desc with
      | AccessSpecifier specifier -> (specifier, rev)
      | _ -> (specifier, { specifier; decl } :: rev) in
    let _specifier, rev =
      List.fold_left annotate_field (default_specifier, []) fields in
    List.rev rev
]]

module Parameter = [%meta node_module [%str
  type t = Ast.parameter [@@deriving refl]

  let get_size_expr ?options param =
    param |> Ast.cursor_of_node |>
    ext_declarator_decl_get_size_expr |> Expr.of_cxcursor ?options

  let get_type_loc ?options param =
    param |> Ast.cursor_of_node |>
    ext_declarator_decl_get_type_loc |> Type_loc.of_typeloc ?options
]]

module Type = [%meta node_module [%str
  type t = Ast.qual_type [@@deriving refl]

  let make ?(const = false) ?(volatile = false) ?(restrict = false) desc : t =
    { cxtype = get_cursor_type (get_null_cursor ()); type_loc = None;
      const; volatile; restrict; desc }

  let of_cxtype = Ast.of_cxtype

  let of_type_loc = Ast.of_type_loc

  let of_cursor ?options cursor =
    get_cursor_type cursor |> of_cxtype ?options

  let of_decoration ?options (decoration : Ast.decoration) =
    match decoration with
    | Cursor cursor -> of_cursor ?options cursor
    | Custom { qual_type; _ } ->
        match qual_type with
        | Some qual_type -> qual_type
        | None -> invalid_arg "of_decoration"

  let of_node ?options (node : 'a Ast.node) =
    of_decoration ?options node.decoration

  let get_size_of (ty : t) = type_get_size_of ty.cxtype

  let get_align_of (ty : t) = type_get_align_of ty.cxtype

  let get_offset_of (ty : t) (field_name : string) =
    let cxtype = get_typedef_underlying_type ~recursive:true ty.cxtype in
    let result = type_get_offset_of cxtype field_name in
    if result = -1 then
      invalid_arg "Clang.Type.get_offset_of"
    else
      result

  let get_typedef_underlying_type ?options ?recursive (qual_type : t) =
    get_typedef_underlying_type ?recursive qual_type.cxtype |>
    of_cxtype ?options

  let get_declaration ?options (qual_type : t) =
    get_type_declaration qual_type.cxtype |> Decl.of_cxcursor ?options

  let iter_fields ?options f (qual_type : t) =
    qual_type.cxtype |> iter_type_fields @@ fun x ->
      f (Decl.of_cxcursor ?options x)

  let list_of_fields ?options (qual_type : t) =
    qual_type.cxtype |> list_of_type_fields |> List.map @@
      Decl.of_cxcursor ?options
]]

module Stmt = [%meta node_module [%str
  type t = Ast.stmt [@@deriving refl]

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.stmt_of_cxcursor cur
]]

module Enum_constant = [%meta node_module [%str
  type t = Ast.enum_constant [@@deriving refl]

  let of_cxcursor ?(options = Ast.Options.default) cur =
    let module Convert = Ast.Converter (struct let options = options end) in
    Convert.enum_constant_of_cxcursor cur

  let get_value enum_constant =
    enum_constant |> Ast.cursor_of_node |> get_enum_constant_decl_value
]]

module Translation_unit = [%meta node_module [%str
  type t = Ast.translation_unit [@@deriving refl]

  let make ?(filename = "") items : Ast.translation_unit_desc =
    { filename; items }
]]
end

module Id = Custom (Clang__ast.IdNode)

module Lazy = Custom (Clang__ast.LazyNode)

include Id
