let psig_value value_description =
  { Parsetree.psig_desc = Psig_value value_description;
    psig_loc = value_description.pval_loc; }

let psig_type ?(rec_flag = Asttypes.Recursive) ?(psig_loc = Location.none)
    type_declarations =
  { Parsetree.psig_desc = Psig_type (rec_flag, type_declarations);
    psig_loc }

let value_description ?(pval_attributes = []) ?(pval_loc = Location.none)
    ?(pval_prim = []) pval_name pval_type =
  { Parsetree.pval_name; pval_type; pval_prim; pval_attributes; pval_loc }

let type_declaration ?(ptype_params = []) ?(ptype_cstrs = [])
    ?(ptype_kind = Parsetree.Ptype_abstract) ?(ptype_private = Asttypes.Public)
    ?ptype_manifest ?(ptype_attributes = []) ?(ptype_loc = Location.none)
    ptype_name =
  { Parsetree.ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
    ptype_manifest; ptype_attributes; ptype_loc }

let constructor_declaration ?(pcd_args = Parsetree.Pcstr_tuple [])
    ?(pcd_res = None) ?(pcd_loc = Location.none) ?(pcd_attributes = [])
    pcd_name =
  { Parsetree.pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }

let label_declaration ?(pld_mutable = Asttypes.Immutable)
    ?(pld_loc = Location.none) ?(pld_attributes = []) pld_name pld_type =
  { Parsetree.pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }

let loc txt =
  { Location.txt; loc = Location.none }

let ptyp_constr ?(ptyp_attributes = []) ?(ptyp_loc = Location.none) ?(args = [])
    ident =
  { Parsetree.ptyp_desc = Ptyp_constr (ident, args);
    ptyp_attributes; ptyp_loc }

let ptyp_arrow ?(ptyp_attributes = []) ?(ptyp_loc = Location.none)
    ?(label = Asttypes.Nolabel) t1 t2 =
  { Parsetree.ptyp_desc = Ptyp_arrow (label, t1, t2);
    ptyp_attributes; ptyp_loc }

let ptyp_tuple ?(ptyp_attributes = []) ?(ptyp_loc = Location.none) list =
  { Parsetree.ptyp_desc = Ptyp_tuple list;
    ptyp_attributes; ptyp_loc }

module String_hashtbl = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module String_map = Map.Make (struct
  type t = string

  let compare = compare
end)

let make_ocaml_type_name s =
  let buffer = Buffer.create 17 in
  String.iter (fun c ->
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' ->
        Buffer.add_char buffer (Char.lowercase_ascii c)
    | _ -> ()) s;
  Buffer.contents buffer

type argument =
  | Index of int
  | Name of string

type argument_interface =
  | Array of { length : argument; contents : argument }
  | Output_on_success of argument
  | Closure of {
      pointer : argument;
      data_caller : argument;
      data_callee : argument;
    }

type type_interface = {
    integer_boolean : bool;
  }

let empty_type_interface =
  { integer_boolean = false }

let integer_boolean =
  { integer_boolean = true }

let union_type_interface a b =
  { integer_boolean = a.integer_boolean || b.integer_boolean }

type function_interface = {
    hidden : bool;
    result : type_interface;
    rename : string -> string;
    arguments : argument_interface list;
  }

let empty_function_interface =
  { hidden = false;
    result = empty_type_interface;
    rename = (fun x -> x);
    arguments = [] }

let hidden_function_interface = { empty_function_interface with hidden = true }

let rename_function rename =
  { empty_function_interface with rename }

let union_function_interfaces a b =
  { hidden = a.hidden || b.hidden;
    result = union_type_interface a.result b.result;
    rename = (fun name -> a.rename (b.rename name));
    arguments = List.rev_append a.arguments b.arguments }

let add_argument argument function_interface =
  { function_interface with
    arguments = argument :: function_interface.arguments }

let add_result type_interface function_interface =
  { function_interface with
    result = union_type_interface function_interface.result type_interface }

type constant_interface = {
    success : bool;
  }

let empty_constant_interface = { success = false }

let union_constant_interfaces a b = { success = a.success || b.success }

type enum_interface = {
    constants : (Pcre.regexp * constant_interface) list;
  }

let empty_enum_interface = { constants = [] }

let union_enum_interfaces a b =
  { constants = List.rev_append a.constants b.constants }

let add_constant constant_name constant_interface enum_interface =
  { constants =
    (constant_name, constant_interface) :: enum_interface.constants }

type field_interface =
  | SizedString of { length : string; contents : string }

type accessor_interface = {
    field_name : string;
    accessor_name : string;
    stub_name : string;
  }

type struct_interface = {
    fields : field_interface list;
    accessors : accessor_interface list;
  }

let empty_struct_interface = { fields = []; accessors = [] }

let union_struct_interfaces a b =
  { fields = List.rev_append a.fields b.fields;
    accessors = List.rev_append a.accessors b.accessors }

let add_field field_interface struct_interface =
  { struct_interface with
    fields = field_interface :: struct_interface.fields }

let add_accessor field_name accessor_name stub_name struct_interface =
  { struct_interface with
    accessors = { field_name; accessor_name; stub_name } ::
      struct_interface.accessors }

type module_interface = {
    functions : (Pcre.regexp * function_interface) list;
    enums : (Pcre.regexp * enum_interface) list;
    structs : (Pcre.regexp * struct_interface) list;
  }

let empty_module_interface = { functions = []; enums = []; structs = [] }

let get_interface empty union list name =
  let add_rule accu (rex, interface) =
    if Pcre.pmatch ~rex name then
      union interface accu
    else
      accu in
  List.fold_left add_rule empty list

let get_function function_name module_interface =
  get_interface empty_function_interface union_function_interfaces
    module_interface.functions function_name

let get_enum enum_name module_interface =
  get_interface empty_enum_interface union_enum_interfaces
    module_interface.enums enum_name

let get_constant constant_name enum_interface =
  get_interface empty_constant_interface union_constant_interfaces
    enum_interface.constants constant_name

let get_struct struct_name module_interface =
  get_interface empty_struct_interface union_struct_interfaces
    module_interface.structs struct_name

let add_function function_name function_interface module_interface =
  { module_interface with functions =
    (function_name, function_interface) :: module_interface.functions }

let add_enum enum_name enum_interface module_interface =
  { module_interface with enums =
    (enum_name, enum_interface) :: module_interface.enums }

let add_struct struct_name struct_interface module_interface =
  { module_interface with structs =
    (struct_name, struct_interface) :: module_interface.structs }

type 'a common_type_info = {
    ocamltype : Parsetree.core_type;
    c_of_ocaml : 'a;
    ocaml_of_c : 'a;
  }

let map_common_type_info f s =
  { ocamltype = s.ocamltype;
    c_of_ocaml = f s.c_of_ocaml;
    ocaml_of_c = f s.ocaml_of_c }

let make_common_type_info ocaml_type_name =
  { ocamltype = ptyp_constr (loc (Longident.Lident ocaml_type_name));
    c_of_ocaml =
    Printf.sprintf "%s_val" (String.capitalize_ascii ocaml_type_name);
    ocaml_of_c = Printf.sprintf "Val_%s" ocaml_type_name }

type enum_info = {
    result : string option;
  }

type struct_info = unit

type regular_info = unit

type type_info =
  | Regular of regular_info
  | Enum of enum_info
  | Struct of struct_info

type translation_context = {
    module_interface : module_interface;
    chan_stubs : out_channel;
    type_table :
      (string option common_type_info * type_info) Lazy.t String_hashtbl.t;
    enum_table :
      (string option common_type_info * enum_info) Lazy.t String_hashtbl.t;
    struct_table :
      (string option common_type_info * struct_info) Lazy.t String_hashtbl.t;
    used_type_table : unit String_hashtbl.t;
    mutable items_accu : Parsetree.signature_item list;
  }

let create_translation_context module_interface chan_stubs =
  let used_type_table = String_hashtbl.of_seq (List.to_seq ["int", ()]) in
  {
    module_interface;
    chan_stubs;
    type_table = String_hashtbl.create 17;
    enum_table = String_hashtbl.create 17;
    struct_table = String_hashtbl.create 17;
    used_type_table;
    items_accu = [];
  }

let make_name_unique used_names name =
  let name =
    if String_hashtbl.mem used_names name then
      let rec add_index index =
        let new_name = Printf.sprintf "%s%d" name index in
        if String_hashtbl.mem used_names new_name then
          add_index (succ index)
        else
          new_name in
      add_index 2
    else
      name in
  String_hashtbl.add used_names name ();
  name

exception Unknown_type

let ocaml_string = ptyp_constr (loc (Longident.Lident "string"))

type elaborated_type =
  | Enum of string
  | Struct of string

let check_prefix prefix string =
  let prefix_length = String.length prefix in
  let string_length = String.length string in
  if string_length >= prefix_length &&
      String.sub string 0 prefix_length = prefix then
    Some (String.sub string prefix_length (string_length - prefix_length))
  else
    None

let get_elaborated_type type_spelling =
  match check_prefix "enum " type_spelling with
  | Some type_name -> Some (Enum type_name)
  | None ->
      match check_prefix "struct " type_spelling with
      | Some type_name -> Some (Struct type_name)
      | None -> None

let bool_info =
  { ocamltype = ptyp_constr (loc (Longident.Lident "bool"));
    c_of_ocaml = Some "Bool_val";
    ocaml_of_c = Some "Val_bool"; }

let find_type_info ?(declare_abstract = true) context type_interface ty =
  match Clang.get_type_kind ty with
  | Void ->
      { ocamltype = ptyp_constr (loc (Longident.Lident "unit"));
        c_of_ocaml = None;
        ocaml_of_c = None; }, Regular ()
  | UInt
  | Int
  | Long
  | LongLong ->
      if type_interface.integer_boolean then
        bool_info, Regular ()
      else
        { ocamltype = ptyp_constr (loc (Longident.Lident "int"));
          c_of_ocaml = Some "Int_val";
          ocaml_of_c = Some "Val_int"; }, Regular ()
  | Bool ->
      bool_info, Regular ()
  | Double ->
      { ocamltype = ptyp_constr (loc (Longident.Lident "float"));
        c_of_ocaml = Some "Double_val";
        ocaml_of_c = Some "caml_copy_double"; }, Regular ()
  | Pointer when Clang.get_type_kind (Clang.get_pointee_type ty) = Char_S ->
      { ocamltype = ocaml_string;
        c_of_ocaml = Some "String_val";
        ocaml_of_c = Some "caml_copy_string"; }, Regular ()
  | Elaborated ->
      let full_type_name = Clang.get_type_spelling ty in
      begin
        match get_elaborated_type full_type_name with
        | None -> failwith full_type_name
        | Some (Enum type_name) ->
            let enum_info =
              try
                String_hashtbl.find context.enum_table type_name
              with Not_found ->
                failwith ("Unknown enum " ^ type_name) in
            let common_info, enum_info = Lazy.force enum_info in
            common_info, Enum enum_info
        | Some (Struct type_name) ->
            let struct_info =
              try
                String_hashtbl.find context.struct_table type_name
              with Not_found ->
                failwith ("Unknown struct " ^ type_name) in
            let common_info, struct_info = Lazy.force struct_info in
            common_info, Struct struct_info
      end
  | _ ->
      let type_name = Clang.get_type_spelling ty in
      match type_name with
      | "CXString" ->
          { ocamltype = ocaml_string;
            c_of_ocaml =  Some "";
            ocaml_of_c = Some "OCAML_OF_CXSTRING"; }, Regular ()
      | _ ->
          match String_hashtbl.find_opt context.type_table type_name with
          | Some type_info -> Lazy.force type_info
          | None ->
              if not declare_abstract then
                raise Unknown_type;
              let ocaml_type_name = make_ocaml_type_name type_name in
              let ocaml_type_name =
                make_name_unique context.used_type_table ocaml_type_name in
              let ocamltype =
                ptyp_constr (loc (Longident.Lident ocaml_type_name)) in
              let type_info =
                { ocamltype; c_of_ocaml = Some ""; ocaml_of_c = Some "" },
                Regular () in
              String_hashtbl.add context.type_table type_name (lazy type_info);
              context.items_accu <-
                psig_type [type_declaration (loc ocaml_type_name)]
                :: context.items_accu;
              type_info

let translate_type_info ?(outputs = []) context (common_info, type_info) =
  match type_info with
  | Regular () -> common_info.ocamltype
  | Enum enum_info ->
      let ocaml_type =
        match enum_info.result with
        | Some _ ->
            let output_tuple =
              match outputs with
              | [] -> ptyp_constr (loc (Longident.Lident "unit"))
              | [ty] -> ty
              | list -> ptyp_tuple list in
            ptyp_constr (loc (Longident.Lident "result"))
              ~args:[output_tuple; common_info.ocamltype]
        | None -> common_info.ocamltype in
      ocaml_type
  | Struct struct_info ->
      common_info.ocamltype

let translate_type ?outputs ?declare_abstract context type_interface ty =
  translate_type_info ?outputs context
    (find_type_info ?declare_abstract context type_interface ty)

type field_type =
  | Unknown of string * Clang.cxtype
  | Translated of string * (string option common_type_info * type_info)
  | SizedString of
      { length : string * Clang.cxtype; contents : string * Clang.cxtype }

let rec list_chop p list =
  match list with
  | [] -> raise Not_found
  | hd :: tl ->
      match p hd with
      | Some hd -> hd, tl
      | None ->
          let item, tl = list_chop p tl in
          item, hd :: tl

let rec list_mutate f list =
  match list with
  | [] -> raise Not_found
  | hd :: tl ->
      match f hd with
      | None -> hd :: list_mutate f tl
      | Some hd -> hd :: tl

let print_list chan list =
  let first = ref true in
  List.iter (fun arg ->
    if !first then
      first := false
    else
      Printf.fprintf chan ", ";
    Printf.fprintf chan "%s" arg) list

let print_ocaml_primitive channel name args print_body =
 Printf.fprintf channel "CAMLprim value\n%s(%a)\n{%t}\n\n"
    name print_list args print_body

let print_return_ocaml_of_c context used_arg_names print_expression
    result_type_interface result_type outputs =
  if Clang.get_type_kind result_type = Void then
    begin
      Printf.fprintf context.chan_stubs "\n  %t;" print_expression;
      Printf.fprintf context.chan_stubs "\n  CAMLreturn(Val_unit);\n"
    end
  else
    begin
      let result = make_name_unique used_arg_names "result" in
      Printf.fprintf context.chan_stubs "\n  %s %s = %t;"
        (Clang.get_type_spelling result_type) result print_expression;
      let common_info, type_info =
        find_type_info context result_type_interface result_type in
      match type_info with
      | Enum { result = Some success } ->
          let make_data channel =
            match outputs with
            | [] -> Printf.fprintf channel "data = Val_unit;"
            | [single, ty] ->
                 let common_info, type_info =
                   find_type_info context empty_type_interface ty in
                 Printf.fprintf channel "data = %s(%s);"
                   (Option.get common_info.ocaml_of_c) single
            | list ->
                 Printf.fprintf channel "data = caml_alloc_tuple(%d);\n"
                   (List.length list);
                 list |> List.iteri @@ fun i (s, ty) ->
                   let common_info, type_info =
                     find_type_info context empty_type_interface ty in
                   Printf.fprintf channel "  Store_field(data, %d, %s(%s));\n"
                     i (Option.get common_info.ocaml_of_c) s in
          Printf.fprintf context.chan_stubs "
  if (%s == %s) {
    CAMLlocal2(result, data);
    result = caml_alloc(1, 0);
    %t
    Store_field(result, 0, data);
    CAMLreturn(result);
  }
  else {
    CAMLlocal1(result);
    result = caml_alloc(1, 1);
    Store_field(result, 0, %s(%s));
    CAMLreturn(result);
  }" result success make_data (Option.get common_info.ocaml_of_c) result
      | _ ->
          let ocaml_of_c = Option.get common_info.ocaml_of_c in
          Printf.fprintf context.chan_stubs "\n  CAMLreturn(%s(%s));\n"
            ocaml_of_c result
    end

let print_return_c_of_ocaml context used_arg_names print_expression
    result_type_interface result_type =
  if Clang.get_type_kind result_type = Void then
    begin
      Printf.fprintf context.chan_stubs "  %t;" print_expression;
    end
  else
    begin
      let result = "result" in
      Printf.fprintf context.chan_stubs "  %s = %t;\n" result print_expression;
      let common_info, type_info =
        find_type_info context result_type_interface result_type in
      let c_of_ocaml = Option.get common_info.c_of_ocaml in
      Printf.fprintf context.chan_stubs "  CAMLreturnT(%s, %s(%s));\n"
        (Clang.get_type_spelling result_type) c_of_ocaml
        result
    end

let translate_struct_decl' context cur typedef name =
  let interface = get_struct name context.module_interface in
  let ocaml_type_name = make_ocaml_type_name name in
  let fields_ref = ref [] in
  ignore (Clang.visit_children cur (fun cur par ->
    begin
      match Clang.get_cursor_kind cur with
      | FieldDecl ->
          let name = Clang.get_cursor_spelling cur in
          let ty = Clang.get_cursor_type cur in
          fields_ref := (name, ty) :: !fields_ref
      | _ -> ()
    end;
    Continue));
  let fields = List.rev !fields_ref in
  let ocaml_fields_ref =
    ref (fields |>List.map (fun (name, ty) -> Unknown (name, ty))) in
  let apply_field_rule (rule : field_interface) =
    match rule with
    | SizedString { length; contents } ->
        let length, fields = !ocaml_fields_ref |> list_chop (fun field ->
          match field with
          | Unknown (name, ty) when name = length -> Some (name, ty)
          | _ -> None) in
        ocaml_fields_ref := fields |> list_mutate (fun field ->
          match field with
          | Unknown (name, ty) when name = contents ->
              Some (SizedString {length; contents = (name, ty)})
          | _ -> None) in
  List.iter apply_field_rule interface.fields;
  let ocaml_fields = !ocaml_fields_ref in
  let common_info = make_common_type_info ocaml_type_name in
  let make_decl () =
    let recognize_type field_type =
      match field_type with
      | Unknown (name, ty) ->
          Translated (
            name,
          find_type_info ~declare_abstract:false context empty_type_interface ty)
      | ty -> ty in
    let record_fields =
      try Some (List.map recognize_type ocaml_fields)
      with Unknown_type -> None in
    let type_name =
      if typedef then name
      else Printf.sprintf "struct %s" name in
    let ptype_kind =
      match record_fields with
      | None ->
          Printf.fprintf context.chan_stubs
            "DECLARE_OPAQUE(%s, %s, %s, %s)\n\n"
            type_name ocaml_type_name common_info.c_of_ocaml
            common_info.ocaml_of_c;
          Parsetree.Ptype_abstract
      | Some fields ->
          let nb_fields = List.length fields in
          Printf.fprintf context.chan_stubs "\
static value
%s(%s v)
{
  CAMLparam0();
  CAMLlocal2(ocaml, string);
  ocaml = caml_alloc_tuple(%d);
" common_info.ocaml_of_c type_name nb_fields;
          fields |> List.iteri (fun i field ->
            match field with
            | Unknown _ -> assert false
            | SizedString { contents = (contents, _); length = (length, _) } ->
                Printf.fprintf context.chan_stubs "\
  string = caml_alloc_string(v.%s);
  memcpy(String_val(string), v.%s, v.%s);
  Store_field(ocaml, %d, string);
" length contents length i
            | Translated (name, (common_type_info, type_info)) ->
                Printf.fprintf context.chan_stubs
                  "  Store_field(ocaml, %d, %s(v.%s));\n"
                  i (Option.get common_type_info.ocaml_of_c) name);
          Printf.fprintf context.chan_stubs "\
  CAMLreturn(ocaml);
}

static %s
%s(value ocaml)
{
  CAMLparam1(ocaml);
  %s v;
" type_name common_info.c_of_ocaml type_name;
          fields |> List.iteri (fun i field ->
            match field with
            | Unknown _ -> assert false
            | SizedString { contents = (contents, _); length = (length, _) } ->
                Printf.fprintf context.chan_stubs "\
  v.%s = caml_string_length(Field(ocaml, %d));
  v.%s = String_val(Field(ocaml, %d));
" length i contents i
            | Translated (name, (common_type_info, type_info)) ->
                Printf.fprintf context.chan_stubs
                  "  v.%s = %s(Field(ocaml, %d));\n"
                  name (Option.get common_type_info.c_of_ocaml) i);
          Printf.fprintf context.chan_stubs "\
  CAMLreturnT(%s, v);
}
" type_name;
          let fields = fields |> List.map (fun field ->
            match field with
            | Unknown _ -> assert false
            | SizedString { contents = (name, _) } ->
                label_declaration (loc (String.lowercase_ascii name))
                  ocaml_string
            | Translated (name, ty) ->
                label_declaration (loc (String.lowercase_ascii name))
                  (translate_type_info context ty)) in
          Ptype_record fields in
    let type_decl = type_declaration (loc ocaml_type_name) ~ptype_kind in
    context.items_accu <- psig_type [type_decl] :: context.items_accu in
  let decl_made = ref false in
  let make_decl () =
    if not !decl_made then
      begin
        decl_made := true;
        make_decl ()
      end in
  let some_common_info = map_common_type_info Option.some common_info in
  if typedef then
    String_hashtbl.add context.type_table name
      (lazy (make_decl (); some_common_info, Struct ()))
  else
    String_hashtbl.add context.struct_table name
      (lazy (make_decl (); some_common_info, ()));
  String_hashtbl.add context.used_type_table ocaml_type_name ();
  let used_arg_names = String_hashtbl.create 17 in
  let print_accessor { field_name; accessor_name; stub_name } =
    make_decl ();
    let field_type = List.assoc field_name fields in
    print_ocaml_primitive context.chan_stubs stub_name ["value arg"]
      (fun channel ->
         let print_expression channel =
           Printf.fprintf channel "%s(arg).%s" common_info.c_of_ocaml
            field_name in
         let print_result channel =
           print_return_ocaml_of_c context used_arg_names print_expression
             empty_type_interface field_type [] in
         Printf.fprintf channel "
  CAMLparam1(arg);
  %t" print_result);
    let pval_prim = [stub_name] in
    let field_type_info, _ =
      find_type_info context empty_type_interface field_type in
    let pval_type = ptyp_arrow common_info.ocamltype
      field_type_info.ocamltype in
    let item =
      psig_value (value_description (loc accessor_name) pval_type ~pval_prim) in
    context.items_accu <- item :: context.items_accu in
  List.iter print_accessor interface.accessors

let translate_struct_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let typedef, name =
    if name = "" then
      true, Clang.get_type_spelling (Clang.get_cursor_type cur)
    else false, name in
  translate_struct_decl' context cur typedef name

let longuest_common_prefix s0 s1 =
  let up_to = min (String.length s0) (String.length s1) in
  let rec check previous i =
    if i < up_to then
      let j = succ i in
      let prefix = String.sub s0 0 j in
      if prefix = String.sub s1 0 j then
        check prefix j
      else
        previous
    else
      previous in
  check "" 0

module Int_hashtbl = Hashtbl.Make (struct
  type t = int

  let equal = ( = )

  let hash = Hashtbl.hash
end)

let translate_enum_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let interface = get_enum name context.module_interface in
  let typedef, name =
    if name = "" then
      true, Clang.get_type_spelling (Clang.get_cursor_type cur)
    else false, name in
  let ocaml_type_name = make_ocaml_type_name name in
  let result = ref None in
  let constructors_ref = ref [] in
  let already_bound = Int_hashtbl.create 17 in
  ignore (Clang.visit_children cur (fun cur par ->
    begin
      match Clang.get_cursor_kind cur with
      | EnumConstantDecl ->
          let value = Clang.get_enum_constant_decl_value cur in
          if not (Int_hashtbl.mem already_bound value) then
            begin
              Int_hashtbl.add already_bound value ();
              let name = Clang.get_cursor_spelling cur in
              let interface = get_constant name interface in
              if interface.success then
                result := Some name
              else
                constructors_ref := name :: !constructors_ref
            end
      | _ -> ()
    end;
    Continue));
  let constructors = List.rev !constructors_ref in
  let longuest_common_prefix =
    match constructors with
    | [] -> ""
    | hd :: tl -> List.fold_left longuest_common_prefix hd tl in
  let ocaml_constructor_names =
    match String.rindex_opt longuest_common_prefix '_' with
    | None -> constructors
    | Some index ->
        List.map (fun name ->
          String.sub name (index + 1) (String.length name - index - 1))
          constructors in
  let ocaml_constructors =
    List.map (fun name ->
      constructor_declaration (loc (String.capitalize_ascii name)))
      ocaml_constructor_names in
  let common_info = make_common_type_info ocaml_type_name in
  let enum_info = { result = !result } in
  let make_decl () =
    let type_name =
      if typedef then name
      else Printf.sprintf "enum %s" name in
    Printf.fprintf context.chan_stubs
      "%s\n%s(value ocaml)\n{\n  switch (Int_val(ocaml)) {\n"
      type_name common_info.c_of_ocaml;
    constructors |> List.iteri (fun i constructor ->
      Printf.fprintf context.chan_stubs
        "  case %i: return %s;\n" i constructor);
    Printf.fprintf context.chan_stubs
      "  default: failwith_fmt(\"invalid value for %s: %%d\", Int_val(ocaml));\n  }\n}\n\n"
      common_info.c_of_ocaml;
    Printf.fprintf context.chan_stubs
      "value\n%s(%s v)\n{\n  switch (v) {\n"
      common_info.ocaml_of_c type_name;
    constructors |> List.iteri (fun i constructor ->
      Printf.fprintf context.chan_stubs
        "  case %s: return Val_int(%i);\n" constructor i);
    Printf.fprintf context.chan_stubs
      "  default: failwith_fmt(\"invalid value for %s: %%d\", v);\n  }\n}\n\n"
      common_info.ocaml_of_c;
    let type_decl =
      type_declaration ~ptype_kind:(Ptype_variant ocaml_constructors)
        (loc ocaml_type_name) in
    context.items_accu <- psig_type [type_decl] :: context.items_accu in
  let some_common_info = map_common_type_info Option.some common_info in
  if typedef then
    String_hashtbl.add context.type_table name
      (lazy (make_decl (); some_common_info, Enum enum_info))
  else
    String_hashtbl.add context.enum_table name
      (lazy (make_decl (); some_common_info, enum_info));
  String_hashtbl.add context.used_type_table ocaml_type_name ()

let translate_typedef_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let underlying_type = Clang.get_typedef_decl_underlying_type cur in
  if
    match Clang.get_type_kind underlying_type,
      get_elaborated_type (Clang.get_type_spelling underlying_type) with
    | Elaborated, Some (Enum type_name) ->
        begin
          match String_hashtbl.find_opt context.enum_table type_name with
          | None -> true
          | Some enum_info ->
              String_hashtbl.add context.type_table name
                (lazy (
                  let common_info, enum_info = Lazy.force enum_info in
                  common_info, Enum enum_info));
              false
        end
    | Elaborated, Some (Struct type_name) ->
        begin
          match String_hashtbl.find_opt context.struct_table type_name with
          | None -> true
          | Some struct_info ->
              String_hashtbl.add context.type_table name
                (lazy (
                  let common_info, struct_info = Lazy.force struct_info in
                  common_info, Struct struct_info));
              false
        end
    | _ ->
        true
  then
    if not (String_hashtbl.mem context.type_table name) then
    begin
      let ocaml_type_name = make_ocaml_type_name name in
      let common_info = make_common_type_info ocaml_type_name in
      let make_decl () =
        Printf.fprintf context.chan_stubs
          "DECLARE_OPAQUE(%s, %s, %s, %s)\n\n"
          name ocaml_type_name common_info.c_of_ocaml
          common_info.ocaml_of_c;
        let type_decl = type_declaration (loc ocaml_type_name) in
        context.items_accu <- psig_type [type_decl] :: context.items_accu in
      let some_common_info = map_common_type_info Option.some common_info in
      String_hashtbl.add context.type_table name
        (lazy (make_decl (); some_common_info, Regular ()));
      String_hashtbl.add context.used_type_table ocaml_type_name ()
    end

type argument_type =
  | Removed
  | Output of Clang.cxtype
  | CXType of Clang.cxtype
  | Array of int * Clang.cxtype * Clang.cxtype
  | Closure of {
      data_caller : int;
      data_caller_type : Clang.cxtype;
      closure_args : (string * Clang.cxtype) array;
      closure_result : Clang.cxtype;
      data_callee : int;
    }

let find_argument argument index_args =
  match argument with
  | Index i -> i
  | Name name ->
      try
        String_hashtbl.find index_args name
      with Not_found ->
        failwith ("HERE: " ^ name)

let translate_argument_type context ty =
  match ty with
  | Removed | Output _ -> assert false
  | CXType ty -> translate_type context empty_type_interface ty
  | Array (_, _, ty) ->
      let args = [
        translate_type context empty_type_interface (Clang.get_pointee_type ty)] in
      ptyp_constr (loc (Longident.Lident "array")) ~args
  | Closure { closure_args; closure_result; data_callee } ->
      let rec build_closure_type (_, arg_type) (i, accu) =
        let j = pred i in
        if j = data_callee then
          j, accu
        else
          j, ptyp_arrow (translate_type context empty_type_interface arg_type) accu in
      snd (Array.fold_right build_closure_type closure_args
        (Array.length closure_args, translate_type context empty_type_interface closure_result))

let index_args args =
  let index = String_hashtbl.create 17 in
  args |> Array.iteri (fun i arg -> String_hashtbl.add index arg i);
  index

let translate_function_decl context cur =
  let name = Clang.get_cursor_spelling cur in
  let function_interface = get_function name context.module_interface in
  if not (function_interface.hidden) then
  let pval_name = loc (function_interface.rename name) in
  let ty = Clang.get_cursor_type cur in
  let num_args = Clang.cursor_get_num_arguments cur in
  let arg_names = Array.init num_args (fun i ->
    Clang.get_cursor_spelling (Clang.cursor_get_argument cur i)) in
  let index = index_args arg_names in
  let args = Array.init num_args (fun i ->
    CXType (Clang.get_arg_type ty i)) in
  let outputs = ref [] in
  let apply_argument_rule (rule : argument_interface) =
    match rule with
    | Array { length; contents } ->
        let length = find_argument length index in
        let contents = find_argument contents index in
        begin
          match args.(length), args.(contents) with
          | CXType length_ty, CXType contents_ty ->
              args.(contents) <-
                Array (length, length_ty, contents_ty)
          | _ -> failwith "Argument expected"
        end;
        args.(length) <- Removed
    | Output_on_success output ->
        let output = find_argument output index in
        begin
          match args.(output) with
          | CXType ty ->
              let ty = Clang.get_pointee_type ty in
              outputs := (output, translate_type context empty_type_interface ty) :: !outputs;
              args.(output) <- Output ty
          | _ -> failwith "Argument expected"
        end
    | Closure { pointer; data_caller; data_callee } ->
        let pointer = find_argument pointer index in
        let data_caller = find_argument data_caller index in
        begin
          match args.(pointer), args.(data_caller) with
          | CXType pointer_ty, CXType data_caller_type ->
              let pointer_ty = Clang.get_typedef_decl_underlying_type (Clang.get_type_declaration pointer_ty) in
              let pointee_ty = Clang.get_pointee_type pointer_ty in
              let num_args = Clang.get_num_arg_types pointee_ty in
              let closure_args = Array.init num_args (fun i ->
                let arg_ty = Clang.get_arg_type pointee_ty i in
                (Printf.sprintf "arg%d" i, arg_ty)) in
              let closure_result = Clang.get_result_type pointee_ty in
              let index = index_args (Array.map fst closure_args) in
              let data_callee = find_argument data_callee index in
              args.(pointer) <-
                Closure {
                  data_caller; data_caller_type; closure_args; closure_result;
                data_callee }
          | _ -> failwith "Argument expected"
        end;
        args.(data_caller) <- Removed in
  List.iter apply_argument_rule function_interface.arguments;
  let outputs = List.rev !outputs in
  let result_type = Clang.get_result_type ty in
  let pval_type =
    if num_args = 0 then
      let result_ty = translate_type context function_interface.result result_type in
      ptyp_arrow (ptyp_constr (loc (Longident.Lident "unit"))) result_ty
    else
      let add_arg arg pval_type =
        match arg with
        | Removed | Output _ -> pval_type
        | _ ->
            ptyp_arrow (translate_argument_type context arg) pval_type in
      let result_ty = translate_type ~outputs:(List.map snd outputs) context
          function_interface.result (Clang.get_result_type ty) in
      Array.fold_right add_arg args result_ty in
  let wrapper_name = name ^ "_wrapper" in
  let used_arg_names = String_hashtbl.create 17 in
  let wrapper_arg_names = Array.map (fun arg_name ->
    let arg_name =
      if arg_name = "" then "arg"
      else arg_name in
    make_name_unique used_arg_names arg_name) arg_names in
  let ocaml_args = Array.map2 (fun arg arg_name ->
      match arg with
      | Removed | Output _ -> None
      | _ -> Some (Printf.sprintf "%s_ocaml" arg_name))
      args wrapper_arg_names |>
    Array.to_seq |> Seq.filter_map (fun x -> x) |> List.of_seq in
  let nb_args = List.length ocaml_args in
  let rec make_buckets args =
    match args with
    | a0 :: a1 :: a2 :: a3 :: a4 :: ((_ :: _) as tl) ->
        [a0; a1; a2; a3; a4] :: make_buckets tl
    | _ -> [args] in
  let ocaml_args_buckets = make_buckets ocaml_args in
  args |> Array.iteri (fun i arg ->
    match arg with
    | Closure {
        data_caller; data_caller_type; closure_args; closure_result;
        data_callee } ->
          let callback_name =
            Printf.sprintf "%s_%s_callback" name wrapper_arg_names.(i) in
          let closure_result_string = Clang.get_type_spelling closure_result in
          let args = closure_args |>
           Array.mapi (fun i (name, ty) -> if i = data_callee then None else Some (name, ty, name ^ "_ocaml")) in
          let ocaml_args = args
           |> Array.to_seq |> Seq.filter_map (Option.map (fun (_, _, name) -> name)) |> List.of_seq in
          let local = "result" :: "f" :: ocaml_args in
          let init_args chan =
            args |> Array.iter @@ Option.iter @@ fun (name, ty, name_ocaml) ->
              let arg_info, _ = find_type_info context empty_type_interface ty in
              flush chan;
              Printf.fprintf chan "  %s = %s(%s);\n"
                 name_ocaml (Option.get arg_info.ocaml_of_c) name in
          let print_expression channel =
            Printf.fprintf channel "caml_callback%s(%a)"
              (match List.length ocaml_args with 1 -> "" | n -> string_of_int n)
              print_list ("f" :: ocaml_args) in
          let print_call_and_return channel =
            print_return_c_of_ocaml context used_arg_names print_expression
              empty_type_interface result_type in
          Printf.fprintf context.chan_stubs "\
%s
%s(%a)
{
  CAMLparam0();
  CAMLlocal%d(%a);
  f = *((value *) %s);
%t%t
}\n\n"
            closure_result_string
            callback_name print_list
            (closure_args |> Array.to_list |> List.map (fun (name, ty) ->
              Printf.sprintf "%s %s" (Clang.get_type_spelling ty) name))
            (List.length local) print_list local
            (fst closure_args.(data_callee))
            init_args print_call_and_return;
          wrapper_arg_names.(data_caller) <-
            Printf.sprintf "&%s_ocaml" wrapper_arg_names.(i);
          wrapper_arg_names.(i) <- callback_name
    | _ -> ()
  );
  let print_body chan =
    begin
      match ocaml_args_buckets with
      | [] -> assert false
      | hd :: tl ->
          Printf.fprintf context.chan_stubs "\n  CAMLparam%d(%a);"
            (List.length hd) print_list hd;
          tl |> List.iter @@ fun args ->
            Printf.fprintf context.chan_stubs "\n  CAMLxparam%d(%a);"
              (List.length args) print_list args
    end;
    (*
    Printf.fprintf context.chan_stubs "\n    fprintf(stderr, \"%s\\n\");\n" name;
     *)
    let index = lazy (
      let i = make_name_unique used_arg_names "i" in
      Printf.fprintf context.chan_stubs "\n  size_t %s;" i;
      i) in
    args |> Array.iteri (fun i arg ->
      match arg with
      | Removed -> ()
      | Output ty ->
          Printf.fprintf context.chan_stubs "\n  %s %s;"
            (Clang.get_type_spelling ty) wrapper_arg_names.(i)
      | CXType ty ->
          let common_info, type_info = find_type_info context empty_type_interface ty in
          let c_of_ocaml = Option.get common_info.c_of_ocaml in
          Printf.fprintf context.chan_stubs "\n  %s %s = %s(%s_ocaml);"
            (Clang.get_type_spelling ty) wrapper_arg_names.(i) c_of_ocaml
            wrapper_arg_names.(i)
      | Array (j, length_ty, contents_ty) ->
          Printf.fprintf context.chan_stubs "\n  %s %s = Wosize_val(%s_ocaml);"
            (Clang.get_type_spelling length_ty) wrapper_arg_names.(j)
            wrapper_arg_names.(i);
          let cell_type = Clang.get_pointee_type contents_ty in
          Printf.fprintf context.chan_stubs "\n  %s %s = xmalloc(%s * sizeof(%s));"
            (Pcre.replace ~pat:"const" ~templ:"" (Clang.get_type_spelling contents_ty)) wrapper_arg_names.(i)
            wrapper_arg_names.(j)
            (Clang.get_type_spelling cell_type);
          let index = Lazy.force index in
          let common_info, type_info = find_type_info context empty_type_interface cell_type in
          let c_of_ocaml = Option.get common_info.c_of_ocaml in
          Printf.fprintf context.chan_stubs "\n  for (%s = 0; %s < %s; %s++) {\n    %s[%s] = %s(Field(%s_ocaml, %s));\n  }"
            index index wrapper_arg_names.(j) index wrapper_arg_names.(i) index c_of_ocaml wrapper_arg_names.(i) index
      | Closure _ -> ());
    let wrapper_args = Array.map2 (fun arg arg_name ->
      match arg with
      | Output _ -> "&" ^ arg_name
      | _ -> arg_name) args wrapper_arg_names in
    let print_expression channel =
      Printf.fprintf channel "%s(%a)" name print_list
        (Array.to_list wrapper_args) in
    print_return_ocaml_of_c context used_arg_names print_expression
      empty_type_interface result_type
      (outputs |> List.map @@ fun (i, _) -> wrapper_arg_names.(i), match args.(i) with Output ty -> ty | _ -> assert false) in
  let ocaml_args_decl =
    ocaml_args |> List.map (fun s -> Printf.sprintf "value %s" s) in
  print_ocaml_primitive context.chan_stubs wrapper_name ocaml_args_decl
    print_body;
  let pval_prim =
    if nb_args <= 5 then
      [wrapper_name]
    else
      let bytecode_name = name ^ "_bytecode" in
      print_ocaml_primitive context.chan_stubs bytecode_name
        ["value *argv"; "int argn"]
        (fun channel ->
          Printf.fprintf channel "\n  return %s(%a);" wrapper_name
            print_list (List.init nb_args
              (fun i -> Printf.sprintf "argv[%d]" i)));
      [bytecode_name; wrapper_name] in
  let item =
    psig_value (value_description pval_name pval_type ~pval_prim) in
  context.items_accu <- item :: context.items_accu

let rename_clang name =
  let result = Buffer.create 17 in
  let previous_lowercase = ref false in
  let add_char c =
    match c with
    | 'A' .. 'Z' ->
        if !previous_lowercase then
          begin
            previous_lowercase := false;
            Buffer.add_char result '_'
          end;
        Buffer.add_char result (Char.lowercase_ascii c)
    | '_' ->
        previous_lowercase := false;
        Buffer.add_char result '_'
    | _ ->
        previous_lowercase := true;
        Buffer.add_char result c in
  String.iter add_char (String.sub name 6 (String.length name - 6));
  Buffer.contents result

let run_llvm_config llvm_config arguments =
  let command = String.concat " " (llvm_config :: arguments) in
  let output = Unix.open_process_in command in
  let result = input_line output in
  if Unix.close_process_in output <> Unix.WEXITED 0 then
    failwith (Printf.sprintf "%s: execution failed" command);
  result

let main cflags llvm_config prefix =
  let llvm_cflags = run_llvm_config llvm_config ["--cflags"] in
  let cflags = cflags |> List.map @@ String.split_on_char ',' |> List.flatten in
  let clang_options = cflags @ String.split_on_char ' ' llvm_cflags in
  let module_interface =
    empty_module_interface |>
    add_function (Pcre.regexp "^(?!clang_)|clang_getCString|clang_disposeString|clang_disposeStringSet|clang_VirtualFileOverlay_writeToBuffer|clang_free|constructUSR|clang_executeOnThread") hidden_function_interface |>
    add_function (Pcre.regexp "^clang_") (rename_function rename_clang) |>
    add_function (Pcre.regexp "^clang_createTranslationUnitFromSourceFile$")
      (empty_function_interface |>
        add_argument (Array {
          length = Name "num_clang_command_line_args";
          contents = Name "clang_command_line_args" })) |>
    add_function (Pcre.regexp "^clang_parseTranslationUnit")
      (empty_function_interface |>
        add_argument (Array {
          length = Name "num_command_line_args";
          contents = Name "command_line_args" })) |>
    add_function (Pcre.regexp "^(clang_(re)?parseTranslationUnit|\
                    clang_createTranslationUnitFromSourceFile$|\
                    clang_codeCompleteAt$|clang_indexSourceFile)")
      (empty_function_interface |>
        add_argument (Array {
          length = Name "num_unsaved_files";
          contents = Name "unsaved_files" })) |>
    add_function (Pcre.regexp "^clang_parseTranslationUnit2")
      (empty_function_interface |>
        add_argument (Output_on_success (Name "out_TU"))) |>
    add_function (Pcre.regexp "^clang_visitChildren$")
      (empty_function_interface |>
        add_result integer_boolean |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 2 })) |>
    add_function (Pcre.regexp "^clang_getInclusions$")
      (empty_function_interface |>
        add_result integer_boolean |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 3 })) |>
    add_function (Pcre.regexp "^clang_Type_visitFields$")
      (empty_function_interface |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 1 })) |>
    add_function (Pcre.regexp "^clang*_(is|equal)[A-Z]")
      (empty_function_interface |> add_result integer_boolean) |>
    add_enum (Pcre.regexp "^CXErrorCode$")
      (empty_enum_interface |>
        add_constant (Pcre.regexp "^CXError_Success$") { success = true }) |>
    add_struct (Pcre.regexp "^CXUnsavedFile$")
      (empty_struct_interface |>
        add_field (SizedString {length = "Length"; contents = "Contents"})) |>
    add_struct (Pcre.regexp "^CXType$")
      (empty_struct_interface |>
        add_accessor "kind" "get_type_kind" "clang_getTypeKind_wrapper") in
  let idx = Clang.create_index 1 1 in
  let tu =
    match
      Clang.parse_translation_unit2 idx
        "source.c"
        (Array.of_list clang_options)
        [| { filename = "source.c"; contents = "\
#include <clang-c/Index.h>
#include \"clangml/libclang_extensions.h\"" } |]
        0 with
    | Error _ -> failwith "Error!"
    | Ok tu -> tu in
  let cur = Clang.get_translation_unit_cursor tu in
  let chan_stubs = open_out (prefix ^ "clang_stubs.c") in
  protect ~finally:(fun () -> close_out chan_stubs) (fun () ->
    output_string chan_stubs "\
#include \"stubgen.h\"
#include <clang-c/Index.h>
#include \"libclang_extensions.h\"
#include <stdio.h>
";
    let context =
      create_translation_context module_interface chan_stubs in
    ignore (Clang.visit_children cur (fun cur par ->
      begin
        match Clang.get_cursor_kind cur with
        | StructDecl ->
            translate_struct_decl context cur
        | EnumDecl ->
            translate_enum_decl context cur
        | FunctionDecl ->
            translate_function_decl context cur
        | TypedefDecl ->
            translate_typedef_decl context cur
        | FieldDecl
        | EnumConstantDecl -> assert false
        | _ -> ()
      end;
      Continue));
    let chan_intf = open_out (prefix ^ "clang__bindings.mli") in
    protect ~finally:(fun () -> close_out chan_intf) (fun () ->
      Format.fprintf (Format.formatter_of_out_channel chan_intf)
        "%a@." Pprintast.signature (List.rev context.items_accu));
    let chan_intf = open_out (prefix ^ "clang__bindings.ml") in
    protect ~finally:(fun () -> close_out chan_intf) (fun () ->
      Format.fprintf (Format.formatter_of_out_channel chan_intf)
        "%a@." Pprintast.signature (List.rev context.items_accu)))

let option_cflags =
  let doc = "Pass option to the C compiler" in
  Cmdliner.Arg.(
    value & opt_all string [] & info ["cc"] ~docv:"FLAGS" ~doc)

let option_llvm_config =
  let doc = "Path to llvm-config" in
  Cmdliner.Arg.(
    required & pos 0 (some non_dir_file) None & info [] ~docv:"LLVM_CONFIG" ~doc)

let option_prefix =
  let doc = "Prefix path for output files" in
  Cmdliner.Arg.(
    required & pos 1 (some string) None & info [] ~docv:"LLVM_CONFIG" ~doc)

let options = Cmdliner.Term.(
    const main $ option_cflags $ option_llvm_config $ option_prefix)

let info =
  let doc = "generate stubs for ClangML" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "stubgen" ~doc ~exits:Cmdliner.Term.default_exits ~man

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (options, info))
