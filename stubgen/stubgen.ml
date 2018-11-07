module String_hashtbl = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module String_map = Map.Make (struct
  type t = string

  let compare = compare
end)

let output_subst f channel template =
  let buffer = Buffer.create (String.length template) in
  Buffer.add_substitute buffer f template;
  Buffer.output_buffer channel buffer

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

type type_spec =
  | Int
  | Enum of string
  | Array_struct of { contents : string; length : string }
  | Sized_string of { can_be_null : bool; length : argument }

type type_interface = {
    reinterpret_as : type_spec option;
    destructor : (string -> string) option;
  }

let empty_type_interface =
  { reinterpret_as = None; destructor = None }

let union_option a b =
  match a, b with
  | None, other
  | other, None -> other
  | Some a, Some b ->
      assert (a = b);
      Some a

let reinterpret_as type_spec type_interface =
  { type_interface with reinterpret_as = union_option type_interface.reinterpret_as (Some type_spec) }

let destructor destructor type_interface =
  { type_interface with destructor = union_option type_interface.destructor (Some destructor) }

let integer_enum enum = reinterpret_as (Enum enum)

let integer_boolean = integer_enum "bool"

let integer_zero_is_true = integer_enum "not bool"

let union_type_interface a b =
  { reinterpret_as =
      union_option a.reinterpret_as b.reinterpret_as;
    destructor =
      union_option a.destructor b.destructor }

type argument_interface =
  | Array of { length : argument; contents : argument }
  | Sized_string of { length : argument; contents : argument }
  | Output of { argument : argument; on_success : bool; on_error : bool }
  | Update of argument
  | Fixed_value of { argument : argument; value : string }
  | Type_interface of { argument : argument; interface : type_interface }
  | Closure of {
      pointer : argument;
      data_caller : argument;
      data_callee : argument;
    }

let output argument =
  Output { argument; on_success = true; on_error = true }

let output_on_success argument =
  Output { argument; on_success = true; on_error = false }

let output_on_error argument =
  Output { argument; on_success = false; on_error = true }

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
  | Sized_string of { length : string; contents : string }

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

type converter =
    out_channel -> src:string -> string array -> tgt:string -> unit

type common_type_info = {
    ocamltype : Parsetree.core_type;
    c_of_ocaml : converter;
    ocaml_of_c : converter;
  }

let simple_converter name fmt ~src _params ~tgt =
  Printf.fprintf fmt "%s = %s(%s);" tgt name src

let name_of_c_of_ocaml converter =
  Printf.sprintf "%s_val" (String.capitalize_ascii converter)

let name_of_ocaml_of_c converter =
  Printf.sprintf "Val_%s" converter

let make_common_type_info ?converter ocaml_type_name =
  let converter =
    match converter with
    | None -> ocaml_type_name
    | Some converter -> converter in
  { ocamltype = ptyp_constr (loc (Longident.Lident ocaml_type_name));
    c_of_ocaml =
      simple_converter (name_of_c_of_ocaml converter);
    ocaml_of_c = simple_converter (name_of_ocaml_of_c converter); }

type enum_info = {
    result : string option;
  }

type struct_info = unit

type type_info =
  | Void
  | Regular
  | Bool
  | Not_bool
  | Enum of enum_info
  | Struct of struct_info

type translation_context = {
    module_interface : module_interface;
    chan_stubs : out_channel;
    type_table : (common_type_info * type_info) Lazy.t String_hashtbl.t;
    enum_table : (common_type_info * enum_info) Lazy.t String_hashtbl.t;
    struct_table : (common_type_info * struct_info) Lazy.t String_hashtbl.t;
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

let int_info = make_common_type_info "int"

let bool_info = make_common_type_info "bool"

let not_bool_info = make_common_type_info "bool" ~converter:"not_bool"

let ocaml_array ty =
  ptyp_constr (loc (Longident.Lident "array")) ~args:[ty]

let ocaml_option ty =
  ptyp_constr (loc (Longident.Lident "option")) ~args:[ty]

let string_type_info =
  { ocamltype = ocaml_string;
    c_of_ocaml = simple_converter "String_val";
    ocaml_of_c = simple_converter "caml_copy_string"; }, Regular

let rec find_type_info ?(declare_abstract = true) ?parameters context type_interface ty =
  let find_enum_info type_name =
    let enum_info =
      try
        String_hashtbl.find context.enum_table type_name
      with Not_found ->
        failwith ("Unknown enum " ^ type_name) in
    let common_info, enum_info = Lazy.force enum_info in
    common_info, (Enum enum_info : type_info) in
  let default_type type_name =
    match type_interface.reinterpret_as with
    | Some Int ->
        { ocamltype = int_info.ocamltype;
          c_of_ocaml = (fun fmt ~src _params ~tgt ->
            Printf.fprintf fmt "%s = (time_t) Int_val(%s);" tgt src);
          ocaml_of_c = (fun fmt ~src _params ~tgt ->
            Printf.fprintf fmt "%s = Val_int((time_t) %s);" tgt src); }, Regular
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
          { ocamltype; c_of_ocaml = simple_converter "";
            ocaml_of_c = simple_converter "" },
          Regular in
        String_hashtbl.add context.type_table type_name (lazy type_info);
        context.items_accu <-
          psig_type [type_declaration (loc ocaml_type_name)]
          :: context.items_accu;
        type_info in
  match Clang.get_type_kind ty with
  | Void ->
      { ocamltype = ptyp_constr (loc (Longident.Lident "unit"));
        c_of_ocaml = (fun _ -> assert false);
        ocaml_of_c = (fun _ -> assert false); }, Void
  | UInt
  | Int
  | Long
  | ULong
  | LongLong
  | ULongLong ->
      begin
        match type_interface.reinterpret_as with
        | Some (Enum "bool") -> bool_info, Bool
        | Some (Enum "not bool") -> not_bool_info, Not_bool
        | Some (Enum enum) -> find_enum_info enum
        | None -> int_info, Regular
        | Some _ -> assert false
      end
  | Bool ->
      bool_info, Bool
  | Double ->
      { ocamltype = ptyp_constr (loc (Longident.Lident "float"));
        c_of_ocaml = simple_converter "Double_val";
        ocaml_of_c = simple_converter "caml_copy_double"; }, Regular
  | Pointer when Clang.get_type_kind (Clang.get_pointee_type ty) = Char_S ->
      begin
        match type_interface.reinterpret_as with
        | Some (Sized_string { can_be_null; length }) ->
            begin
              match parameters with
              | None -> assert false
              | Some parameters ->
                  parameters := [| length |]
            end;
            let ocamltype, template_c_of_ocaml, template_ocaml_of_c =
              if can_be_null then
                ocaml_option ocaml_string, "  \
  if (Is_long($src)) {
    $tgt = NULL;
  }
  else {
    CAMLlocal1(field);
    field = Field($src, 0);
    $length = caml_string_length(field);
    $tgt = String_val(field);
  };
", "  \
  if ($src == NULL) {
    $tgt = Val_int(0);
  }
  else {
    $tgt = caml_alloc(1, 0);
    Store_field($tgt, 0, caml_alloc_initialized_string($length, $src));
    $destructor
  };
"
            else
                ocaml_string, "  \
  $length = caml_string_length($src);
  $tgt = String_val($src);
", "  \
  $tgt = caml_alloc_initialized_string($length, $src));
  $destructor
" in
          let subst ~src ~tgt ~length var =
            match var with
            | "src" -> src
            | "tgt" -> tgt
            | "length" -> length
            | "destructor" ->
                begin
                  match type_interface.destructor with
                  | None -> ""
                  | Some destructor -> destructor src
                end
            | _ -> assert false in
          let converter template channel ~src params ~tgt =
            let length = params.(0) in
            output_subst (subst ~src ~tgt ~length) channel template in
          { ocamltype;
            c_of_ocaml = converter template_c_of_ocaml;
            ocaml_of_c = converter template_ocaml_of_c; }, Regular
        | None ->
            string_type_info
        | _ -> assert false
      end
  | Pointer ->
      begin
        match type_interface.reinterpret_as with
        | Some (Array_struct { length; contents }) ->
            let pointee = Clang.get_canonical_type (Clang.get_pointee_type ty) in
            let field_types = pointee |> Clang.list_of_type_fields |> List.map @@ fun cur ->
              Clang.get_cursor_spelling cur, Clang.get_cursor_type cur in
            let find_field name =
              try List.assoc name field_types
              with Not_found -> failwith (Printf.sprintf "Unknown field: %s" name) in
            let length_ty = find_field length in
            let contents_ty = find_field contents in
            let contents_type_info, _ = find_type_info ~declare_abstract context empty_type_interface (Clang.get_pointee_type contents_ty) in
            { ocamltype = ocaml_array contents_type_info.ocamltype;
              c_of_ocaml = simple_converter "";
              ocaml_of_c = (fun channel ~src params ~tgt ->
                Printf.fprintf channel "
%s = caml_alloc(%s->%s, 0);
for (%s i = 0; i < %s->%s; i++) {
  CAMLlocal1(field);
  %t
  Store_field(%s, i, field);
}
" tgt src length (Clang.get_type_spelling length_ty) src length
(fun channel -> contents_type_info.ocaml_of_c channel ~src:(Printf.sprintf "%s->%s[i]" src contents) [| |] ~tgt:"field")
tgt); }, Regular
        | None -> default_type (Clang.get_type_spelling ty)
        | _ -> assert false
      end
  | Elaborated ->
      let full_type_name = Clang.get_type_spelling ty in
      begin
        match get_elaborated_type full_type_name with
        | None -> failwith full_type_name
        | Some (Enum type_name) -> find_enum_info type_name
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
            c_of_ocaml = (fun _ -> assert false);
            ocaml_of_c = (fun fmt ~src _params ~tgt ->
  Printf.fprintf fmt "%s = caml_copy_string(clang_getCString(%s));
clang_disposeString(%s);" tgt src src) }, Regular
      | _ ->
          default_type type_name

let make_tuple list =
  match list with
  | [] -> ptyp_constr (loc (Longident.Lident "unit"))
  | [ty] -> ty
  | _ -> ptyp_tuple list

type 'a output = {
    desc : 'a;
    on_success : bool;
    on_error : bool;
  }

let desc_on_success list =
  list |> List.filter (fun o -> o.on_success) |> List.map (fun o -> o.desc)

let desc_on_error list =
  list |> List.filter (fun o -> o.on_error) |> List.map (fun o -> o.desc)

let translate_type_info ?(outputs = []) (common_info, type_info) =
  match type_info with
  | Void -> make_tuple (desc_on_success outputs)
  | Regular -> make_tuple (common_info.ocamltype :: (desc_on_success outputs))
  | Bool | Not_bool ->
      if outputs = [] then
        bool_info.ocamltype
      else
        ocaml_option (make_tuple (desc_on_success outputs))
  | Enum enum_info ->
      let ocaml_type =
        match enum_info.result with
        | Some _ ->
            ptyp_constr (loc (Longident.Lident "result"))
              ~args:[make_tuple (desc_on_success outputs); make_tuple (common_info.ocamltype :: desc_on_error outputs)]
        | None -> common_info.ocamltype in
      ocaml_type
  | Struct struct_info ->
      make_tuple (common_info.ocamltype :: desc_on_success outputs)

let translate_type ?outputs ?declare_abstract ?parameters context type_interface ty =
  translate_type_info ?outputs
    (find_type_info ?declare_abstract ?parameters context type_interface ty)

type field_type =
  | Unknown of string * Clang.cxtype
  | Translated of string * Clang.cxtype * (common_type_info * type_info)
  | Sized_string of
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

type output_desc =
  | Info of common_type_info * type_info * string array
  | Regular of Clang.cxtype
  | Sized_string of string
  | Array of string * Clang.cxtype * Clang.cxtype

let print_return_ocaml_of_c context used_arg_names print_expression
    result_type result_type_interface ?result_name (common_info, type_info) params outputs =
  let print_output channel src output type_interface tgt =
    match output with
    | Info (common_info, type_info, params) ->
        common_info.ocaml_of_c channel ~src params ~tgt
    | Regular ty ->
        let common_info, type_info =
          find_type_info context type_interface ty in
        common_info.ocaml_of_c channel ~src [| |] ~tgt
    | Sized_string length ->
        Printf.fprintf channel "%s = caml_alloc_initialized_string(%s, %s);\n"
          tgt length src
    | Array (length, length_ty, cell_ty) ->
        let common_info, type_info =
          find_type_info context empty_type_interface cell_ty in
        Printf.fprintf channel
          "%s = caml_alloc(%s, 0);
for (%s i = 0; i < %s; i++) {
  CAMLlocal1(cell);
  %t
  Store_field(%s, i, cell);
}
" tgt length (Clang.get_type_spelling length_ty) length
(fun channel -> common_info.ocaml_of_c channel ~src:(Printf.sprintf "%s[i]" src) [| |] ~tgt:"cell")
tgt
 in
  let make_data outputs channel =
    begin
      match outputs with
      | [] -> Printf.fprintf channel "data = Val_unit;"
      | [single, output, type_interface] ->
          print_output channel single output type_interface "data"
      | list ->
          Printf.fprintf channel "data = caml_alloc_tuple(%d);\n"
            (List.length list);
          list |> List.iteri @@ fun i (s, output, type_interface) ->
            Printf.fprintf channel "  {
    CAMLlocal1(field);
    %t
    Store_field(data, %d, field);
  }\n"
              (fun channel -> print_output channel s output type_interface "field")
              i
    end;
    outputs |> List.iter @@ fun (s, output, type_interface) ->
      match type_interface.destructor with
      | None -> ()
      | Some destructor -> output_string channel (destructor s) in
  if Clang.get_type_kind result_type = Void then
    if outputs = [] then
      begin
        Printf.fprintf context.chan_stubs "\n  %t;" print_expression;
        Printf.fprintf context.chan_stubs "\n  CAMLreturn(Val_unit);\n"
      end
    else
      begin
        Printf.fprintf context.chan_stubs "
  %t;
  {
    CAMLlocal1(data);
    %t
    CAMLreturn(data);
  }
" print_expression (make_data (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i))))
      end
  else
    begin
      let result = make_name_unique used_arg_names "result" in
      Printf.fprintf context.chan_stubs "\n  %s %s = %t;"
        (Clang.get_type_spelling result_type) result print_expression;
      match type_info with
      | Bool | Not_bool ->
          let prefix =
            if type_info = Bool then ""
            else "!" in
          Printf.fprintf context.chan_stubs "
  if (%s%s) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    %t
    Store_field(ocaml_result, 0, data);
    %s
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLreturn(Val_int(0));
  }" prefix result (make_data (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i))))
          (match result_type_interface.destructor with None -> ""
               | Some destructor -> destructor result)
      | Enum { result = Some success } ->
          let real_result = match result_name with Some result -> result | None -> result in
          Printf.fprintf context.chan_stubs "
  if (%s == %s) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    %t
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    %t
    Store_field(ocaml_result, 0, data);
    %s
    CAMLreturn(ocaml_result);
  }" real_result success (make_data (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i)))) (make_data ((real_result, Info (common_info, type_info, params), result_type_interface) :: (outputs |> List.filter (fun (_, o, _) -> o.on_error) |> List.map (fun (s, o, i) -> (s, o.desc, i)))))
          (match result_type_interface.destructor with None -> ""
               | Some destructor -> destructor result)
      | _ ->
          Printf.fprintf context.chan_stubs "
  {
    CAMLlocal1(data);
    %t
    CAMLreturn(data);
  }\n"
(make_data ((result, Info (common_info, type_info, params), result_type_interface) :: (outputs |> List.filter (fun (_, o, _) -> o.on_success) |> List.map (fun (s, o, i) -> (s, o.desc, i)))))
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
      Printf.fprintf context.chan_stubs "  \
  {
    CAMLlocal1(data);
    %t
    CAMLreturnT(%s, data);
  }
"
        (fun channel -> common_info.c_of_ocaml channel result [| |] "data")
        (Clang.get_type_spelling result_type)
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
    | Sized_string { length; contents } ->
        let length, fields = !ocaml_fields_ref |> list_chop (fun field ->
          match field with
          | Unknown (name, ty) when name = length -> Some (name, ty)
          | _ -> None) in
        ocaml_fields_ref := fields |> list_mutate (fun field ->
          match field with
          | Unknown (name, ty) when name = contents ->
              Some (Sized_string {length; contents = (name, ty)})
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
          ty,
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
            type_name ocaml_type_name (name_of_c_of_ocaml ocaml_type_name)
            (name_of_ocaml_of_c ocaml_type_name);
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
" (name_of_ocaml_of_c ocaml_type_name) type_name nb_fields;
          fields |> List.iteri (fun i field ->
            match field with
            | Unknown _ -> assert false
            | Sized_string { contents = (contents, _); length = (length, _) } ->
                Printf.fprintf context.chan_stubs "\
  string = caml_alloc_string(v.%s);
  memcpy(String_val(string), v.%s, v.%s);
  Store_field(ocaml, %d, string);
" length contents length i
            | Translated (name, _ty, (common_type_info, type_info)) ->
                Printf.fprintf context.chan_stubs
                  "  \
  {
     CAMLlocal1(data);
     %t
     Store_field(ocaml, %d, data);
  }
"
                  (fun channel -> common_type_info.ocaml_of_c channel (Printf.sprintf "v.%s" name) [| |] "data") i);
          Printf.fprintf context.chan_stubs "\
  CAMLreturn(ocaml);
}

static %s
%s(value ocaml)
{
  CAMLparam1(ocaml);
  %s v;
" type_name (name_of_c_of_ocaml ocaml_type_name) type_name;
          fields |> List.iteri (fun i field ->
            match field with
            | Unknown _ -> assert false
            | Sized_string { contents = (contents, _); length = (length, _) } ->
                Printf.fprintf context.chan_stubs "\
  v.%s = caml_string_length(Field(ocaml, %d));
  v.%s = String_val(Field(ocaml, %d));
" length i contents i
            | Translated (name, ty, (common_type_info, type_info)) ->
                Printf.fprintf context.chan_stubs
                  "{
                      %s data;
                      %t
                      v.%s = data;
                              }
"
                  (Clang.get_type_spelling ty)
                  (fun channel -> common_type_info.c_of_ocaml channel (Printf.sprintf "Field(ocaml, %d)" i) [| |] "data") name);
          Printf.fprintf context.chan_stubs "\
  CAMLreturnT(%s, v);
}
" type_name;
          let fields = fields |> List.map (fun field ->
            match field with
            | Unknown _ -> assert false
            | Sized_string { contents = (name, _) } ->
                label_declaration (loc (String.lowercase_ascii name))
                  ocaml_string
            | Translated (name, _, ty) ->
                label_declaration (loc (String.lowercase_ascii name))
                  (translate_type_info ty)) in
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
  if typedef then
    String_hashtbl.add context.type_table name
      (lazy (make_decl (); common_info, Struct ()))
  else
    String_hashtbl.add context.struct_table name
      (lazy (make_decl (); common_info, ()));
  String_hashtbl.add context.used_type_table ocaml_type_name ();
  let used_arg_names = String_hashtbl.create 17 in
  let print_accessor { field_name; accessor_name; stub_name } =
    make_decl ();
    let field_type = List.assoc field_name fields in
    print_ocaml_primitive context.chan_stubs stub_name ["value arg"]
      (fun channel ->
         let print_expression channel =
           Printf.fprintf channel "%s(arg).%s"
             (name_of_c_of_ocaml ocaml_type_name)
             field_name in
         let field_type_info =
           find_type_info context empty_type_interface field_type in
         let print_result channel =
           print_return_ocaml_of_c context used_arg_names print_expression
             field_type empty_type_interface field_type_info [| |] [] in
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
  let result = !result in
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
  let enum_info = { result } in
  let make_decl () =
    let type_name =
      if typedef then name
      else Printf.sprintf "enum %s" name in
    Printf.fprintf context.chan_stubs
      "%s\n%s(value ocaml)\n{\n  switch (Int_val(ocaml)) {\n"
      type_name (name_of_c_of_ocaml ocaml_type_name);
    constructors |> List.iteri (fun i constructor ->
      Printf.fprintf context.chan_stubs
        "  case %i: return %s;\n" i constructor);
    Printf.fprintf context.chan_stubs
      "  }
  failwith_fmt(\"invalid value for %s: %%d\", Int_val(ocaml));
  return %s;
}\n\n"
      (name_of_c_of_ocaml ocaml_type_name) (List.hd constructors);
    Printf.fprintf context.chan_stubs
      "value\n%s(%s v)\n{\n  switch (v) {\n"
      (name_of_ocaml_of_c ocaml_type_name) type_name;
    constructors |> List.iteri (fun i constructor ->
      Printf.fprintf context.chan_stubs
        "  case %s: return Val_int(%i);\n" constructor i);
    begin
      match result with
      | None -> ()
      | Some result ->
          Printf.fprintf context.chan_stubs
            "  case %s: failwith(\"unexpected success value\");\n"
            result
    end;
    Printf.fprintf context.chan_stubs
      "  }
  failwith_fmt(\"invalid value for %s: %%d\", v);
  return Val_int(0);
}\n\n"
      (name_of_ocaml_of_c ocaml_type_name);
    let type_decl =
      type_declaration ~ptype_kind:(Ptype_variant ocaml_constructors)
        (loc ocaml_type_name) in
    context.items_accu <- psig_type [type_decl] :: context.items_accu in
  if typedef then
    String_hashtbl.add context.type_table name
      (lazy (make_decl (); common_info, Enum enum_info))
  else
    String_hashtbl.add context.enum_table name
      (lazy (make_decl (); common_info, enum_info));
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
          name ocaml_type_name (name_of_c_of_ocaml ocaml_type_name)
          (name_of_ocaml_of_c ocaml_type_name);
        let type_decl = type_declaration (loc ocaml_type_name) in
        context.items_accu <- psig_type [type_decl] :: context.items_accu in
      String_hashtbl.add context.type_table name
        (lazy (make_decl (); common_info, Regular));
      String_hashtbl.add context.used_type_table ocaml_type_name ()
    end

type argument_type =
  | Removed of argument_type
  | Removed_output of argument_type
  | Output of { output_type : argument_type; on_success : bool; on_error : bool }
  | Fixed_value of string
  | CXType of Clang.cxtype
  | Update of Clang.cxtype
  | Array of int * Clang.cxtype * Clang.cxtype
  | Sized_string of int * Clang.cxtype * Clang.cxtype
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
  | Removed _ | Removed_output _ | Output _ | Fixed_value _ -> assert false
  | CXType ty | Update ty -> translate_type context empty_type_interface ty
  | Array (_, _, ty) ->
      let contents = 
        translate_type context empty_type_interface (Clang.get_pointee_type ty) in
      ocaml_array contents
  | Sized_string _ -> ocaml_string
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

let rec get_argument_type ty =
  match ty with
  | Removed ty -> get_argument_type ty
  | Removed_output ty -> get_argument_type ty
  | CXType ty | Update ty -> ty
  | _ -> failwith "get_argument_type"

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
  let arg_interfaces = Array.make num_args empty_type_interface in
  let apply_argument_rule (rule : argument_interface) =
    match rule with
    | Array { length; contents } ->
        let length = find_argument length index in
        let contents = find_argument contents index in
        let length_ty = get_argument_type args.(length) in
        let contents_ty = get_argument_type args.(contents) in
        args.(contents) <- Array (length, length_ty, contents_ty);
        args.(length) <- Removed args.(length)
    | Sized_string { length; contents } ->
        let length = find_argument length index in
        let contents = find_argument contents index in
        let length_ty = get_argument_type args.(length) in
        let contents_ty = get_argument_type args.(contents) in
        args.(contents) <- Sized_string (length, length_ty, contents_ty);
        args.(length) <- Removed args.(length)
    | Output { argument; on_success; on_error } ->
        let output = find_argument argument index in
        begin
          match args.(output) with
          | CXType ty ->
              let ty = Clang.get_pointee_type ty in
              outputs := { desc = (Some output, find_type_info context empty_type_interface ty); on_success; on_error } :: !outputs;
              args.(output) <- Output { output_type = CXType ty; on_success; on_error }
          | Sized_string (length, length_ty, contents_ty) ->
              let length_ty = Clang.get_pointee_type length_ty in
              let contents_ty = Clang.get_pointee_type contents_ty in
              outputs := { desc = (Some output, string_type_info); on_success; on_error } :: !outputs;
              args.(output) <- Output { output_type = Sized_string (length, length_ty, contents_ty); on_success = true; on_error = false };
              args.(length) <- Removed_output args.(length)
          | Array (length, length_ty, contents_ty) -> 
              let length_ty = Clang.get_pointee_type length_ty in
              let contents_ty = Clang.get_pointee_type contents_ty in
              outputs := { desc = (Some output, string_type_info); on_success; on_error } :: !outputs;
              args.(output) <- Output { output_type = Array (length, length_ty, contents_ty); on_success = true; on_error = false };
              args.(length) <- Removed_output args.(length)
          | _ -> failwith "Argument expected"
        end
    | Update output ->
        let output = find_argument output index in
        begin
          match args.(output) with
          | CXType ty ->
              let ty = Clang.get_pointee_type ty in
              outputs := { desc = (Some output, find_type_info context empty_type_interface ty); on_success = true; on_error = false } :: !outputs;
              args.(output) <- Update ty
          | _ -> failwith "Argument expected"
        end
    | Fixed_value { argument; value } ->
        let argument = find_argument argument index in
        args.(argument) <- Fixed_value value
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
        args.(data_caller) <- Removed args.(data_caller)
    | Type_interface { argument; interface } ->
        let argument = find_argument argument index in
        arg_interfaces.(argument) <- union_type_interface arg_interfaces.(argument)
          interface in
  List.iter apply_argument_rule function_interface.arguments;
  let outputs = List.rev !outputs in
  let result_type = Clang.get_result_type ty in
  let parameters = ref [| |] in
  let result_type_info =
    find_type_info ~parameters context function_interface.result result_type in
  let used_arg_names = String_hashtbl.create 17 in
  let wrapper_arg_names = Array.map (fun arg_name ->
    let arg_name =
      if arg_name = "" then "arg"
      else arg_name in
    make_name_unique used_arg_names arg_name) arg_names in
  let result_type_info, real_outputs, result_name =
    let rec aux first_outputs last_outputs =
      match last_outputs with
      | [] -> result_type_info, outputs, None
      | { on_success = true; on_error = true; desc = (Some i, ((_, (Enum { result = Some field } : type_info)) as type_info)) } :: tl ->
          type_info, List.rev_append first_outputs ({ on_success = true; on_error = false; desc = (None, result_type_info) } :: tl), Some (wrapper_arg_names.(i))
      | hd :: tl ->
          aux (hd :: first_outputs) tl in
    aux [] outputs in
  let params =
    !parameters |> Array.map @@ fun param ->
      let param = find_argument param index in
      let ty =
        match args.(param) with
        | CXType ty -> Clang.get_pointee_type ty
        | _ -> failwith "Argument expected" in
      args.(param) <- Output { output_type = CXType ty; on_success = true; on_error = false };
      param in
  let result_ty = translate_type_info ~outputs:(List.map (fun o -> { o with desc = translate_type_info (snd o.desc) }) real_outputs) result_type_info in
  let pval_type =
    if num_args = 0 then
      ptyp_arrow (ptyp_constr (loc (Longident.Lident "unit"))) result_ty
    else
      let add_arg arg pval_type =
        match arg with
        | Removed _ | Removed_output _ | Output _ | Fixed_value _ -> pval_type
        | _ ->
            ptyp_arrow (translate_argument_type context arg) pval_type in
      Array.fold_right add_arg args result_ty in
  let wrapper_name = name ^ "_wrapper" in
  let ocaml_args = Array.map2 (fun arg arg_name ->
      match arg with
      | Removed _ | Removed_output _ | Output _ | Fixed_value _ -> None
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
              arg_info.ocaml_of_c chan name [| |] name_ocaml in
          let print_expression channel =
            Printf.fprintf channel "caml_callback%s(%a)"
              (match List.length ocaml_args with 1 -> "" | n -> string_of_int n)
              print_list ("f" :: ocaml_args) in
          let print_call_and_return channel =
            print_return_c_of_ocaml context used_arg_names print_expression
              empty_type_interface closure_result in
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
    args |> Array.iteri (fun i arg ->
      match arg with
      | Removed _ | Removed_output _ | Fixed_value _ -> ()
      | Output { output_type = CXType ty } ->
          Printf.fprintf context.chan_stubs "\n  %s %s;"
            (Clang.get_type_spelling ty) wrapper_arg_names.(i)
      | Output { output_type = Sized_string (length, length_ty, contents_ty) } ->
          Printf.fprintf context.chan_stubs "\n  %s %s;\n  %s %s;"
            (Clang.get_type_spelling length_ty) wrapper_arg_names.(length)
            (Clang.get_type_spelling contents_ty) wrapper_arg_names.(i)
      | Output { output_type = Array (length, length_ty, contents_ty) } ->
          Printf.fprintf context.chan_stubs "\n  %s %s;\n  %s %s;"
            (Clang.get_type_spelling length_ty) wrapper_arg_names.(length)
            (Clang.get_type_spelling contents_ty) wrapper_arg_names.(i)
      | Output _ -> assert false
      | CXType ty | Update ty ->
          let common_info, type_info = find_type_info context empty_type_interface ty in
          Printf.fprintf context.chan_stubs "
  %s %s;
  %t"
            (Clang.get_type_spelling ty) wrapper_arg_names.(i)
            (fun channel -> common_info.c_of_ocaml channel
                (Printf.sprintf "%s_ocaml" wrapper_arg_names.(i))
                [| |]
                wrapper_arg_names.(i))
      | Sized_string (j, length_ty, contents_ty) ->
          Printf.fprintf context.chan_stubs "\n  %s %s = caml_string_length(%s_ocaml);"
            (Clang.get_type_spelling length_ty) wrapper_arg_names.(j)
            wrapper_arg_names.(i);
          Printf.fprintf context.chan_stubs "\n  %s %s = String_val(%s_ocaml);"
            (Pcre.replace ~pat:"const" ~templ:"" (Clang.get_type_spelling contents_ty)) wrapper_arg_names.(i) wrapper_arg_names.(i)
      | Array (j, length_ty, contents_ty) ->
          let c_length_ty = Clang.get_type_spelling length_ty in
          Printf.fprintf context.chan_stubs "\n  %s %s = Wosize_val(%s_ocaml);"
            c_length_ty wrapper_arg_names.(j)
            wrapper_arg_names.(i);
          let cell_type = Clang.get_pointee_type contents_ty in
          Printf.fprintf context.chan_stubs "\n  %s %s = xmalloc(%s * sizeof(%s));"
            (Pcre.replace ~pat:"const" ~templ:"" (Clang.get_type_spelling contents_ty)) wrapper_arg_names.(i)
            wrapper_arg_names.(j)
            (Clang.get_type_spelling cell_type);
          let common_info, type_info = find_type_info context empty_type_interface cell_type in
          let index = make_name_unique used_arg_names "i" in
          Printf.fprintf context.chan_stubs "\n  %s %s; for (%s = 0; %s < %s; %s++) {\n    %t\n  }"
            c_length_ty index index index wrapper_arg_names.(j) index (fun channel -> common_info.c_of_ocaml channel (Printf.sprintf "Field(%s_ocaml, %s)" wrapper_arg_names.(i) index) [| |] (Printf.sprintf "%s[%s]" wrapper_arg_names.(i) index))
      | Closure _ -> ());
    let wrapper_args = Array.map2 (fun arg arg_name ->
      match arg with
      | Output _ | Removed_output _ | Update _ -> "&" ^ arg_name
      | Array (_, _, contents_ty) ->
          Printf.sprintf "(%s) %s" (Clang.get_type_spelling contents_ty) arg_name
      | Fixed_value value -> value
      | _ -> arg_name) args wrapper_arg_names in
    let print_expression channel =
      Printf.fprintf channel "%s(%a)" name print_list
        (Array.to_list wrapper_args) in
    print_return_ocaml_of_c context used_arg_names print_expression
      result_type function_interface.result result_type_info ?result_name
      (params |> Array.map @@ fun i -> wrapper_arg_names.(i))
      (real_outputs |> List.map @@ fun { desc = (i, _) } -> match i with None -> "result", { desc = Regular result_type; on_success = true; on_error = false }, empty_type_interface | Some i -> wrapper_arg_names.(i), (match args.(i) with Output { output_type = CXType ty; on_success; on_error } -> { desc = Regular ty; on_success; on_error } | Update ty ->  { desc = Regular ty; on_success = true; on_error = false } | Output { output_type = Sized_string (length, _, _); on_success; on_error } -> { desc = Sized_string wrapper_arg_names.(length); on_success; on_error } | Output { output_type = Array (length, length_ty, contents_ty); on_success; on_error } -> { desc = Array (wrapper_arg_names.(length), length_ty, Clang.get_pointee_type contents_ty); on_success; on_error } | _ -> assert false), arg_interfaces.(i)) in
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

let make_destructor f =
  destructor (fun value -> Printf.sprintf "%s(%s);" f value)

let main cflags llvm_config prefix =
  let llvm_version = run_llvm_config llvm_config ["--version"] in
  let llvm_prefix = run_llvm_config llvm_config ["--prefix"] in
  let llvm_cflags = run_llvm_config llvm_config ["--cflags"] in
  let cflags = cflags |> List.map @@ String.split_on_char ',' |> List.flatten in
  let clang_options = cflags @ String.split_on_char ' ' llvm_cflags @ ["-I"; List.fold_left Filename.concat llvm_prefix ["lib"; "clang"; llvm_version; "include"]] in
  let module_interface =
    empty_module_interface |>
    add_function (Pcre.regexp "^(?!clang_)|clang_getCString|clang_disposeString|clang_disposeStringSet|clang_VirtualFileOverlay_writeToBuffer|clang_free|constructUSR|clang_executeOnThread|clang_getDiagnosticCategoryName|^clang_getDefinitionSpellingAndExtent$|^clang_disposeOverriddenCursors$|^clang_disposeSourceRangeList$|^clang_disposeTokens$|^clang_getFileUniqueID$") hidden_function_interface |>
    add_function (Pcre.regexp "^clang_") (rename_function rename_clang) |>
    add_function (Pcre.regexp "^clang_createTranslationUnitFromSourceFile$")
      (empty_function_interface |>
        add_argument (Array {
          length = Name "num_clang_command_line_args";
          contents = Name "clang_command_line_args" })) |>
    add_function (Pcre.regexp "^clang_parseTranslationUnit|^clang_indexSourceFile")
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
    add_function (Pcre.regexp "^clang_parseTranslationUnit2|^clang_createTranslationUnit2$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "out_TU"))) |>
    add_function (Pcre.regexp "^clang_visitChildren$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 2 })) |>
    add_function (Pcre.regexp "^clang_getInclusions$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 3 })) |>
    add_function (Pcre.regexp "^clang_Type_visitFields$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        add_argument (Closure {
          pointer = Name "visitor";
          data_caller = Name "client_data";
          data_callee = Index 1 })) |>
    add_function (Pcre.regexp "^clang.*_(is|equal)[A-Z]")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean)) |>
    add_enum (Pcre.regexp "^CXErrorCode$")
      (empty_enum_interface |>
        add_constant (Pcre.regexp "^CXError_Success$") { success = true }) |>
    add_enum (Pcre.regexp "^CXSaveError$")
      (empty_enum_interface |>
        add_constant (Pcre.regexp "^CXSaveError_None$") { success = true }) |>
    add_struct (Pcre.regexp "^CXUnsavedFile$")
      (empty_struct_interface |>
        add_field (Sized_string {length = "Length"; contents = "Contents"})) |>
    add_struct (Pcre.regexp "^CXType$")
      (empty_struct_interface |>
        add_accessor "kind" "get_type_kind" "clang_getTypeKind_wrapper") |>
    add_function (Pcre.regexp "^clang_saveTranslationUnit")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_enum "CXSaveError")) |>
    add_function (Pcre.regexp "^clang_reparseTranslationUnit")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_enum "CXErrorCode")) |>
    add_function (Pcre.regexp "^clang_getFileUniqueID")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_zero_is_true) |>
        add_argument (output_on_success (Name "outID"))) |>
    add_function (Pcre.regexp "^clang_indexSourceFile")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_enum "CXErrorCode") |>
        add_argument (output_on_success (Name "out_TU"))) |>
    add_function (Pcre.regexp "^clang_((indexLoc_)?getFile|getExpansion|getInstantiation|getSpelling)Location$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "file")) |>
        add_argument (output_on_success (Name "line")) |>
        add_argument (output_on_success (Name "column")) |>
        add_argument (output_on_success (Name "offset"))) |>
    add_function (Pcre.regexp "^clang_getPresumedLocation$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "filename")) |>
        add_argument (output_on_success (Name "line")) |>
        add_argument (output_on_success (Name "column"))) |>
    add_function (Pcre.regexp "^clang_indexLoc_getFileLocation$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "indexFile"))) |>
    add_function (Pcre.regexp "^clang_ModuleMapDescriptor_writeToBuffer$")
      (empty_function_interface |>
        add_argument (Sized_string {length = Name "out_buffer_size"; contents = Name "out_buffer_ptr"}) |>
        add_argument (Type_interface {argument = Name "out_buffer_ptr"; interface = empty_type_interface |> make_destructor "clang_free"}) |>
        add_argument (output_on_success (Name "out_buffer_ptr"))) |>
    add_function (Pcre.regexp "^clang_getFileContents$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as (Sized_string { can_be_null = true; length = Name "size" }))) |>
    add_function (Pcre.regexp "^clang_getDiagnosticOption$")
      (empty_function_interface |>
        add_argument (output_on_success (Name "Disable"))) |>
    add_function (Pcre.regexp "^clang_Cursor_isExternalSymbol$")
      (empty_function_interface |>
        add_result (empty_type_interface |> integer_boolean) |>
        add_argument (output_on_success (Name "language")) |>
        add_argument (output_on_success (Name "definedIn")) |>
        add_argument (output_on_success (Name "isGenerated"))) |>
    add_function (Pcre.regexp "^clang_getCompletionParent$")
      (empty_function_interface |>
        add_argument (Fixed_value { argument = Name "kind"; value = "NULL" })) |>
    add_function (Pcre.regexp "^clang_getRemappingsFromFileList$")
      (empty_function_interface |>
        add_argument (Array { contents = Name "filePaths"; length = Name "numFiles" })) |>
    add_function (Pcre.regexp "^clang_getOverriddenCursors$")
      (empty_function_interface |>
        add_argument (Array { contents = Name "overridden"; length = Name "num_overridden" }) |>
        add_argument (Type_interface {argument = Name "overridden"; interface = empty_type_interface |> make_destructor "clang_disposeOverriddenCursors"}) |>
        add_argument (output_on_success (Name "overridden"))) |>
    add_function (Pcre.regexp "^clang_get(All)?SkippedRanges$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as (Array_struct { length = "count"; contents = "ranges" }) |>
          make_destructor "clang_disposeSourceRangeList")) |>
    add_function (Pcre.regexp "^clang_tokenize$")
      (empty_function_interface |>
        add_argument (Array { contents = Name "Tokens"; length = Name "NumTokens" }) |>
        add_argument (Type_interface {argument = Name "Tokens"; interface = empty_type_interface |> destructor (fun s -> Printf.sprintf "clang_disposeTokens(TU, %s, NumTokens);" s)}) |>
        add_argument (output_on_success (Name "Tokens"))) |>
    add_function (Pcre.regexp "^clang_getDiagnosticFixIt$")
      (empty_function_interface |>
        add_argument (Update (Name "ReplacementRange"))) |>
    add_function (Pcre.regexp "^clang_getFileTime$")
      (empty_function_interface |>
        add_result (empty_type_interface |> reinterpret_as Int)) |>
    add_enum (Pcre.regexp "^CXLoadDiag_Error$")
      (empty_enum_interface |>
        add_constant (Pcre.regexp "^CXLoadDiag_None$") { success = true }) |>
    add_function (Pcre.regexp "^clang_loadDiagnostics$")
      (empty_function_interface |>
        add_argument (output (Name "error")) |>
        add_argument (output_on_error (Name "errorString"))) |>
    add_function (Pcre.regexp "^clang_getCursorPlatformAvailability$")
      (empty_function_interface |>
        add_argument (Type_interface { argument = Name "always_deprecated"; interface = empty_type_interface |> integer_boolean }) |>
        add_argument (output (Name "always_deprecated")) |>
        add_argument (output (Name "deprecated_message")) |>
        add_argument (Type_interface { argument = Name "always_unavailable"; interface = empty_type_interface |> integer_boolean }) |>
        add_argument (output (Name "always_unavailable")) |>
        add_argument (output (Name "unavailable_message"))) |>
    add_function (Pcre.regexp "^clang_Cursor_get(CXX|ObjC)Manglings$")
      (empty_function_interface |>
        add_result (empty_type_interface |>
          reinterpret_as (Array_struct { length = "Count"; contents = "Strings" }) |>
          make_destructor "clang_disposeStringSet")) in
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
  if Clang.has_error tu then
    failwith "Clang compilation error";
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
