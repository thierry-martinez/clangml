open Clang__bindings

open Clang__compat

let iter_children f c =
  let exn_ref = ref (None : exn option) in
  if
    visit_children c @@ fun cur _par ->
      try
        f cur;
        Continue
      with exn ->
        exn_ref := Some exn;
        Break
  then
    assert (!exn_ref = None)
  else
    match !exn_ref with
    | None -> assert false
    | Some exn -> raise exn

let list_of_children c =
  let children_ref = ref [] in
  c |> iter_children (fun cur -> children_ref := cur :: !children_ref);
  List.rev !children_ref

let iter_type_fields f ty =
  let exn_ref = ref (None : exn option) in
  assert begin
    type_visit_fields ty @@ fun cur ->
      try
        f cur;
        Continue
      with exn ->
        exn_ref := Some exn;
        Break
  end;
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

let is_warning_or_error diagnostic_severity =
  match diagnostic_severity with
  | Warning | Error | Fatal -> true
  | _ -> false

let has_error tu =
  try
    seq_of_diagnostics tu |>
    Seq.iter (fun d ->
      if is_error (get_diagnostic_severity d) then raise Exit);
    false
  with Exit ->
    true

let has_warning_or_error tu =
  try
    seq_of_diagnostics tu |>
    Seq.iter (fun d ->
      if is_warning_or_error (get_diagnostic_severity d) then raise Exit);
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

let parse_file_res ?(index = create_index true true)
    ?(command_line_args = []) ?(unsaved_files = [])
    ?(options = default_editing_translation_unit_options ()) filename =
  parse_translation_unit2 index filename (Array.of_list command_line_args)
    (Array.of_list unsaved_files) options

let parse_file ?index ?command_line_args ?unsaved_files ?options filename =
  match
    parse_file_res ?index ?command_line_args ?unsaved_files ?options filename
  with
  | Ok cxtranslationunit -> cxtranslationunit
  | Error cxerrorcode -> failwith (string_of_cxerrorcode cxerrorcode)

let parse_string_res ?index ?(filename = "<string>.c")
    ?command_line_args ?(unsaved_files = [])
    ?options contents =
  parse_file_res ?index ?command_line_args
    ~unsaved_files:({ filename; contents } :: unsaved_files)
    ?options filename

let parse_string ?index ?filename ?command_line_args ?unsaved_files ?options
    contents =
  match
    parse_string_res ?index ?filename ?command_line_args ?unsaved_files ?options
      contents
  with
  | Ok cxtranslationunit -> cxtranslationunit
  | Error cxerrorcode -> failwith (string_of_cxerrorcode cxerrorcode)
