[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]

open Clang__bindings

open Clang__types

val string_of_language : language -> string

val language_of_string : string -> language

val language_of_string_opt : string -> language option

val suffix_of_language : language -> string

val extern_of_language : language -> string

val string_of_cxx_access_specifier : cx_cxxaccessspecifier -> string

(** {2 Compatibility layer} *)

[%%meta Metapp.Sigi.of_list (
  if Clangml_config.version >= { major = 3; minor = 5; subminor = 0 } then
    []
  else [%sig:
    type cxerrorcode =
      | Failure
      | Crashed
      | InvalidArguments
      | ASTReadError
    (** Error codes introduced in clang 3.5, declared here for compatibility.
        Only {!constr:Failure} will be used. *)

    val parse_translation_unit2 :
      cxindex -> string -> string array -> cxunsavedfile array ->
        Cxtranslationunit_flags.t -> (cxtranslationunit, cxerrorcode) result
    (** Compatibility wrapper for [parse_translation_unit2].
        In case of error, [Error Failure] will be returned. *)])]

val predefined_expr_get_function_name : cxcursor -> cxcursor -> string

[%%meta Metapp.Sigi.of_list (
  if Clangml_config.version >= { major = 3; minor = 7; subminor = 0 } then
    []
  else [%sig:
    type cxvisitorresult =
      | Break
      | Continue

    val type_visit_fields : cxtype -> (cxcursor -> cxvisitorresult) -> bool])]

(** {2 Parsing files and strings } *)

val parse_file : ?index:cxindex ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> cxtranslationunit
(** [parse_file ?index ?command_line_args ?unsaved_files ?options filename]
  parses file [filename] and returns its translation unit.
    This function is equivalent to {!val:Clang.parse_translation_unit2},
    with, by default, a fresh [index]
    (created by {!val:Clang.create_index}[ true true]),
    an empty list for [command_line_args] and [unsaved_files], and default
    [options]
    (obtained by {!val:Clang.default_editing_translation_unit_options}).
    See also {!val:Clang.Ast.parse_file} which returns a pattern-matchable
    representation of the AST. *)

val parse_file_res : ?index:cxindex ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) result
(** Equivalent to {!val:parse_file} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

val parse_string : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> cxtranslationunit
(** [parse_string ?index ?filename ?command_line_args ?unsaved_files ?options
    contents]
  parses string [contents] and returns its translation unit.
    This function calls {!val:Clang.parse_file} with an unsaved file called
    [filename] (by default, [<string>.c]) with [contents]: this unsaved file
    is consed to the list [unsaved_files] (by default, an empty list).
    Note that [filename] should have the [.cpp] suffix to parse C++ code
    (for instance, [<string>.cpp]).
    See also {!val:Clang.Ast.parse_string} which returns a pattern-matchable
    representation of the AST. *)

val parse_string_res : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) result
(** Equivalent to {!val:parse_string} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

(** {2 Iterators } *)

val iter_children : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_children f cur] calls [f] over all the direct child nodes of
    [cur]. *)

val iter_decl_attributes : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_decl_attributes f cur] calls [f] over all the attributes of
    the declaration [cur]. *)

val iter_cxxrecorddecl_bases : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_decl_attributes f cur] calls [f] over all the bases of
    the CXX class [cur]. *)

val list_of_children_filter_map : (cxcursor -> 'a option) -> cxcursor -> 'a list
(** [list_of_children_map f cur] applies f to all the direct child nodes of
    [cur] and returns the list of [Some _] results. *)

val list_of_children_map : (cxcursor -> 'a) -> cxcursor -> 'a list
(** [list_of_children_map f cur] applies f to all the direct child nodes of
    [cur] and returns the list of results. *)

val list_of_children : cxcursor -> cxcursor list
(** [list_of_children cur] returns the list of all the direct child nodes of
    [cur]. *)

val option_of_cursor_map : (cxcursor -> 'a) -> cxcursor -> 'a option
(** [option_of_cursor_map f cur] returns [None] if [cur] is null,
    [Some (f cur)] otherwise. *)

val option_of_cursor : cxcursor -> cxcursor option
(** [option_of_cursor cur] returns [None] if [cur] is null,
    [Some cur] otherwise. *)

val first_child : cxcursor -> cxcursor option
(** [first_child cur] returns the first direct child node of [cur], or [None]
    if there is no child. *)

val last_child : cxcursor -> cxcursor option
(** [last_child cur] returns the last direct child node of [cur], or [None]
    if there is no child. *)

val iter_type_fields : (cxcursor -> unit) -> cxtype -> unit
(** [iter_type_fields f ty] calls [f] over all the declaration nodes of the
    fields belonging to the record type [ty] (either a struct or union).
    See also {!val:Clang.Type.iter_fields} for a higher-level interface. *)

val list_of_type_fields : cxtype -> cxcursor list
(** [list_of_type_fields f ty] returns the list of all the declaration nodes
    of the fields belonging to the record type [ty] (either a struct or
    union). *)

val iter_decl_context : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_decl_context f c] calls [f] over all the declaration nodes of
    declaration context [c]. *)

val list_of_decl_context_map : (cxcursor -> 'a) -> cxcursor -> 'a list
(** [list_of_decl_context_map f c] applies [f] to all the declaration nodes
    of declaration context [c] and returns the list of results. *)

val list_of_decl_context : cxcursor -> cxcursor list
(** [list_of_decl_context c] returns the list of all the declaration nodes
    of declaration context [c]. *)

val iter_indirect_field_decl_chain : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_indirect_field_decl_chain f c] calls [f] over the chain of
    indirect field declaration [c]. *)

val list_of_indirect_field_decl_chain_map :
  (cxcursor -> 'a) -> cxcursor -> 'a list
(** [list_of_indirect_field_decl_chain_map f c] applies [f] to all declarations
    in the chain of indirect field declaration [c] and returns the list of
    results. *)

val list_of_indirect_field_decl_chain : cxcursor -> cxcursor list
(** [list_of_indirect_field_decl_chain f c] returns the list of the declarations
    in the chain of indirect field declaration [c]. *)

val iter_cxxconstructor_initializers :
  (clang_ext_cxxctorinitializer -> unit) -> cxcursor -> unit
(** [iter_cxxconstructor_initializers f c] calls [f] over the initializers
    of constructor [c]. *)

val list_of_cxxconstructor_initializers_map :
  (clang_ext_cxxctorinitializer -> 'a) -> cxcursor -> 'a list
(** [list_of_cxxconstructor_initializers_map f c] calls [f] over the
    initializers of constructor [c] and returns the list of results. *)

val list_of_cxxconstructor_initializers :
  cxcursor -> clang_ext_cxxctorinitializer list
(** [list_of_cxxconstructor_initializers_map c] returns the list of
    the initializers of constructor [c]. *)

val list_of_iter : (('a -> unit) -> unit) -> 'a list
(** [list_of_iter iter] calls [iter f] and returns all the values which [f] has
    been applied to. *)

val list_of_iter_filter_map :
  ('a -> 'b option) -> (('a -> unit) -> unit) -> 'b list
(** [list_of_iter g iter] calls [iter f] and applies [g] to all the values
    which [f] has been applied to and returns the [Some _] results. *)

val list_of_iter_map : ('a -> 'b) -> (('a -> unit) -> unit) -> 'b list
(** [list_of_iter g iter] calls [iter f] and applies [g] to all the values
    which [f] has been applied to and returns the results. *)

(** {2 Integer or floating point types} *)
val is_integer : cxtypekind -> bool
(** [is_integer ty] returns true if [ty] is a built-in integer type. *)

val is_unsigned_integer : cxtypekind -> bool
(** [is_unsigned_integer ty] returns true if [ty] is a built-in integer type
    that is unsigned. *)

val is_signed_integer : cxtypekind -> bool
(** [is_signed_integer ty] returns true if [ty] is a built-in integer type
    that is signed. *)

val is_floating_point : cxtypekind -> bool
(** [is_floating_point ty] returns true if [ty] is a built-in floating-point
    type. *)

(** {2 Integer conversions } *)

val int64_of_cxint_opt : ?signed:bool -> cxint -> Int64.t option
(** [int64_of_cxint_opt ~signed x] returns [Some i] if [x] is representable as
    a 64-bit integer value [i], or [None] otherwise.
    [signed] specifies if [x] is signed (default: true). *)

val int64_of_cxint : ?signed:bool -> cxint -> Int64.t
(** [int64_of_cxint x] returns [i] if [x] is representable as
    a 64-bit integer value [i], or raises [Invalid_argument _] otherwise.
    [signed] specifies if [x] is signed (default: true).*)

val int_of_cxint_opt : ?signed:bool -> cxint -> int option
(** [int_of_cxint_opt x] returns [Some i] if [x] is representable as
    an integer value [i], or [None] otherwise.
    [signed] specifies if [x] is signed (default: true). *)

val int_of_cxint : ?signed:bool -> cxint -> int
(** [int_of_cxint x] returns [i] if [x] is representable as
    an integer value [i], or raises [Invalid_argument _] otherwise.
    [signed] specifies if [x] is signed (default: true). *)

val string_of_cxint : ?signed:bool -> cxint -> string
(** [string_of_cxint f] is an alias for
    {!val:Clang__bindings.ext_int_to_string}, radix 10.
    [signed] specifies if [x] is signed (default: true). *)

(** {2 Floating conversions } *)

val float_of_cxfloat_opt : cxfloat -> float option
(** [float_of_cxfloat_opt x] returns [Some f] if [x] is a floating-point
    value with either IEEE single or double semantics,
    or [None] otherwise. *)

val float_of_cxfloat : cxfloat -> float
(** [float_of_cxfloat x] returns [f] if [x] is a floating-point
    value with either IEEE single or double semantics,
    or raises [Invalid_argument _] otherwise. *)

val string_of_cxfloat : cxfloat -> string
(** [string_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_to_string}. *)


(** {2 Error management } *)

val string_of_cxerrorcode : cxerrorcode -> string
(** [string_of_cxerrorcode ec] returns a message describing [ec]. *)

val seq_of_diagnostics : cxtranslationunit -> cxdiagnostic Seq.t
(** [seq_of_diagnostics tu] returns the diagnostics
    (notes, warnings, errors, ...)
    produced for the given translation unit *)

val concrete_of_cxsourcelocation :
    location_kind -> cxsourcelocation -> concrete_location
(** [concrete_of_cxsourcelocation kind location] returns the concrete location
    associated to [location]. [kind] selects whether
    {!val:Clang.get_presumed_location} (which ignores [#] line directive)
    or {!val:Clang.get_expansion_location} (which honors [#] line directive)
    is called. *)

val string_of_severity : cxdiagnosticseverity -> string
(** [string_of_severity severity] returns a string describing the severity:
    this string is used as prefix for the diagnostic in {!val:pp_diagnostic}. *)

val pp_concrete_location :
    ?options:Display_source_location.t ->
      ?ranges:(unit -> (concrete_location * concrete_location) list) ->
      Format.formatter -> concrete_location -> unit

val pp_diagnostic : ?options:Diagnostic_display_options.t -> Format.formatter ->
  cxdiagnostic -> unit
(** [pp_diagnostic ?options fmt diag] formats diagnostic [diag], mimicking
    {!val: Clang__bindings.format_diagnostic} behavior.
    {!Clang__types.Display_source_location.kind} supports various location kinds
    whereas {!val: Clang__bindings.format_diagnostic} only displays spelling
    locations. *)

val format_diagnostics :
  ?pp:((Format.formatter -> unit -> unit)
       -> Format.formatter -> unit -> unit) ->
  ?options:Diagnostic_display_options.t ->
  cxdiagnosticseverity list -> Format.formatter ->
  cxtranslationunit -> unit
(** [format_diagnostics ?pp severities fmt tu] formats the
    diagnostics produced for the given translation unit. Only the diagnostics,
    the severity of which is listed in [severities] are displayed.
    If there is a printer given in [pp], then this printer is called once if
    and only if there is at least one diagnostic to display, and [pp] should
    call the printer passed in its first argument to display the diagnostics.
    In the case there is no diagnostic to display, nothing is printed. *)

val error : cxdiagnosticseverity list
(** [error] contains the severities [Error] and [Fatal]. *)

val warning_or_error : cxdiagnosticseverity list
(** [warning_or_error] contains the severities [Warning], [Error] and
    [Fatal]. *)

val not_ignored_diagnostics : cxdiagnosticseverity list
(** [not_ignored_diagnostics] contains the severities [Note], [Warning],
    [Error] and [Fatal]. *)

val all_diagnostics : cxdiagnosticseverity list
(** [all_diagnostics] contains the severities [Ignored], [Note], [Warning],
    [Error] and [Fatal]. *)

val has_severity : cxdiagnosticseverity list -> cxtranslationunit -> bool
(** [has_severity l tu] returns whether the translation unit [tu] produced a
    diagnostic, the severity of which belongs to [l]. *)

val cursor_get_translation_unit : cxcursor -> cxtranslationunit
(** [cursor_get_translation_unit cursor] returns the translation unit
    associated to [cursor]. *)

val sourcelocation_get_translation_unit :
    cxsourcelocation -> cxtranslationunit
(** [sourcelocation_get_translation_unit location] returns the translation
    unit associated to [location]. *)

val binary_of_overloaded_operator_kind :
    clang_ext_overloadedoperatorkind -> clang_ext_binaryoperatorkind

(** {2 General purpose utilities} *)

val extract_prefix_from_list :
    ('a -> 'b option) -> 'a list -> 'b list * 'a list
