open Clang__bindings

open Clang__compat

type language = C | Cxx

val string_of_language : language -> string

val language_of_string : string -> language

(** {2 Parsing files and strings } *)

val parse_file : ?index:cxindex ->
  ?command_line_args:string list -> ?language:language ->
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
  ?command_line_args:string list -> ?language:language ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) result
(** Equivalent to {!val:parse_file} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

val parse_string : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list -> ?language:language ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> cxtranslationunit
(** [parse_string ?index ?filename ?command_line_args ?unsaved_files ?options contents]
  parses string [contents] and returns its translation unit.
    This function calls {!val:Clang.parse_file} with an unsaved file called
    [filename] (by default, [<string>.c]) with [contents]: this unsaved file
    is consed to the list [unsaved_files] (by default, an empty list).
    Note that [filename] should have the [.cpp] suffix to parse C++ code
    (for instance, [<string>.cpp]).
    See also {!val:Clang.Ast.parse_string} which returns a pattern-matchable
    representation of the AST. *)

val parse_string_res : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list -> ?language:language ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) result
(** Equivalent to {!val:parse_string} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

(** {2 Iterators } *)

val iter_children : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_children f cur] calls [f] over all the direct child nodes of
    [cur]. *)

val list_of_children : cxcursor -> cxcursor list
(** [list_of_children cur] returns the list of all the direct child nodes of
    [cur]. *)

val iter_type_fields : (cxcursor -> unit) -> cxtype -> unit
(** [iter_type_fields f ty] calls [f] over all the declaration nodes of the
    fields belonging to the record type [ty] (either a struct or union).
    See also {!val:Clang.Type.iter_fields} for a higher-level interface. *)

val list_of_type_fields : cxtype -> cxcursor list
(** [list_of_type_fields f ty] returns the list of all the declaration nodes
    of the fields belonging to the record type [ty] (either a struct or
    union). *)

(** {2 Integer conversions } *)

val int64_of_cxint_opt : cxint -> Int64.t option
(** [int64_of_cxint_opt x] returns [Some i] if [x] is representable as
    a 64-bit integer value [i], or [None] otherwise. *)

val int64_of_cxint : cxint -> Int64.t
(** [int64_of_cxint x] returns [i] if [x] is representable as
    a 64-bit integer value [i], or raises [Failure _] otherwise. *)

val int_of_cxint_opt : cxint -> int option
(** [int_of_cxint_opt x] returns [Some i] if [x] is representable as
    an integer value [i], or [None] otherwise. *)

val int_of_cxint : cxint -> int
(** [int_of_cxint x] returns [i] if [x] is representable as
    an integer value [i], or raises [Failure _] otherwise. *)

val string_of_cxint : cxint -> string
(** [string_of_cxint f] is an alias for
    {!val:Clang__bindings.ext_int_to_string}, radix 10 and signed. *)

(** {2 Floating conversions } *)

val float_of_cxfloat : cxfloat -> float
(** [float_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_convert_to_double}. *)

val string_of_cxfloat : cxfloat -> string
(** [string_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_to_string}. *)

(** {2 Error management } *)

val string_of_cxerrorcode : cxerrorcode -> string
(** [string_of_cxerrorcode ec] returns a message describing [ec]. *)

val seq_of_diagnostics : cxtranslationunit -> cxdiagnostic Seq.t
(** [seq_of_diagnostics tu] returns the diagnostics (warnings and errors)
    produced for the given translation unit *)

val is_error : cxdiagnosticseverity -> bool
(** [is_error d] returns whether [d] is [Error] or [Fatal]. *)

val is_warning_or_error : cxdiagnosticseverity -> bool
(** [is_warning_or_error d] returns whether [d] is [Warning] or [Error] or [Fatal]. *)

val has_error : cxtranslationunit -> bool
(** [has_error tu] returns whether the translation unit [tu] produced an
    error. *)

val has_warning_or_error : cxtranslationunit -> bool
(** [has_warning_or_error tu] returns whether the translation unit [tu] produced a
    warning or an error. *)
