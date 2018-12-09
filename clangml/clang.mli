include module type of struct
  include Clang__bindings
end

val iter_children : (cxcursor -> unit) -> cxcursor -> unit

val list_of_children : cxcursor -> cxcursor list

val iter_type_fields : (cxcursor -> unit) -> cxtype -> unit

val list_of_type_fields : cxtype -> cxcursor list

val seq_of_diagnostics : cxtranslationunit -> cxdiagnostic Seq.t

val is_error : cxdiagnosticseverity -> bool

val has_error : cxtranslationunit -> bool

val int64_of_cxint_opt : cxint -> Int64.t option

val int64_of_cxint : cxint -> Int64.t

val int_of_cxint_opt : cxint -> int option

val int_of_cxint : cxint -> int

module Ast : sig
  include module type of struct
    include Clang__ast
  end

  val of_cxtype :
      ?ignore_implicit_cast:bool -> ?ignore_paren:bool -> ?ignore_paren_in_types:bool ->
        cxtype -> qual_type

  val expr_of_cxcursor :
      ?ignore_implicit_cast:bool -> ?ignore_paren:bool -> ?ignore_paren_in_types:bool ->
        cxcursor -> expr

  val stmt_of_cxcursor :
      ?ignore_implicit_cast:bool -> ?ignore_paren:bool -> ?ignore_paren_in_types:bool ->
        cxcursor -> stmt

  val of_cxtranslationunit :
      ?ignore_implicit_cast:bool -> ?ignore_paren:bool -> ?ignore_paren_in_types:bool ->
        cxtranslationunit -> translation_unit
end

val parse_file : ?index:cxindex ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) Stdcompat.result

val parse_string : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) Stdcompat.result
