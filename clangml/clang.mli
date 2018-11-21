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

module Ast : sig
  include module type of struct
    include Clang__ast
  end

  val of_cxtype : ?ignore_paren:bool -> ?ignore_implicit_cast:bool -> cxtype ->
    qual_type

  val expr_of_cxcursor : ?ignore_paren:bool -> ?ignore_implicit_cast:bool ->
    cxcursor -> expr

  val stmt_of_cxcursor : ?ignore_paren:bool -> ?ignore_implicit_cast:bool ->
    cxcursor -> stmt

  val of_cxtranslationunit : ?ignore_paren:bool -> ?ignore_implicit_cast:bool ->
    cxtranslationunit -> translation_unit
end

val parse_string : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string array ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) Stdcompat.result
