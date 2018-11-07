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

  val of_cxtranslationunit : cxtranslationunit -> translation_unit
end
