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

  module Options : sig
    type t = {
        ignore_implicit_cast : bool [@default true];
        ignore_paren : bool [@default true];
        ignore_paren_in_types : bool [@default true];
      }
          [@@deriving make]
  end

  val of_cxtype :
      ?options:Options.t ->
        cxtype -> qual_type

  val expr_of_cxcursor :
      ?options:Options.t ->
        cxcursor -> expr

  val stmt_of_cxcursor :
      ?options:Options.t ->
        cxcursor -> stmt

  val of_cxtranslationunit :
      ?options:Options.t ->
        cxtranslationunit -> translation_unit
end

module Type : sig
  type t = Ast.qual_type

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Map : Map.S with type key = t
end

val string_of_cxerrorcode : cxerrorcode -> string

val parse_file : ?index:cxindex ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> cxtranslationunit

val parse_string : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> cxtranslationunit
