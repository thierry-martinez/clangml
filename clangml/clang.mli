(** {2 Low-level interface } *)

(** The module includes {!module:Clang__bindings} which contains the
auto-generated wrappers over [libclang] and some extensions defined in
[libclang_extensions.h]. *)
include module type of struct
  include Clang__bindings
end

val iter_children : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_children f cur] calls [f] over all the direct child nodes of
    [cur]. *)

val list_of_children : cxcursor -> cxcursor list
(** [list_of_children cur] returns the list of all the direct child nodes of
    [cur]. *)

val iter_type_fields : (cxcursor -> unit) -> cxtype -> unit
(** [iter_type_fields f ty] calls [f] over all the declaration nodes of the
    fields belonging to the record type [ty] (either a struct or union). *)

val list_of_type_fields : cxtype -> cxcursor list
(** [list_of_type_fields f ty] returns the list of all the declaration nodes
    of the fields belonging to the record type [ty] (either a struct or
    union). *)

val seq_of_diagnostics : cxtranslationunit -> cxdiagnostic Seq.t
(** [seq_of_diagnostics tu] returns the diagnostics (warnings and errors)
    produced for the given translation unit *)

val is_error : cxdiagnosticseverity -> bool
(** [is_error d] returns whether [d] is [Error] or [Fatal]. *)

val has_error : cxtranslationunit -> bool
(** [has_error tu] returns whether the translation unit [tu] produced an
    error. *)

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

val float_of_cxfloat : cxfloat -> float
(** [float_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_convert_to_double}. *)

val string_of_cxfloat : cxfloat -> string
(** [string_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_to_string}. *)

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

(** {2 Abstract syntax tree} *)

module Ast : sig
  (** The module includes {!module:Clang__ast} which contains the declaration of
      the abstract syntax tree. Since the abstract syntax tree is a pure type
      declaration without value definition, the declaration is written in a
      separate module, the interface and the implementation of which have the
      same contents (and we use a symbolic link to ensure that).
   *)
  include module type of struct
    include Clang__ast
  end

  (** {!type:Options.t} stores flags that change the construction of the abstract
      syntax tree. Beware that the nodes that are ignored by default can differ
      from one version of Clang to the other. *)
  module Options : sig
    type t = {
        ignore_implicit_cast : bool [@default true];
        (** Ignore implicit cast nodes in expressions.
            See {!const:Clang__ast.Cast} for examples. *)

        ignore_paren : bool [@default true];
        (** Ignore parenthese nodes in expressions.
            See {!type:Clang__ast.expr} for examples. *)

        ignore_paren_in_types : bool [@default true];
        (** Ignore parenthese nodes in types.
            See {!type:Clang__ast.qual_type} for examples. *)
      }
          [@@deriving make]
  end

  val of_cxtype : ?options:Options.t -> cxtype -> qual_type

  val expr_of_cxcursor : ?options:Options.t -> cxcursor -> expr

  val stmt_of_cxcursor : ?options:Options.t -> cxcursor -> stmt

  val decl_of_cxcursor : ?options:Options.t -> cxcursor -> decl

  val field_of_cxcursor : ?options:Options.t -> cxcursor -> field

  val of_cxtranslationunit : ?options:Options.t -> cxtranslationunit ->
    translation_unit

  val get_enum_constant_decl_value : enum_constant -> int

  val get_field_decl_bit_width : field -> int

  val get_typedef_decl_underlying_type : ?options:Options.t -> decl -> qual_type

  val list_of_type_fields : ?options:Options.t -> qual_type -> field list

  val get_type_declaration : ?options:Options.t -> qual_type -> decl

  val get_typedef_underlying_type : ?options:Options.t -> qual_type -> qual_type

  val parse_file : ?index:cxindex ->
    ?command_line_args:string list ->
      ?unsaved_files:cxunsavedfile list ->
        ?clang_options:Cxtranslationunit_flags.t ->
          ?options:Options.t ->
            string -> translation_unit

  val parse_string : ?index:cxindex -> ?filename:string ->
    ?command_line_args:string list ->
      ?unsaved_files:cxunsavedfile list ->
        ?clang_options:Cxtranslationunit_flags.t ->
          ?options:Options.t ->
            string -> translation_unit
end

module Type : sig
  type t = Ast.qual_type

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Map : Map.S with type key = t
end

module Expr : sig
  type t = Ast.expr

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Map : Map.S with type key = t
end

module Stmt : sig
  type t = Ast.stmt

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Map : Map.S with type key = t
end

module Decl : sig
  type t = Ast.decl

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Map : Map.S with type key = t
end
