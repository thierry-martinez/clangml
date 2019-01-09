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
      separate module, written in an implementation file (.ml) without
      interface file (.mli)).
   *)
  include module type of struct
    include Clang__ast
  end

  val node : ?decoration:decoration -> ?cursor:cxcursor ->
    ?location:source_location -> ?qual_type:qual_type -> 'a -> 'a node
  (** [node desc] returns a node with the given [desc] value. The associated
      [cxcursor] is the null cursor (returned by {val:get_null_cursor}). This
      function can be used to build ASTs from scratch. *)

  val cursor_of_decoration : decoration -> cxcursor

  val cursor_of_node : 'a node -> cxcursor

  val location_of_decoration : decoration -> source_location

  val location_of_node : 'a node -> source_location

  type concrete_location = {
      filename : string;
      line : int;
      column : int
    }

  val get_presumed_location : source_location -> concrete_location

  val get_expansion_location : source_location -> concrete_location

  val string_of_elaborated_type_keyword : elaborated_type_keyword -> string

  val string_of_unary_operator_kind : unary_operator_kind -> string

  val string_of_binary_operator_kind : binary_operator_kind -> string

  val literal_of_int : int -> integer_literal

  val int64_of_literal_opt : integer_literal -> Int64.t option
  (** [int64_of_literal_opt x] returns [Some i] if [x] is representable as
      a 64-bit integer value [i], or [None] otherwise. *)
  
  val int64_of_literal : integer_literal -> Int64.t
  (** [int64_of_literal x] returns [i] if [x] is representable as
      a 64-bit integer value [i], or raises [Failure _] otherwise. *)
  
  val int_of_literal_opt : integer_literal -> int option
  (** [int_of_literal_opt x] returns [Some i] if [x] is representable as
      an integer value [i], or [None] otherwise. *)
  
  val int_of_literal : integer_literal -> int
  (** [int_of_literal x] returns [i] if [x] is representable as
      an integer value [i], or raises [Failure _] otherwise. *)
  
  val string_of_integer_literal : integer_literal -> string
  (** [string_of_integer_literal f] is an alias for
      {!val:Clang__bindings.ext_int_to_string}, radix 10 and signed. *)
  
  val literal_of_float : float -> floating_literal

  val float_of_literal : floating_literal -> float
  (** [float_of_cxfloat f] is an alias for
      {!val:Clang__bindings.ext_float_convert_to_double}. *)
  
  val string_of_floating_literal : floating_literal -> string
  (** [string_of_float_literal f] is an alias for
      {!val:Clang__bindings.ext_float_to_string}. *)

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

  val of_cxtranslationunit : ?options:Options.t -> cxtranslationunit ->
    translation_unit

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

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val make : ?const:bool -> ?volatile:bool -> ?restrict:bool -> Ast.type_desc -> t

  val of_cxtype : ?options:Ast.Options.t -> cxtype -> t

  val of_cursor : ?options:Ast.Options.t -> cxcursor -> t

  val of_decoration : ?options:Ast.Options.t -> Ast.decoration -> t

  val of_node : ?options:Ast.Options.t -> 'a Ast.node -> t

  val iter_fields : ?options:Ast.Options.t ->
    (Ast.field -> unit) -> t -> unit

  val list_of_fields : ?options:Ast.Options.t -> t -> Ast.field list

  val get_declaration : ?options:Ast.Options.t -> t -> Ast.decl

  val get_typedef_underlying_type : ?options:Ast.Options.t -> t -> t

  val get_align_of : t -> int

  val get_size_of : t -> int
end

module Expr : sig
  type t = Ast.expr

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
end

module Stmt : sig
  type t = Ast.stmt

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
end

module Decl : sig
  type t = Ast.decl

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t

  val get_typedef_underlying_type : ?options:Ast.Options.t -> t -> Type.t
end

module Enum_constant : sig
  type t = Ast.enum_constant

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t

  val get_value : t -> int
end

module Field : sig
  type t = Ast.field

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t

  val get_bit_width : t -> int
end
