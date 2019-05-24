(** {!type:Options.t} stores flags that change the construction of the
    abstract syntax tree. Beware that the nodes that are ignored by default
    can differ from one version of Clang to the other. *)

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

    ignore_expr_with_cleanups : bool [@default true];

    ignore_materialize_temporary_expr : bool [@default true];

    convert_integer_literals : bool [@default true];
    (** Convert integer literals into {!constr:Clang__ast.Int}.
        See {!constr:Clang__ast.IntegerLiteral} for examples. *)

    convert_floating_literals : bool [@default true];
    (** Convert floating literals into {!constr:Clang__ast.Float}.
        See {!constr:Clang__ast.FloatingLiteral} for examples. *)
  }
      [@@deriving make]
