type 'a pp = Format.formatter -> 'a -> unit

val pp_decl : Clang.Decl.t pp

val pp_type : Clang.Type.t pp

val pp_expr : Clang.Expr.t pp

val pp_stmt : Clang.Stmt.t pp

val pp_enum_constant : Clang.Enum_constant.t pp

val pp_translation_unit : Clang.Translation_unit.t pp

val pp_linkage_kind : Clang.Ast.linkage_kind pp
