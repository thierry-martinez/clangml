[@@@ocaml.warning "-30"]

[%%override module Clang__bindings = struct
  [%%recursive [%%types]]
    [@@deriving traverse_lift]
end]

open Clang__bindings

[%%override module Clang__ast = struct
  [%%recursive [%%types]]
    [@@deriving traverse_lift]
end]

class virtual ['a] lift = object
  inherit ['a] Clang__bindings.lift
  inherit ['a] Clang__ast.lift
end

class lift_expr loc = object(self)
  inherit [Parsetree.expression] lift
  inherit Ppxlib_metaquot_lifters.expression_lifters loc

  method! open_node :
    'a 'b . ('a -> Parsetree.expression) -> ('b -> Parsetree.expression) ->
      ('a, 'b) Clang.Ast.open_node -> Parsetree.expression =
    fun _a _qual_type node ->
      [%expr {
        decoration = Custom { location = None; qual_type = None };
        desc = [%e _a node.desc] }]

  method! qual_type (qual_type : Clang.Ast.qual_type) =
    let lift = Ppxlib_metaquot.Expr.lift loc in
    let const = lift#bool qual_type.const in
    let volatile = lift#bool qual_type.volatile in
    let restrict = lift#bool qual_type.restrict in
    [%expr {
      cxtype = Clang__bindings.get_cursor_type
        (Clang__bindings.get_null_cursor ());
      const = [%e const]; volatile = [%e volatile]; restrict = [%e restrict];
      desc = [%e self#type_desc qual_type.desc ] }]
end

class lift_pattern loc = object(self)
  inherit [Parsetree.pattern] lift
  inherit Ppxlib_metaquot_lifters.pattern_lifters loc

  method! open_node :
    'a 'b . ('a -> Parsetree.pattern) -> ('b -> Parsetree.pattern) ->
      ('a, 'b) Clang.Ast.open_node -> Parsetree.pattern =
    fun _a _qual_type node ->
      [%pat? {
        decoration = _;
        desc = [%p _a node.desc] }]

  method! qual_type (qual_type : Clang.Ast.qual_type) =
    let lift = Ppxlib_metaquot.Patt.lift loc in
    let const = lift#bool qual_type.const in
    let volatile = lift#bool qual_type.volatile in
    let restrict = lift#bool qual_type.restrict in
    [%pat? {
      cxtype = _; const = [%p const];
      volatile = [%p volatile]; restrict = [%p restrict];
      desc = [%p self#type_desc qual_type.desc ] }]
end
