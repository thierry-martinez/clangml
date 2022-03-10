let () =
  let code = {|
    typedef struct {
      int x;
      int y;
    } vect;

    int main() {
      vect v;
      v = (vect) {0,10};
    }
 |} in
  let ast = Clang.Ast.parse_file
    ~unsaved_files:[{ filename = "code.c"; contents = code }]
    "code.c" in
  let compound_expressions =
    match ast.desc.items with
    | [_; _; { desc = Function ({ body = Some ({ desc = Compound [_;
        { desc = Expr { desc =
          BinaryOperator { rhs; _ }; _ }; _ }]; _ }) ; _}); _}] ->
        rhs
    | _ -> assert false in
  match compound_expressions.desc with
  | CompoundLiteral { init = { desc = InitList [
      { desc = IntegerLiteral (Int 0); _ };
      { desc = IntegerLiteral (Int 10); _ }]; _}; _ } ->
      ()
  | _ -> assert false
