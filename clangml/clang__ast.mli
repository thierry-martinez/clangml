[@@@ocaml.warning "-30"]

open Clang__bindings

(** Most of the AST nodes carry the [cxcursor] from where they come from
    in the Clang translation unit. *)
type 'a node = {
    cxcursor : cxcursor;
    desc : 'a;
  }

(*{[
open Stdcompat

let () =
  prerr_endline (Clang.get_clang_version ())
]}*)

(**
The following example declares the function [parse_declaration_list]
that returns the AST obtained from the parsing of [source] string as a
declaration list:
this function is used in the following examples to check the AST of
various programs.
    {[
let parse_declaration_list ?filename ?ignore_paren ?ignore_paren_in_types
    source =
  prerr_endline source;
  (Clang.parse_string ?filename source |> Result.get_ok |>
      Clang.Ast.of_cxtranslationunit ?ignore_paren ?ignore_paren_in_types)
    .desc.items
   ]}*)

type cast_kind =
  | CStyle
  | Implicit

(** Qualified type. *)
type qual_type = {
    cxtype : cxtype;
    const : bool;
(** [true] if the type is const-qualified.
      {[
let example = "const int one = 1;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "one";
      qual_type = {
        const = true;
        desc = OtherType Int};
      init = Some { desc = IntegerLiteral one }}}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false

let example = "int x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x";
      qual_type = {
        const = false;
        desc = OtherType Int};
      init = None }}] -> ()
  | _ -> assert false
     ]}*)
    volatile : bool;
(** [true] if the type is volatile-qualified.
    {[
let example = "volatile int x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x";
      qual_type = {
        volatile = true;
        desc = OtherType Int}}}] -> ()
  | _ -> assert false

let example = "int x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x";
      qual_type = {
        volatile = false;
        desc = OtherType Int}}}] -> ()
  | _ -> assert false
    ]}*)
    restrict : bool;
(** [true] if the type is restrict-qualified.
    {[
let example = "int * restrict x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = true;
      desc = Pointer { desc = OtherType Int }}}}] -> ()
  | _ -> assert false

let example = "int * x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = false;
      desc = Pointer { desc = OtherType Int }}}}] -> ()
  | _ -> assert false
    ]}*)
    desc : type_desc;
  }

(** Type description. *)
and type_desc =
  | Pointer of qual_type
(** Pointer.
    {[
let example = "char *s;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "s"; qual_type = { desc =
      Pointer { desc = OtherType Char_S }}}}] -> ()
  | _ -> assert false
    ]}*)
  | ConstantArray of {
      element : qual_type;
      size : int;
    }
(** Constant-sized array.
    {[
let example = "char s[42];"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "s"; qual_type = { desc = ConstantArray {
      element = { desc = OtherType Char_S };
      size = 42 }}}}] -> ()
  | _ -> assert false
    ]}*)
  | IncompleteArray of {
      element : qual_type;
    }
(** Incomplete array.
    {[
let example = "struct s { int i; char array[]; };"

let () =
  match parse_declaration_list example with
  | [{ desc = Struct { name = "s"; fields = [
      { desc = { name = "i"; qual_type = { desc = OtherType Int}}};
      { desc = { name = "array"; qual_type = { desc = IncompleteArray {
        element = { desc = OtherType Char_S }}}}}] }}] -> ()
  | _ -> assert false
    ]}*)
  | VariableArray of {
      element : qual_type;
      size : expr;
    }
  (** Variable array.
    {[
let example = "void f(int i, char array[i]);"

let () =
  match parse_declaration_list example with
  | [{ desc = Function { name = "f"; function_type =
      { result = { desc = OtherType Void };
        args = Some {
          non_variadic = [
            ("i", { desc = OtherType Int });
            ("array", { desc = VariableArray {
               element = { desc = OtherType Char_S };
               size = { desc = DeclRef "i" }}})];
          variadic = false }}}}] -> ()
  | _ -> assert false
    ]}*)
  | Elaborated of {
      keyword : clang_ext_elaboratedtypekeyword;
      named_type : qual_type;
    }
(** Elaborated type.
    {[
let example = "enum example { A, B, C }; enum example e;"

let () =
  match parse_declaration_list example with
  | [{ desc = Enum _ };
     { desc = Var { name = "e"; qual_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { desc = Enum "example" }}}}}] -> ()
  | _ -> assert false
    ]}*)
  | Enum of string
(** Enum type.
    {[
let example = "enum { A, B, C } e;"

let () =
  match parse_declaration_list example with
  | [{ desc = Enum _ };
     { desc = Var { name = "e"; qual_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { cxtype; desc = Enum "" }}}}}] ->
        let values = cxtype |> Clang.get_type_declaration |>
          Clang.list_of_children |> List.map @@ fun cur ->
            Clang.get_cursor_spelling cur,
            Clang.get_enum_constant_decl_value cur in
        assert (values = ["A", 0; "B", 1; "C", 2]);
  | _ -> assert false
    ]}*)
  | Function of function_type
(** Function type.
    {[
let example = "int (*p)(void);"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { desc = Function {
        result = { desc = OtherType Int };
        args = Some { non_variadic = []; variadic = false}}}}}}] -> ()
  | _ -> assert false
    ]}*)
  | Record of string
(** Record type (either struct or union).

    The argument is the name and is the empty string for anonymous struct or
    union.
    {[
let example = "struct { int i; float f; } s;"

let () =
  match parse_declaration_list example with
  | [{ desc = Struct _ };
     { desc = Var { name = "s"; qual_type = { desc = Elaborated {
      keyword = Struct;
      named_type = { cxtype; desc = Record "" }}}}}] ->
        let fields = cxtype |> Clang.list_of_type_fields |>
          List.map @@ fun cur ->
            Clang.get_cursor_spelling cur,
            Clang.get_cursor_type cur |> Clang.Ast.of_cxtype in
        begin
          match fields with
          | ["i", { desc = OtherType Int };
             "f", { desc = OtherType Float }] -> ()
          | _ -> assert false
        end
  | _ -> assert false

let example = "union { int i; float f; } u;"

let () =
  match parse_declaration_list example with
  | [{ desc = Union _ };
     { desc = Var { name = "u"; qual_type = { desc = Elaborated {
      keyword = Union;
      named_type = { cxtype; desc = Record "" }}}}}] ->
        let fields = cxtype |> Clang.list_of_type_fields |>
          List.map @@ fun cur ->
            Clang.get_cursor_spelling cur,
            Clang.get_cursor_type cur |> Clang.Ast.of_cxtype in
        begin
          match fields with
          | ["i", { desc = OtherType Int };
             "f", { desc = OtherType Float }] -> ()
          | _ -> assert false
        end
  | _ -> assert false
    ]}*)
  | Typedef of string
(** Typedef type.
    {[
let example = "typedef struct { int i; float f; } struct_t; struct_t s;"

let () =
  match parse_declaration_list example with
  | [{ desc = Struct _ }; { desc = Typedef _ };
     { desc = Var { name = "s";
       qual_type = { cxtype; desc = Typedef "struct_t" }}}] ->
        let fields = cxtype |> Clang.get_type_declaration |>
          Clang.get_typedef_decl_underlying_type |> Clang.list_of_type_fields |>
          List.map @@ fun cur ->
            Clang.get_cursor_spelling cur,
            Clang.get_cursor_type cur |> Clang.Ast.of_cxtype in
        begin
          match fields with
          | ["i", { desc = OtherType Int };
             "f", { desc = OtherType Float }] -> ()
          | _ -> assert false
        end
  | _ -> assert false
    ]}*)
  | Paren of qual_type
(** Parenthesized type.

    Warning: parenthesized type only occurs with Clang <7.0.0 and when
    ~ignore_paren_in_types:false argument is passed to the AST converting
    function. From 7.0.0, Clang automatically passes through parentheses in
    types.

    {[
let example = "int (*p)(void);"

let () =
  match parse_declaration_list ~ignore_paren_in_types:false example with
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { desc = Function {
        result = { desc = OtherType Int };
        args = Some { non_variadic = []; variadic = false}}}}}}] ->
      assert (Clang.get_clang_version () >= "clang version 7.0.0")
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { desc = Paren { desc = Function {
        result = { desc = OtherType Int };
        args = Some { non_variadic = []; variadic = false}}}}}}}] ->
      assert (Clang.get_clang_version () < "clang version 7.0.0")
  | _ -> assert false
    ]}

*)
  | OtherType of cxtypekind
(** Other type.
    {[
(* TODO: stdbool.h not available
let example = "#include <stdbool.h>\nbool s;"

let () =
  match parse_declaration_list example |> List.rev |> List.hd with
  | { desc = Var { name = "s";
      qual_type = { desc = OtherType Bool}}} -> ()
  | _ -> assert false
*)
    ]}*)

(** Function type. *)
and function_type = {
  calling_conv : cxcallingconv;
(** Calling convention.
    {[
let example = "void f(void);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { calling_conv = C }}}] -> ()
    | _ -> assert false

let example = "__vectorcall void f(void);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { calling_conv = X86VectorCall }}}] -> ()
    | _ -> assert false
    ]}
 *)
  result : qual_type;
(** Result type.
    {[
let example = "void f(void);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { result = { desc = OtherType Void }}}}] -> ()
    | _ -> assert false

let example = "f(void);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { result = { desc = OtherType Int }}}}] -> ()
    | _ -> assert false
    ]}*)

  args : args option;
(** Argument types. [None] for K&R-style 'int foo()' function.
    {[
let example = "void f(void);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = [];
          variadic = false }}}}] -> ()
    | _ -> assert false

let example = "void f();"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = None }}}] -> ()
    | _ -> assert false

    ]}
 *)
}


(** Function arguments. *)
and args = {
  non_variadic : (string * qual_type) list;
(** Non-variadic arguments: the list gives for each argument its name and its
    type.

    For a function type which is not attached to an actual function declaration,
    all arguments have the empty name [""], since Clang does not keep argument
    names in function types.
    {[
let example = "void f(int i);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = ["i", { desc = OtherType Int }];
          variadic = false }}}}] -> ()
    | _ -> assert false

let example = "void f(int);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = ["", { desc = OtherType Int }];
          variadic = false }}}}] -> ()
    | _ -> assert false

let example = "typedef void (*f)(int x);"

let () =
    match parse_declaration_list example with
    | [{ desc = Typedef {
        name = "f"; 
        underlying_type = { desc =
          Pointer { desc = Function { args = Some {
            non_variadic = ["", { desc = OtherType Int }];
            variadic = false }}}}}}] -> ()
    | _ -> assert false
    ]}
 *)
  variadic : bool;
(** True if the function type is variadic.
    {[
let example = "void f(int i, ...);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = ["i", { desc = OtherType Int }];
          variadic = true }}}}] -> ()
    | _ -> assert false
    ]}
 *)
}

(** Statement.

The following example declares the function [parse_statement_list]
that returns the AST obtained from the parsing of [source] string as a
statement list (by putting it in the context of a function):
this function is used in the following examples to check the AST of
various types.
    {[
let parse_statement_list ?filename ?ignore_paren source =
  match
    Printf.sprintf "int f(void) { %s }" source |>
    parse_declaration_list ?filename ?ignore_paren
  with
  | [{ desc = Function { stmt = Some { desc = Compound items }}}] -> items
  | _ -> assert false
    ]}*)
and stmt = stmt_desc node
and stmt_desc =
  | Null
(** Null statement.
    {[
let example = ";"

let () =
  match parse_statement_list example with
  | [{ desc = Null }] -> ()
  | _ -> assert false
    ]}
    *)
  | Compound of stmt list
(** Compound statement.
    {[
let example = "{}"

let () =
  match parse_statement_list example with
  | [{ desc = Compound [] }] -> ()
  | _ -> assert false

let example = "{;;}"

let () =
  match parse_statement_list example with
  | [{ desc = Compound [{ desc = Null }; { desc = Null }] }] -> ()
  | _ -> assert false
    ]}
    *)
  | For of {
      init : stmt option; (** C++ *)
      condition_variable : var_decl option; (** C++ *)
      cond : stmt option;
      inc : stmt option;
      body : stmt;
    }
(** For statement.
    {[
let example = "for (;;) {}"

let () =
  match parse_statement_list example with
  | [{ desc = For {
      init = None;
      condition_variable = None;
      cond = None;
      inc = None;
      body = { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = "int i; for (i = 0; i < 4; i++) { i; }"

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = For {
      init = Some { desc = Expr (BinaryOperator {
        lhs = { desc = DeclRef "i"};
        kind = Assign;
        rhs = { desc = IntegerLiteral zero}})};
      condition_variable = None;
      cond = Some { desc = Expr (BinaryOperator {
        lhs = { desc = DeclRef "i"};
        kind = LT;
        rhs = { desc = IntegerLiteral four}})};
      inc = Some { desc = Expr (UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef "i"}})};
      body = { desc = Compound [{ desc =
        Expr (DeclRef "i")}] }}}] ->
      assert (Clang.int_of_cxint zero = 0);
      assert (Clang.int_of_cxint four = 4)
  | _ -> assert false

let example = "for (int i = 0; i < 4; i++) { i; }"

let () =
  match parse_statement_list example with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        name = "i";
        qual_type = { desc = OtherType Int};
        init = Some { desc = IntegerLiteral zero}}}] };
      condition_variable = None;
      cond = Some { desc = Expr (BinaryOperator {
        lhs = { desc = DeclRef "i"};
        kind = LT;
        rhs = { desc = IntegerLiteral four}})};
      inc = Some { desc = Expr (UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef "i"}})};
      body = { desc = Compound [{ desc =
        Expr (DeclRef "i")}] }}}] ->
      assert (Clang.int_of_cxint zero = 0);
      assert (Clang.int_of_cxint four = 4)
  | _ -> assert false

let example = "for (int i = 0; int j = i - 1; i--) { j; }"

let () =
  match parse_statement_list ~filename:"<string>.cpp" example with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        name = "i";
        qual_type = { desc = OtherType Int};
        init = Some { desc = IntegerLiteral zero}}}] };
      condition_variable = Some { desc = {
        name = "j";
        qual_type = { desc = OtherType Int};
        init = Some { desc = BinaryOperator {
          lhs = { desc = DeclRef "i"};
          kind = Sub;
          rhs = { desc = IntegerLiteral one}}}}};
      cond = Some { desc = Expr (DeclRef "j")};
      inc = Some { desc = Expr (UnaryOperator {
        kind = PostDec;
        operand = { desc = DeclRef "i"}})};
      body = { desc = Compound [{ desc =
        Expr (DeclRef "j")}] }}}] ->
      assert (Clang.int_of_cxint zero = 0);
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]}*)
  | If of {
      init : stmt option; (** C++17 *)
      condition_variable : var_decl option; (** C++ *)
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
(** If statement.
    {[
let example = "if (1) { 2; } else { 3; }"

let () =
  match parse_statement_list example with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral one };
       then_branch = { desc = Compound [{
         desc = Expr (IntegerLiteral two)}] };
       else_branch = Some { desc = Compound [{
         desc = Expr (IntegerLiteral three) }] }}}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint two = 2);
      assert (Clang.int_of_cxint three = 3)
  | _ -> assert false

let example = "if (1) { 2; }"

let () =
  match parse_statement_list example with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral one };
       then_branch = { desc = Compound [{
         desc = Expr (IntegerLiteral two)}] };
       else_branch = None }}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint two = 2)
  | _ -> assert false

let example = "if (int i = 1) { i; }"

let () =
  match parse_statement_list ~filename:"<string>.cpp" example with
  | [{ desc = If {
       init = None;
       condition_variable = Some ({ desc = {
         qual_type = { desc = OtherType Int};
         name = "i";
         init = Some { desc = IntegerLiteral one }}});
       cond = { desc = DeclRef "i"};
       then_branch = { desc = Compound [{
         desc = Expr (DeclRef "i")}] };
       else_branch = None }}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false

let example = "if (int i = 1; i) { i; }"

let () =
  match parse_statement_list ~filename:"<string>.cpp" example with
  | [{ desc = If {
       init = Some { desc = Decl [{ desc = Var {
         name = "i";
         qual_type = { desc = OtherType Int };
         init = Some { desc = IntegerLiteral one }}}] };
       condition_variable = None;
       then_branch = { desc = Compound [{
         desc = Expr (DeclRef "i")}] };
       else_branch = None }}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
   ]}*)
  | Switch of {
      init : stmt option; (** C++17 *)
      condition_variable : var_decl option; (** C++ *)
      cond : expr;
      body : stmt;
    }
(** Switch statement.
    {[
let example = "switch (1) { case 1: f(); break; case 2: break; default:;}"

let () =
  match parse_statement_list example with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral one };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          body = { desc =
            Expr (Call { f = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint one' = 1);
      assert (Clang.int_of_cxint two = 2)
  | _ -> assert false

let example =
  "switch (int i = 1) { case 1: f(); break; case 2: break; default:;}"

let () =
  match parse_statement_list ~filename:"<string>.cpp" example with
  | [{ desc = Switch {
      init = None;
      condition_variable = Some ({ desc = {
         qual_type = { desc = OtherType Int};
         name = "i";
         init = Some { desc = IntegerLiteral one }}});
      cond = { desc = DeclRef "i" };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          body = { desc =
            Expr (Call { f = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint one' = 1);
      assert (Clang.int_of_cxint two = 2)
  | _ -> assert false

let example =
  "switch (int i = 1; i) { case 1: f(); break; case 2: break; default:;}"

let () =
  match parse_statement_list ~filename:"<string>.cpp" example with
  | [{ desc = Switch {
      init = Some { desc = Decl [{ desc = Var {
         name = "i";
         qual_type = { desc = OtherType Int };
         init = Some { desc = IntegerLiteral one }}}] };
      condition_variable = None;
      cond = { desc = DeclRef "i" };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          body = { desc =
            Expr (Call { f = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint one' = 1);
      assert (Clang.int_of_cxint two = 2)
  | _ -> assert false
    ]}*)
  | Case of {
      lhs : expr;
      rhs : expr option; (** GNU extension: case ranges "case low ... high:" *)
      body : stmt;
    }
(** Case statement.

    Note that [body] only covers the first statement that follows. Other
    statements are attached to the parent.
    {[
let example = "switch (1) { case 1: f(); break; case 2 ... 3: break; default:;}"

let () =
  match parse_statement_list example with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral one };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          rhs = None;
          body = { desc =
            Expr (Call { f = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          rhs = Some { desc = IntegerLiteral three };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint one' = 1);
      assert (Clang.int_of_cxint two = 2);
      assert (Clang.int_of_cxint three = 3)
  | _ -> assert false
    ]}*)
  | Default of stmt
(** Default statement.

    Note that the parameter only covers the first statement that follows. Other
    statements are attached to the parent.
 *)
  | While of {
      condition_variable : var_decl option; (** C++ *)
      cond : expr;
      body : stmt;
    }
(** While statement.
    {[
let example = "while (1);"

let () =
  match parse_statement_list example with
  | [{ desc = While {
      condition_variable = None;
      cond = { desc = IntegerLiteral one };
      body = { desc = Null }}}] ->
      assert (Clang.int_of_cxint one = 1);
  | _ -> assert false

let example = "while (int i = 1) { i; }"

let () =
  match parse_statement_list ~filename:"<string>.cpp" example with
  | [{ desc = While {
      condition_variable = Some ({ desc = {
         qual_type = { desc = OtherType Int};
         name = "i";
         init = Some { desc = IntegerLiteral one }}});
      cond = { desc = DeclRef "i" };
      body = { desc = Compound [{ desc = Expr (DeclRef "i")}] }}}] ->
      assert (Clang.int_of_cxint one = 1);
  | _ -> assert false
    ]}*)
  | Do of {
      body : stmt;
      cond : expr;
    }
(** Do statement.
    {[
let example = "do; while (1);"

let () =
  match parse_statement_list example with
  | [{ desc = Do {
      body = { desc = Null };
      cond = { desc = IntegerLiteral one }}}] ->
      assert (Clang.int_of_cxint one = 1);
  | _ -> assert false

let example = "do { f(); } while (1);"

let () =
  match parse_statement_list example with
  | [{ desc = Do {
      body = { desc = Compound [{ desc =
        Expr (Call { f = { desc = DeclRef "f" }; args = [] })}] };
      cond = { desc = IntegerLiteral one }}}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]}*)
  | Label of {
      label : label_ref;
      body : stmt;
    }
(** Label statement.
    {[
let example = "label: 1; 2;"

let () =
  match parse_statement_list example with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (IntegerLiteral one)}}};
      { desc = Expr (IntegerLiteral two)}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint two = 2)
  | _ -> assert false
    ]}*)
  | Goto of label_ref
(** Goto statement.
    {[
let example = "label: 1; goto label;"

let () =
  match parse_statement_list example with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (IntegerLiteral one)}}};
      { desc = Goto "label" }] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]}*)
  | IndirectGoto of expr
(** Indirect goto statement (Labels as Values GNU extension).
    {[
let example = "label: 1; void *ptr = &&label; goto *ptr;"

let () =
  match parse_statement_list example with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (IntegerLiteral one)}}};
      { desc = Decl [{ desc = Var {
        name = "ptr";
        qual_type = { desc = Pointer { desc = OtherType Void }};
        init = Some { desc = AddrLabel "label" }}}] };
      { desc = IndirectGoto { desc = DeclRef "ptr"}}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]}*)
  | Continue
(** Continue statement.
    {[
let example = "for (;;) continue;"

let () =
  match parse_statement_list example with
  | [{ desc = For { body = { desc = Continue } }}] -> ()
  | _ -> assert false
   ]}*)
  | Break
(** Break statement.
    {[
let example = "for (;;) break;"

let () =
  match parse_statement_list example with
  | [{ desc = For { body = { desc = Break } }}] -> ()
  | _ -> assert false
   ]}*)
  | GCCAsm of string * string node list
(** GCC assembler statement.
    {[
let example = {|
  int src = 1;
  int dst;   

  asm ("mov %1, %0\n\t"
    "add $1, %0"
    : "=r" (dst) 
    : "r" (src));
|}

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Decl _ };
     { desc = GCCAsm (
       "mov %1, %0\n\tadd $1, %0",
       [{ desc = "dst" }; { desc = "src" }])}] -> ()
  | _ -> assert false
   ]}*)
  | MSAsm of string
(** MS assembler statement. *)
  | Return of expr
(** Return statement.
    {[
let example = "return 1;"

let () =
  match parse_statement_list example with
  | [{ desc = Return { desc = IntegerLiteral one }}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
   ]}*)
  | Decl of decl_stmt list
  | Expr of expr_desc

and expr = expr_desc node

and expr_desc =
  | IntegerLiteral of cxint
(** Integer literal.
    {[
let example = "0;"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (IntegerLiteral zero)}] ->
      assert (Clang.int_of_cxint zero = 0)
  | _ -> assert false
    ]}*)
  | FloatingLiteral of cxfloat
(** Floating literal.
    {[
let example = "0.5;"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (FloatingLiteral f)}] ->
    assert (Clang.ext_float_convert_to_double f = 0.5)
  | _ -> assert false
    ]}*)
  | StringLiteral of string
(** String literal.
    {[
let example = "\"Hello!\";"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (StringLiteral "Hello!")}] -> ()
  | _ -> assert false
    ]}*)
  | UnaryOperator of {
      kind : clang_ext_unaryoperatorkind;
      operand : expr;
    }
(** Unary operator.
    {[
let example = "+1;"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (UnaryOperator {
      kind = Plus;
      operand = { desc = IntegerLiteral one}})}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false

let example = "int x; &x;"

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (UnaryOperator {
      kind = AddrOf;
      operand = { desc = DeclRef "x" }})}] -> ()
  | _ -> assert false
    ]}*)
  | BinaryOperator of {
      lhs : expr;
      kind : clang_ext_binaryoperatorkind;
      rhs : expr;
    }
(** Binary operator.
    {[
let example = "1 + 2;"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (BinaryOperator {
      lhs = { desc = IntegerLiteral one};
      kind = Add;
      rhs = { desc = IntegerLiteral two}})}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint two = 2)
  | _ -> assert false
    ]} *)
  | DeclRef of string
(** Declaration reference.
    {[
let example = "int i; i;"

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (DeclRef "i")}] -> ()
  | _ -> assert false
    ]} *)
  | Call of {
      f : expr;
      args : expr list;
    }
(** Function call.
    {[
let example = "void g(int); g(1);"

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (Call {
      f = { desc = DeclRef "g" };
      args = [{ desc = IntegerLiteral one }] })}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]} *)
  | Cast of {
      kind : cast_kind;
      qual_type : qual_type;
      operand : expr;
    }
(** Cast.

    Implicit casts are removed in the AST unless ~ignore_implicit_cast:false is
    passed to the converting function.
    {[
let example = {| (void * ) "Hello"; |}

let () =
  match parse_statement_list example with
  | [{ desc = Expr (Cast {
      kind = CStyle;
      qual_type = { desc = Pointer _ };
      operand = { desc = StringLiteral "Hello" }})}] -> ()
  | _ -> assert false
    ]}*)
  | Member of {
      base : expr;
      arrow : bool;
      field : string node;
    }
(** Member dot or arrow
    {[
let example = {| struct s { int i } s; s.i = 0; |}

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (BinaryOperator {
      lhs = { desc = Member {
        base = { desc = DeclRef "s" };
        arrow = false;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral zero}})}] ->
      assert (Clang.int_of_cxint zero = 0)
  | _ -> assert false

let example = {| struct s { int i } *p; p->i = 0; |}

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (BinaryOperator {
      lhs = { desc = Member {
        base = { desc = DeclRef "p" };
        arrow = true;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral zero}})}] ->
      assert (Clang.int_of_cxint zero = 0)
  | _ -> assert false
    ]}*)
  | ArraySubscript of {
      lhs : expr;
      rhs : expr;
    }
(** Array subscript
    {[
let example = {| int a[1]; a[0] = 1; |}

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (BinaryOperator {
      lhs = { desc = ArraySubscript {
        lhs = { desc = DeclRef "a" };
        rhs = { desc = IntegerLiteral zero}}};
      kind = Assign;
      rhs = { desc = IntegerLiteral one}})}] ->
      assert (Clang.int_of_cxint zero = 0);
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]}*)
  | ConditionalOperator of {
      cond : expr;
      then_branch : expr option;
      else_branch : expr;
    }
(** Conditional operator.
    [None] in [else_branch] captures GNU "missing middle" extension.
    {[
let example = {| 1 ? 2 : 3; |}

let () =
  match parse_statement_list example with
  | [{ desc = Expr (ConditionalOperator {
      cond = { desc = IntegerLiteral one };
      then_branch = Some { desc = IntegerLiteral two };
      else_branch = { desc = IntegerLiteral three }})}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint two = 2);
      assert (Clang.int_of_cxint three = 3)
  | _ -> assert false

let example = {| 1 ? : 3; |}

let () =
  match parse_statement_list example with
  | [{ desc = Expr (ConditionalOperator {
      cond = { desc = IntegerLiteral one };
      then_branch = None;
      else_branch = { desc = IntegerLiteral three }})}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint three = 3)
  | _ -> assert false
    ]}*)
  | ParenExpr of expr
(** Parenthesed expression.

    Parenthesed expression are removed in the AST unless ~ignore_paren:false
    is passed to the converting function.
    {[
let example = {| (1); |}

let () =
  match parse_statement_list ~ignore_paren:false example with
  | [{ desc = Expr (ParenExpr ({ desc = IntegerLiteral one}))}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false

let () =
  match parse_statement_list example with
  | [{ desc = Expr (IntegerLiteral one)}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]}*)
  | AddrLabel of string
(** Label address (Labels as Values GNU extension)
    {[
let example = {| label: &&label; |}

let () =
  match parse_statement_list example with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (AddrLabel "label")}}}] ->
      ()
  | _ -> assert false
    ]}*)
  | UnexposedExpr of {
      s : string;
    }
  | OtherExpr

and decl_stmt = decl_stmt_desc node

and decl_stmt_desc =
  | Function of {
      linkage : cxlinkagekind;
      function_type : function_type;
      name : string;
      stmt : stmt option;
    }
(** Function definition or forward declaration.
    {[
let example = {| int f(void) {} |}

let () =
  match parse_declaration_list example with
  | [{ desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = OtherType Int};
        args = Some { non_variadic = []; variadic = false }};
      name = "f";
      stmt = Some { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = {| static int f(int x); |}

let () =
  match parse_declaration_list example with
  | [{ desc = Function {
      linkage = Internal;
      function_type = {
        calling_conv = C;
        result = { desc = OtherType Int};
        args = Some {
          non_variadic = [("x", { desc = OtherType Int})];
          variadic = false }};
      name = "f";
      stmt = None }}] -> ()
  | _ -> assert false
    ]}*)
  | Var of var_decl_desc
(** Variable declaration.
    {[
let example = {| int x = 1; |}

let () =
    match parse_declaration_list example with
  | [{ desc = Var {
      linkage = External;
      qual_type = { const = false; desc = OtherType Int };
      name = "x";
      init = Some ({ desc = IntegerLiteral one })}}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false

let example = {| const int x = 1; |}

let () =
    match parse_declaration_list example with
  | [{ desc = Var {
      linkage = External;
      qual_type = { const = true; desc = OtherType Int };
      name = "x";
      init = Some ({ desc = IntegerLiteral one })}}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false

let example = {| static int x = 1; |}

let () =
    match parse_declaration_list example with
  | [{ desc = Var {
      linkage = Internal;
      qual_type = { const = false; desc = OtherType Int };
      name = "x";
      init = Some ({ desc = IntegerLiteral one })}}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
    ]}*)
  | Enum of {
      name : string;
      constants : enum_constant list;
    }
(** Enum declaration.
    {[
let example = {| enum e { A, B = 2, C }; |}

let () =
  match parse_declaration_list example with
  | [{ desc = Enum {
      name = "e";
      constants = [
        { cxcursor = a; desc = { name = "A"; init = None }};
        { cxcursor = b; desc = {
          name = "B";
          init = Some { desc = IntegerLiteral two }}};
        { cxcursor = c; desc = { name = "C"; init = None }}] }}] ->
        assert (Clang.int_of_cxint two = 2);
        assert (Clang.get_enum_constant_decl_value a = 0);
        assert (Clang.get_enum_constant_decl_value b = 2);
        assert (Clang.get_enum_constant_decl_value c = 3)
  | _ -> assert false
    ]}*)
  | Struct of {
      name : string;
      fields : field_desc node list;
    }
(** Structure declaration.
    {[
let example = {| struct s { int i; float f; }; |}

let () =
  match parse_declaration_list example with
  | [{ desc = Struct {
      name = "s";
      fields = [
        { desc = { name = "i";
          qual_type = { desc = OtherType Int}}};
        { desc = { name = "f";
          qual_type = { desc = OtherType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| struct s { int a:1; int b:2; int c; }; |}

let () =
    match parse_declaration_list example with
  | [{ desc = Struct {
      name = "s";
      fields = [
        { cxcursor; desc = { name = "a";
          qual_type = { desc = OtherType Int};
          bitfield = Some { desc = IntegerLiteral one }}};
        { desc = { name = "b";
          qual_type = { desc = OtherType Int};
          bitfield = Some { desc = IntegerLiteral two }}};
        { desc = { name = "c";
          qual_type = { desc = OtherType Int};
          bitfield = None}}] }}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint two = 2);
      assert (Clang.get_field_decl_bit_width cxcursor = 1)
  | _ -> assert false
    ]}*)
  | Union of {
      name : string;
      fields : field_desc node list;
    }
(** Union declaration.
    {[
let example = {| union u { int i; float f; }; |}

let () =
  match parse_declaration_list example with
  | [{ desc = Union {
      name = "u";
      fields = [
        { desc = { name = "i";
          qual_type = { desc = OtherType Int}}};
        { desc = { name = "f";
          qual_type = { desc = OtherType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| union u { int a:1; int b:2; int c; }; |}

let () =
    match parse_declaration_list example with
  | [{ desc = Union {
      name = "u";
      fields = [
        { cxcursor; desc = { name = "a";
          qual_type = { desc = OtherType Int};
          bitfield = Some { desc = IntegerLiteral one }}};
        { desc = { name = "b";
          qual_type = { desc = OtherType Int};
          bitfield = Some { desc = IntegerLiteral two }}};
        { desc = { name = "c";
          qual_type = { desc = OtherType Int};
          bitfield = None}}] }}] ->
      assert (Clang.int_of_cxint one = 1);
      assert (Clang.int_of_cxint two = 2);
      assert (Clang.get_field_decl_bit_width cxcursor = 1)
  | _ -> assert false
    ]}*)
  | Typedef of {
      name : string;
      underlying_type : qual_type;
    }
(** Typedef declaration.

    Note that if the typedef declares a new underlying type,
    the declaration of the underlying type precedes the typedef
    declaration in the AST.
    {[
let example = {| typedef int int_t; |}

let () =
  match parse_declaration_list example with
  | [{ desc = Typedef {
      name = "int_t";
      underlying_type = { desc = OtherType Int }}}] -> ()
  | _ -> assert false

let example = {| typedef union u { int i; float f } u_t; |}

let () =
  match parse_declaration_list example with
  | [{ desc = Union {
        name = "u";
        fields = [
          { desc = { name = "i";
            qual_type = { desc = OtherType Int}}};
          { desc = { name = "f";
            qual_type = { desc = OtherType Float}}}] }};
      { desc = Typedef {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record "u" }}}}}] -> ()
  | _ -> assert false

let example = {| typedef union { int i; float f } u_t; |}

let () =
  match parse_declaration_list example with
  | [{ desc = Union {
        name = "";
        fields = [
          { desc = { name = "i";
            qual_type = { desc = OtherType Int}}};
          { desc = { name = "f";
            qual_type = { desc = OtherType Float}}}] }};
      { cxcursor; desc = Typedef {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record "" }}}}}] ->
        let fields = cxcursor |>
          Clang.get_typedef_decl_underlying_type |> Clang.list_of_type_fields |>
          List.map @@ fun cur ->
            Clang.get_cursor_spelling cur,
            Clang.get_cursor_type cur |> Clang.Ast.of_cxtype in
        begin
          match fields with
          | ["i", { desc = OtherType Int };
             "f", { desc = OtherType Float }] -> ()
          | _ -> assert false
        end
  | _ -> assert false
    ]}*)
  | OtherDecl

and field_desc = {
  name : string;
  qual_type : qual_type;
  bitfield : expr option;
}

and label_ref = string

and enum_constant = enum_constant_desc node

and enum_constant_desc = {
    name : string;
    init : expr option;
  }

and var_decl = var_decl_desc node

and var_decl_desc = {
    linkage : cxlinkagekind;
    name : string;
    qual_type : qual_type;
    init : expr option
  }

type translation_unit_desc = {
    filename : string; items : decl_stmt list
  }

type translation_unit = translation_unit_desc node
