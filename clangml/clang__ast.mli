[@@@ocaml.warning "-30"]

open Clang__bindings

type 'a node = {
    cxcursor : cxcursor;
    desc : 'a;
  }

(**
   {[
open Stdcompat

let () =
  prerr_endline (Clang.get_clang_version ())
   ]}

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
   ]}
*)

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
     ]}
*)
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
    ]}
*)
    restrict : bool;
(** [true] if the type is restrict-qualified.
    {[
let example = "int * restrict x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = true;
      desc = Pointer { pointee = {
        desc = OtherType Int}}}}}] -> ()
  | _ -> assert false

let example = "int * x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = false;
      desc = Pointer { pointee = {
        desc = OtherType Int}}}}}] -> ()
  | _ -> assert false
    ]}
*)
    desc : type_desc;
  }

(** Type description. *)
and type_desc =
  | Pointer of {
      pointee : qual_type;
    }
(** Pointer.
    {[
let example = "char *s;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "s"; qual_type = { desc = Pointer {
      pointee = { desc = OtherType Char_S }}}}}] -> ()
  | _ -> assert false
    ]} *)
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
    ]} *)
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
    ]} *)
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
    ]} *)
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
      named_type = { desc = Enum { name = "example" } }}}}}] -> ()
  | _ -> assert false
    ]} *)
  | Enum of {
      name : string;
    }
(** Enum type.
    {[
let example = "enum { A, B, C } e;"

let () =
  match parse_declaration_list example with
  | [{ desc = Enum _ };
     { desc = Var { name = "e"; qual_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { cxtype; desc = Enum { name = "" } }}}}}] ->
        let values = cxtype |> Clang.get_type_declaration |>
          Clang.list_of_children |> List.map @@ fun cur ->
            Clang.get_cursor_spelling cur,
            Clang.get_enum_constant_decl_value cur in
        assert (values = ["A", 0; "B", 1; "C", 2]);
  | _ -> assert false
    ]} *)
  | Function of function_type
(** Function type.
    {[
let example = "int (*p)(void);"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { pointee = { desc = Function {
        result = { desc = OtherType Int };
        args = Some { non_variadic = []; variadic = false};
    }}}}}}] -> ()
  | _ -> assert false
    ]} *)
  | Record of {
      name : string;
    }
(** Record type (either struct or union).
    {[
let example = "struct { int i; float f; } s;"

let () =
  match parse_declaration_list example with
  | [{ desc = Struct _ };
     { desc = Var { name = "s"; qual_type = { desc = Elaborated {
      keyword = Struct;
      named_type = { cxtype; desc = Record { name = "" } }}}}}] ->
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
      named_type = { cxtype; desc = Record { name = "" } }}}}}] ->
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
    ]} *)
  | Typedef of {
      name : string;
    }
(** Typedef type.
    {[
let example = "typedef struct { int i; float f; } struct_t; struct_t s;"

let () =
  match parse_declaration_list example with
  | [{ desc = Struct _ }; { desc = Typedef _ };
     { desc = Var { name = "s";
       qual_type = { cxtype; desc = Typedef { name = "struct_t" }}}}] ->
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
    ]} *)
  | Paren of qual_type
(** Parenthesized type.

    Warning: parenthesized type only occurs with Clang <7.0.0 and when
    ~ignore_paren_in_types:false argument is passed to the AST converting
    function. From 7.0.0, Clang automatically passes through type.

    {[
let example = "int (*p)(void);"

let () =
  match parse_declaration_list ~ignore_paren_in_types:false example with
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { pointee = { desc = Function {
        result = { desc = OtherType Int };
        args = Some { non_variadic = []; variadic = false};
    }}}}}}] ->
      assert (Clang.get_clang_version () >= "clang version 7.0.0")
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { pointee = { desc = Paren { desc = Function {
        result = { desc = OtherType Int };
        args = Some { non_variadic = []; variadic = false};
    }}}}}}}] ->
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
    ]} *)

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

let example = "__regcall void f(void);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { calling_conv = X86RegCall }}}] -> ()
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
    ]}
*)

  args : args option;
(** Argument types. [None] for K&R-style 'int foo()' function.
    {[
let example = "void f(void);"

let () =
    match parse_declaration_list example with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some { non_variadic = []; variadic = false }}}}] -> ()
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
(** Non-variadic arguments: the list gives for each argument its name and its type.
    For a function type which is not attached to an actual function declaration, all
    arguments have the empty name [""], since Clang does not keep argument names in
    function types.
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
          Pointer { pointee = { desc = Function { args = Some {
            non_variadic = ["", { desc = OtherType Int }];
            variadic = false }}}}}}}] -> ()
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
    ]} *)
and stmt = stmt_desc node
and stmt_desc =
  | Null
(** Null statement
    {[
let example = ";"

let () =
  match parse_statement_list example with
  | [{ desc = Null }] -> ()
  | _ -> assert false
    ]}
    *)
  | Compound of stmt list
(** Compound statement
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
      init : stmt option;
      condition_variable : var_decl option;
      cond : stmt option;
      inc : stmt option;
      body : stmt;
    }
(** For statement
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
    ]}
*)
  | If of {
      init : stmt option; (* TODO: How ? *)
      condition_variable : var_decl option;
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
(** If statement
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
         init = Some { desc = IntegerLiteral one }}});
       cond = { desc = DeclRef "i"};
       then_branch = { desc = Compound [{
         desc = Expr (DeclRef "i")}] };
       else_branch = None }}] ->
      assert (Clang.int_of_cxint one = 1)
  | _ -> assert false
   ]} *)
  | Switch of {
      init : stmt option;
      condition_variable : var_decl option;
      cond : expr;
      body : stmt;
    }
  | Case of {
      lhs : expr;
      rhs : expr option;
      body : stmt;
    }
  | Default of {
      body : stmt;
    }
  | While of {
      condition_variable : var_decl option;
      cond : expr;
      body : stmt;
    }
  | Do of {
      body : stmt;
      cond : expr;
    }
  | Label of {
      label : label_ref;
      body : stmt;
    }
  | Goto of {
      label : label_ref;
    }
  | IndirectGoto of {
      target : expr;
    }
  | Continue
  | Break
  | Asm
  | Decl of decl_stmt list
  | Return of {
      value : expr
    }
  | Expr of expr_desc

and expr = expr_desc node

and expr_desc =
  | IntegerLiteral of cxint
(** Integer literal
    {[
let example = "0;"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (IntegerLiteral zero)}] ->
      assert (Clang.int_of_cxint zero = 0)
  | _ -> assert false
    ]} *)
  | FloatingLiteral of cxfloat
(** Floating literal
    {[
let example = "0.5;"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (FloatingLiteral f)}] ->
    assert (Clang.ext_float_convert_to_double f = 0.5)
  | _ -> assert false
    ]} *)
  | StringLiteral of string
(** String literal
    {[
let example = "\"Hello!\";"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (StringLiteral "Hello!")}] -> ()
  | _ -> assert false
    ]} *)
  | UnaryOperator of {
      kind : clang_ext_unaryoperatorkind;
      operand : expr;
    }
(** Unary operator
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
    ]} *)
  | BinaryOperator of {
      lhs : expr;
      kind : clang_ext_binaryoperatorkind;
      rhs : expr;
    }
  | DeclRef of string
  | Call of {
      f : expr;
      args : expr list;
    }
  | Cast of {
      kind : cast_kind;
      qual_type : qual_type;
      operand : expr;
    }
(** Cast
    {[
let example = {| (void * ) "Hello"; |}

let () =
  match parse_statement_list example with
  | [{ desc = Expr (Cast {
      kind = CStyle;
      qual_type = { desc = Pointer _ };
      operand = { desc = StringLiteral "Hello" }})}] -> ()
  | _ -> assert false
    ]} *)
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
    ]} *)
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
    ]} *)
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
    ]} *)
  | ParenExpr of expr
(** Parenthesed expression
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
    ]} *)

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
    ]} *)
  | Var of var_decl_desc
  | Enum of {
      name : string;
      constants : enum_constant list;
    }
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
    ]} *)
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
    ]} *)

  | Typedef of {
      name : string;
      underlying_type : qual_type;
    }
  | OtherDecl

and field_desc = {
  name : string;
  qual_type : qual_type;
  bitfield : expr option;
}

and label_ref = label_ref_desc node

and label_ref_desc = {
    name : string;
  }

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
