[@@@ocaml.warning "-30"]

open Clang__bindings

type 'a node = {
    cxcursor : cxcursor;
    desc : 'a;
  }

(**
The following example declares the function [parse_declaration_list]
that returns the AST obtained from the parsing of [source] string as a
declaration list:
this function is used in the following examples to check the AST of
various programs.
    {[
open Stdcompat

let parse_declaration_list ?filename source =
   prerr_endline source;
  (Clang.parse_string ?filename source |> Result.get_ok |>
    Clang.Ast.of_cxtranslationunit).desc.items
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
let example = "const int one = 1;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "one"; qual_type = {
      const = true;
      desc = OtherType { kind = Int }}}}] -> ()
  | _ -> assert false

let example = "int x = 1;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "one"; qual_type = {
      const = false;
      desc = OtherType { kind = Int }}}}] -> ()
  | _ -> assert false
*)
    volatile : bool;
(** [true] if the type is volatile-qualified.
let example = "volatile int x = 1;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      volatile = true;
      desc = OtherType { kind = Int }}}}] -> ()
  | _ -> assert false

let example = "int x = 1;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      volatile = false;
      desc = OtherType { kind = Int }}}}] -> ()
  | _ -> assert false
*)
    restrict : bool;
(** [true] if the type is restrict-qualified.
let example = "int * restrict x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = true;
      desc = Pointer { desc = OtherType { kind = Int }}}}}] -> ()
  | _ -> assert false

let example = "int * x;"

let () =
  match parse_declaration_list example with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = false;
      desc = OtherType { kind = Int } }}}] -> ()
  | _ -> assert false
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
      pointee = { desc = OtherType { kind = Char_S }}}}}}] -> ()
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
      element = { desc = OtherType { kind = Char_S } };
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
      "i", { desc = OtherType { kind = Int }};
      "array", { desc = IncompleteArray {
        element = { desc = OtherType { kind = Char_S }}}}] }}] -> ()
  | _ -> assert false
    ]} *)
  | VariableArray of {
      element : qual_type;
      size : expr;
    }
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
        result = { desc = OtherType { kind = Int } };
        args = Some { non_variadic = []; variadic = false};
    }}}}}}] -> ()
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { pointee = { desc = OtherType { kind = Unexposed }}}}}}] ->
      prerr_endline "warning: unexposed function type (< 7.0.0?)"
  | _ -> assert false
    ]} *)
  | Struct of {
      name : string;
    }
(** Struct type.
    {[
let example = "struct { int i; float f; } s;"

let () =
  match parse_declaration_list example with
  | [{ desc = Struct _ };
     { desc = Var { name = "s"; qual_type = { desc = Elaborated {
      keyword = Struct;
      named_type = { cxtype; desc = Struct { name = "" } }}}}}] ->
        let fields = cxtype |> Clang.list_of_type_fields |>
          List.map @@ fun cur ->
            Clang.get_cursor_spelling cur,
            Clang.get_cursor_type cur |> Clang.Ast.of_cxtype in
        begin
          match fields with
          | ["i", { desc = OtherType { kind = Int } };
             "f", { desc = OtherType { kind = Float } }] -> ()
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
          | ["i", { desc = OtherType { kind = Int } };
             "f", { desc = OtherType { kind = Float } }] -> ()
          | _ -> assert false
        end
  | _ -> assert false
    ]} *)
  | OtherType of {
      kind : cxtypekind;
    }
(** Other type.
    {[
(* TODO: stdbool.h not available
let example = "#include <stdbool.h>\nbool s;"

let () =
  match parse_declaration_list example |> List.rev |> List.hd with
  | { desc = Var { name = "s";
      qual_type = { desc = OtherType { kind = Bool }}}} -> ()
  | _ -> assert false
*)
    ]} *)

and function_type = {
  calling_conv : cxcallingconv;
  result : qual_type;
  args : args option;
}

and args = {
  non_variadic : (string * qual_type) list;
  variadic : bool;
}

(** Statement.

The following example declares the function [parse_statement_list]
that returns the AST obtained from the parsing of [source] string as a
statement list (by putting it in the context of a function):
this function is used in the following examples to check the AST of
various types.
    {[
let parse_statement_list ?filename source =
  match
    Printf.sprintf "int f(void) { %s }" source |>
    parse_declaration_list ?filename
  with
  | [{ desc = Function { stmt = Some { desc = Compound { items }}}}] -> items
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
  | Compound of {
      items : stmt list;
    }
(** Compound statement
    {[
let example = "{}"

let () =
  match parse_statement_list example with
  | [{ desc = Compound { items = [] }}] -> ()
  | _ -> assert false

let example = "{;;}"

let () =
  match parse_statement_list example with
  | [{ desc = Compound { items = [{ desc = Null }; { desc = Null }] }}] -> ()
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
      body = { desc = Compound { items = [] }}}}] -> ()
  | _ -> assert false

let example = "int i; for (i = 0; i < 4; i++) { i; }"

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = For {
      init = Some { desc = Expr (BinaryOperator {
        lhs = { desc = DeclRef { s = "i" }};
        kind = Assign;
        rhs = { desc = IntegerLiteral { s = "0" }}})};
      condition_variable = None;
      cond = Some { desc = Expr (BinaryOperator {
        lhs = { desc = DeclRef { s = "i" }};
        kind = LT;
        rhs = { desc = IntegerLiteral { s = "4" }}})};
      inc = Some { desc = Expr (UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef { s = "i" }}})};
      body = { desc = Compound { items = [{ desc =
        Expr (DeclRef { s = "i" })}] }}}}] -> ()
  | _ -> assert false

let example = "for (int i = 0; i < 4; i++) { i; }"

let () =
  match parse_statement_list example with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        name = "i";
        qual_type = { desc = OtherType { kind = Int }};
        init = Some { desc = IntegerLiteral { s = "0" }}}}] };
      condition_variable = None;
      cond = Some { desc = Expr (BinaryOperator {
        lhs = { desc =
          (* DeclRef is exposed since 7.0.0 *)
          (UnexposedExpr { s = "i" } | DeclRef { s = "i" })};
        kind = LT;
        rhs = { desc = IntegerLiteral { s = "4" }}})};
      inc = Some { desc = Expr (UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef { s = "i" }}})};
      body = { desc = Compound { items = [{ desc =
        Expr (DeclRef { s = "i" })}] }}}}] -> ()
  | _ -> assert false

let example = "for (int i = 0; int j = i - 1; i--) { j; }"

let () =
  match parse_statement_list ~filename:"<string>.cpp" example with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        name = "i";
        qual_type = { desc = OtherType { kind = Int }};
        init = Some { desc = IntegerLiteral { s = "0" }}}}] };
      condition_variable = Some { desc = {
        name = "j";
        qual_type = { desc = OtherType { kind = Int }};
        init = Some { desc = BinaryOperator {
          lhs = { desc =
            (* DeclRef is exposed since 7.0.0 *)
            (UnexposedExpr { s = "i" } | DeclRef { s = "i" })};
          kind = Sub;
          rhs = { desc = IntegerLiteral { s = "1" }}}}}};
      cond = Some { desc = Expr (
        (* DeclRef is exposed since 7.0.0 *)
        (UnexposedExpr { s = "j" } | DeclRef { s = "j" }))};
      inc = Some { desc = Expr (UnaryOperator {
        kind = PostDec;
        operand = { desc = DeclRef { s = "i" }}})};
      body = { desc = Compound { items = [{ desc =
        Expr (UnexposedExpr { s = "j" } | DeclRef { s = "j" })}] }}}}] -> ()
  | _ -> assert false
    ]}
*)
  | If of {
      init : stmt option;
      condition_variable : var_decl option;
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
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
  | IntegerLiteral of {
      s : string;
    }
(** Integer literal
    {[
let example = "0;"

let () =
  match parse_statement_list example with
  | [{ desc = Expr (IntegerLiteral { s = "0" })}] -> ()
  | _ -> assert false
    ]} *)
  | StringLiteral of string
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
      operand = { desc = IntegerLiteral { s = "1" }}})}] -> ()
  | _ -> assert false

let example = "int x; &x;"

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (UnaryOperator {
      kind = AddrOf;
      operand = { desc = DeclRef { s = "x" }}})}] -> ()
  | _ -> assert false
    ]} *)
  | BinaryOperator of {
      lhs : expr;
      kind : clang_ext_binaryoperatorkind;
      rhs : expr;
    }
  | DeclRef of {
      s : string;
    }
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
        base = { desc = DeclRef { s = "s" }};
        arrow = false;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral { s = "0" }}})}] -> ()
  | _ -> assert false

let example = {| struct s { int i } *p; p->i = 0; |}

let () =
  match parse_statement_list example with
  | [{ desc = Decl _ }; { desc = Expr (BinaryOperator {
      lhs = { desc = Member {
        base = { desc = DeclRef { s = "p" }};
        arrow = true;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral { s = "0" }}})}] -> ()
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
        lhs = { desc = DeclRef { s = "a" }};
        rhs = { desc = IntegerLiteral { s = "0" }}}};
      kind = Assign;
      rhs = { desc = IntegerLiteral { s = "1" }}})}] -> ()
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
        result = { desc = OtherType { kind = Int }};
        args = Some { non_variadic = []; variadic = false }};
      name = "f";
      stmt = Some { desc = Compound { items = [] }}}}] -> ()
  | _ -> assert false

let example = {| static int f(int x); |}

let () =
  match parse_declaration_list example with
  | [{ desc = Function {
      linkage = Internal;
      function_type = {
        calling_conv = C;
        result = { desc = OtherType { kind = Int }};
        args = Some {
          non_variadic = [("x", { desc = OtherType { kind = Int }})];
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
      fields : (string * qual_type) list;
    }
  | Typedef of {
      name : string;
      underlying_type : qual_type;
    }
  | OtherDecl

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
