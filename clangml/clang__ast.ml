[@@@ocaml.warning "-30"]

open Clang__bindings

(** Most of the AST nodes carry the [cxcursor] from where they come from
    in the Clang translation unit. *)

type source_location = cxsourcelocation

type 'qual_type open_decoration =
  | Cursor of cxcursor
  | Custom of {
      location : source_location option;
      qual_type : 'qual_type option;
    }

type ('a, 'qual_type) open_node = {
    decoration : 'qual_type open_decoration
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0]
      [@opaque];
    desc : 'a;
  }
  [@@deriving eq, ord, show]

type elaborated_type_keyword = clang_ext_elaboratedtypekeyword

and character_kind = clang_ext_characterkind

and unary_expr_kind = clang_ext_unaryexpr

and unary_operator_kind = clang_ext_unaryoperatorkind

and binary_operator_kind = clang_ext_binaryoperatorkind

and builtin_type = cxtypekind
  [@@deriving eq, ord, show]

type integer_literal =
  | Int of int
  | CXInt of cxint
  [@@deriving eq, ord]

type floating_literal =
  | Float of float
  | CXFloat of cxfloat
  [@@deriving eq, ord]

class ['self] base_iter =
  object (self)
    method visit_open_node : 'env 'a 'qual_type . ('env -> 'a -> unit) -> ('env -> 'qual_type -> unit) -> 'env -> ('a, 'qual_type) open_node -> unit =
      fun visit_desc _visit_qual_type env open_node -> visit_desc env open_node.desc

    method visit_integer_literal : 'env . 'env -> integer_literal -> unit =
      fun _env _ -> ()

    method visit_floating_literal : 'env . 'env -> floating_literal -> unit =
      fun _env _ -> ()

    method visit_elaborated_type_keyword : 'env . 'env -> elaborated_type_keyword -> unit =
      fun _env _ -> ()

    method visit_builtin_type : 'env . 'env -> builtin_type -> unit =
      fun _env _ -> ()

    method visit_cxcallingconv : 'env . 'env -> cxcallingconv -> unit =
      fun _env _ -> ()

    method visit_cxlinkagekind : 'env . 'env -> cxlinkagekind -> unit =
      fun _env _ -> ()

    method visit_character_kind : 'env . 'env -> character_kind -> unit =
      fun _env _ -> ()

    method visit_unary_expr_kind : 'env . 'env -> unary_expr_kind -> unit =
      fun _env _ -> ()

    method visit_unary_operator_kind : 'env . 'env -> unary_operator_kind -> unit =
      fun _env _ -> ()

    method visit_binary_operator_kind : 'env . 'env -> binary_operator_kind -> unit =
      fun _env _ -> ()
  end

class virtual ['self] base_reduce =
  object (self : 'self)
    inherit [_] VisitorsRuntime.reduce

    method visit_open_node : 'env 'a 'qual_type . ('env -> 'a -> 'monoid) -> ('env -> 'qual_type -> 'monoid) -> 'env -> ('a, 'qual_type) open_node -> 'monoid =
      fun visit_desc _visit_qual_type env open_node -> visit_desc env open_node.desc

    method visit_integer_literal : 'env . 'env -> integer_literal -> 'monoid =
      fun _env _ -> self#zero

    method visit_floating_literal : 'env . 'env -> floating_literal -> 'monoid =
      fun _env _ -> self#zero

    method visit_elaborated_type_keyword : 'env . 'env -> elaborated_type_keyword -> 'monoid =
      fun _env _ -> self#zero

    method visit_builtin_type : 'env . 'env -> builtin_type -> 'monoid =
      fun _env _ -> self#zero

    method visit_cxcallingconv : 'env . 'env -> cxcallingconv -> 'monoid =
      fun _env _ -> self#zero

    method visit_cxlinkagekind : 'env . 'env -> cxlinkagekind -> 'monoid =
      fun _env _ -> self#zero

    method visit_character_kind : 'env . 'env -> character_kind -> 'monoid =
      fun _env _ -> self#zero

    method visit_unary_expr_kind : 'env . 'env -> unary_expr_kind -> 'monoid =
      fun _env _ -> self#zero

    method visit_unary_operator_kind : 'env . 'env -> unary_operator_kind -> 'monoid =
      fun _env _ -> self#zero

    method visit_binary_operator_kind : 'env . 'env -> binary_operator_kind -> 'monoid =
      fun _env _ -> self#zero
  end

(*{[
open Stdcompat

let () =
  prerr_endline (Clang.get_clang_version ())

let success_count = ref 0 and failure_count = ref 0

let check pp ast checker =
  try
    checker ast;
    incr success_count
  with e ->
    Printf.eprintf "failed with: %s\n" (Printexc.to_string e);
    Format.eprintf "@[parsed@ as:@ @[%a@]@]@."
      (Format.pp_print_list pp) ast;
    incr failure_count
]}*)

(**
The following example declares the function [parse_declaration_list]
that returns the AST obtained from the parsing of [source] string as a
declaration list:
this function is used in the following examples to check the AST of
various programs.
    {[
let parse_declaration_list ?filename ?command_line_args ?options source =
  prerr_endline source;
  let ast =
    Clang.Ast.parse_string ?filename ?command_line_args ?options source in
  ast.desc.items
   ]}*)

(** Qualified type. *)
type qual_type = {
    cxtype : cxtype
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0]
      [@opaque];
    const : bool;
(** [true] if the type is const-qualified.
      {[
let example = "const int one = 1;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "one";
      qual_type = {
        const = true;
        desc = BuiltinType Int};
      init = Some { desc = IntegerLiteral one }}}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let example = "int x;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "x";
      qual_type = {
        const = false;
        desc = BuiltinType Int};
      init = None }}] -> ()
  | _ -> assert false
     ]}*)
    volatile : bool;
(** [true] if the type is volatile-qualified.
    {[
let example = "volatile int x;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "x";
      qual_type = {
        volatile = true;
        desc = BuiltinType Int}}}] -> ()
  | _ -> assert false

let example = "int x;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "x";
      qual_type = {
        volatile = false;
        desc = BuiltinType Int}}}] -> ()
  | _ -> assert false
    ]}*)
    restrict : bool;
(** [true] if the type is restrict-qualified.
    {[
let example = "int * restrict x;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = true;
      desc = Pointer { desc = BuiltinType Int }}}}] -> ()
  | _ -> assert false

let example = "int * x;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "x"; qual_type = {
      restrict = false;
      desc = Pointer { desc = BuiltinType Int }}}}] -> ()
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
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "s"; qual_type = { desc =
      Pointer { desc = BuiltinType Char_S }}}}] -> ()
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
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "s"; qual_type = { desc = ConstantArray {
      element = { desc = BuiltinType Char_S };
      size = 42 }}}}] -> ()
  | _ -> assert false
    ]}*)
  | IncompleteArray of qual_type
(** Incomplete array.
    {[
let example = "struct s { int i; char array[]; };"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Struct { name = "s"; fields = [
      { desc = Named { name = "i"; qual_type = { desc = BuiltinType Int}}};
      { desc = Named { name = "array"; qual_type = { desc =
        IncompleteArray { desc = BuiltinType Char_S }}}}] }}] -> ()
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
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Function { name = "f"; function_type =
      { result = { desc = BuiltinType Void };
        args = Some {
          non_variadic = [
            ("i", { desc = BuiltinType Int });
            ("array", { desc = VariableArray {
               element = { desc = BuiltinType Char_S };
               size = { desc = DeclRef "i" }}})];
          variadic = false }}}}] -> ()
  | _ -> assert false
    ]}*)
  | Elaborated of {
      keyword : elaborated_type_keyword;
      named_type : qual_type;
    }
(** Elaborated type.
    {[
let example = "enum example { A, B, C }; enum example e;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = EnumDecl _ };
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
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = EnumDecl _ };
     { desc = Var { name = "e"; qual_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { desc = Enum "" } as named_type }}}}] ->
        let values =
          match Clang.Type.get_declaration named_type with
          | { desc = EnumDecl { constants }} ->
              constants |> List.map @@ fun (constant : Clang.Ast.enum_constant) ->
                constant.desc.name,
                Clang.Enum_constant.get_value constant
          | _ -> assert false in
        assert (values = ["A", 0; "B", 1; "C", 2]);
  | _ -> assert false
    ]}*)
  | FunctionType of function_type
(** Function type.
    {[
let example = "int (*p)(void);"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
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
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Struct _ };
     { desc = Var { name = "s"; qual_type = { desc = Elaborated {
      keyword = Struct;
      named_type = { desc = Record "" } as named_type }}}}] ->
        let fields = named_type |> Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Named {
                  name = "i";
                  qual_type = { desc = BuiltinType Int }}};
              { desc = Named {
                  name = "f";
                  qual_type = { desc = BuiltinType Float }}}] -> ()
          | _ -> assert false
        end
  | _ -> assert false

let example = "union { int i; float f; } u;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Union _ };
     { desc = Var { name = "u"; qual_type = { desc = Elaborated {
      keyword = Union;
      named_type = { desc = Record "" } as named_type }}}}] ->
        let fields = named_type |> Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Named {
                  name = "i";
                  qual_type = { desc = BuiltinType Int }}};
              { desc = Named {
                  name = "f";
                  qual_type = { desc = BuiltinType Float }}}] -> ()
          | _ -> assert false
        end
  | _ -> assert false
    ]}*)
  | Typedef of string
(** Typedef type.
    {[
let example = "typedef struct { int i; float f; } struct_t; struct_t s;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Struct _ }; { desc = TypedefDecl _ };
     { desc = Var { name = "s";
       qual_type = { desc = Typedef "struct_t" } as qual_type }}] ->
        let fields = qual_type |>
          Clang.Type.get_typedef_underlying_type |>
          Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Named {
                  name = "i";
                  qual_type = { desc = BuiltinType Int }}};
              { desc = Named {
                  name = "f";
                  qual_type = { desc = BuiltinType Float }}}] -> ()
          | _ -> assert false
        end
  | _ -> assert false
    ]}*)
  | Complex of qual_type
(** Complex number type (C99).

    {[
let example = "double _Complex c;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { name = "c";
      qual_type = { desc = Complex { desc = BuiltinType Double }}}} -> ()
  | _ -> assert false

let example = "float _Complex c;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { name = "c";
      qual_type = { desc = Complex { desc = BuiltinType Float }}}} -> ()
  | _ -> assert false
    ]} *)
  | ParenType of qual_type
(** Parenthesized type.

    Warning: parenthesized type only occurs with Clang <7.0.0 and when
    ~ignore_paren_in_types:false argument is passed to the AST converting
    function. From 7.0.0, Clang automatically passes through parentheses in
    types.

    {[
let example = "int (*p)(void);"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example
    ~options:(Clang.Ast.Options.make ~ignore_paren_in_types:false ())) @@
  fun ast -> match ast with
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        args = Some { non_variadic = []; variadic = false}}}}}}] ->
      assert (Clang.get_clang_version () >= "clang version 7.0.0")
  | [{ desc = Var { name = "p"; qual_type = { desc =
      Pointer { desc = ParenType { desc = FunctionType {
        result = { desc = BuiltinType Int };
        args = Some { non_variadic = []; variadic = false}}}}}}}] ->
      assert (Clang.get_clang_version () < "clang version 7.0.0")
  | _ -> assert false
    ]}

*)
  | BuiltinType of builtin_type
(** Built-in type.
    {[
let example = "_Bool s;"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { name = "s";
      qual_type = { desc = BuiltinType Bool}}} -> ()
  | _ -> assert false
    ]}*)
(** Function type. *)
and function_type = {
  calling_conv : cxcallingconv;
(** Calling convention.
    {[
let example = "void f(void);"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = Function {
        name = "f"; 
        function_type = { calling_conv = C }}}] -> ()
    | _ -> assert false

let example = "__vectorcall void f(void);"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
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
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = Function {
        name = "f"; 
        function_type = { result = { desc = BuiltinType Void }}}}] -> ()
    | _ -> assert false

let example = "f(void);"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = Function {
        name = "f"; 
        function_type = { result = { desc = BuiltinType Int }}}}] -> ()
    | _ -> assert false
    ]}*)

  args : args option;
(** Argument types. [None] for K&R-style 'int foo()' function.
    {[
let example = "void f(void);"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = [];
          variadic = false }}}}] -> ()
    | _ -> assert false

let example = "void f();"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
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
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = ["i", { desc = BuiltinType Int }];
          variadic = false }}}}] -> ()
    | _ -> assert false

let example = "void f(int);"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = ["", { desc = BuiltinType Int }];
          variadic = false }}}}] -> ()
    | _ -> assert false

let example = "typedef void (*f)(int x);"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = TypedefDecl {
        name = "f"; 
        underlying_type = { desc =
          Pointer { desc = FunctionType { args = Some {
            non_variadic = ["", { desc = BuiltinType Int }];
            variadic = false }}}}}}] -> ()
    | _ -> assert false
    ]}
 *)
  variadic : bool;
(** True if the function type is variadic.
    {[
let example = "void f(int i, ...);"

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
    | [{ desc = Function {
        name = "f"; 
        function_type = { args = Some {
          non_variadic = ["i", { desc = BuiltinType Int }];
          variadic = true }}}}] -> ()
    | _ -> assert false
    ]}
 *)
}
(** Statement.

The following example declares the function [parse_statement_list]
that returns the AST obtained from the parsing of [source] string as a
statement list (by putting it in the context of a function):
this function is used in the following examples to check Clang.Ast.pp_decl the AST of
various types.
    {[
let parse_statement_list ?(return_type = "int") ?filename ?command_line_args ?options source =
  match
    Printf.sprintf "%s f(void) { %s }" return_type source |>
    parse_declaration_list ?filename ?command_line_args ?options
  with
  | [{ desc = Function { stmt = Some { desc = Compound items }}}] -> items
  | _ -> assert false
    ]}*)
and stmt = (stmt_desc, qual_type) open_node
and stmt_desc =
  | Null
(** Null statement.
    {[
let example = ";"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Null }] -> ()
  | _ -> assert false
    ]}
    *)
  | Compound of stmt list
(** Compound statement.
    {[
let example = "{}"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Compound [] }] -> ()
  | _ -> assert false

let example = "{;;}"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = For {
      init = None;
      condition_variable = None;
      cond = None;
      inc = None;
      body = { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = "int i; for (i = 0; i < 4; i++) { i; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
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
      assert (Clang.Ast.int_of_literal zero = 0);
      assert (Clang.Ast.int_of_literal four = 4)
  | _ -> assert false

let example = "for (int i = 0; i < 4; i++) { i; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        name = "i";
        qual_type = { desc = BuiltinType Int};
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
      assert (Clang.Ast.int_of_literal zero = 0);
      assert (Clang.Ast.int_of_literal four = 4)
  | _ -> assert false

let example = "for (int i = 0; int j = i - 1; i--) { j; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example) @@ fun ast -> match ast with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        name = "i";
        qual_type = { desc = BuiltinType Int};
        init = Some { desc = IntegerLiteral zero}}}] };
      condition_variable = Some { desc = {
        name = "j";
        qual_type = { desc = BuiltinType Int};
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
      assert (Clang.Ast.int_of_literal zero = 0);
      assert (Clang.Ast.int_of_literal one = 1)
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral one };
       then_branch = { desc = Compound [{
         desc = Expr (IntegerLiteral two)}] };
       else_branch = Some { desc = Compound [{
         desc = Expr (IntegerLiteral three) }] }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2);
      assert (Clang.Ast.int_of_literal three = 3)
  | _ -> assert false

let example = "if (1) { 2; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral one };
       then_branch = { desc = Compound [{
         desc = Expr (IntegerLiteral two)}] };
       else_branch = None }}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2)
  | _ -> assert false

let example = "if (int i = 1) { i; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example) @@ fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = Some ({ desc = {
         qual_type = { desc = BuiltinType Int};
         name = "i";
         init = Some { desc = IntegerLiteral one }}});
       cond = { desc = DeclRef "i"};
       then_branch = { desc = Compound [{
         desc = Expr (DeclRef "i")}] };
       else_branch = None }}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let example = "if (int i = 1; i) { i; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example) @@ fun ast -> match ast with
  | [{ desc = If {
       init = Some { desc = Decl [{ desc = Var {
         name = "i";
         qual_type = { desc = BuiltinType Int };
         init = Some { desc = IntegerLiteral one }}}] };
       condition_variable = None;
       then_branch = { desc = Compound [{
         desc = Expr (DeclRef "i")}] };
       else_branch = None }}] ->
      assert (Clang.Ast.int_of_literal one = 1)
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral one };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          body = { desc =
            Expr (Call { callee = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal one' = 1);
      assert (Clang.Ast.int_of_literal two = 2)
  | _ -> assert false

let example =
  "switch (int i = 1) { case 1: f(); break; case 2: break; default:;}"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example) @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = Some ({ desc = {
         qual_type = { desc = BuiltinType Int};
         name = "i";
         init = Some { desc = IntegerLiteral one }}});
      cond = { desc = DeclRef "i" };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          body = { desc =
            Expr (Call { callee = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal one' = 1);
      assert (Clang.Ast.int_of_literal two = 2)
  | _ -> assert false

let example =
  "switch (int i = 1; i) { case 1: f(); break; case 2: break; default:;}"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example) @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = Some { desc = Decl [{ desc = Var {
         name = "i";
         qual_type = { desc = BuiltinType Int };
         init = Some { desc = IntegerLiteral one }}}] };
      condition_variable = None;
      cond = { desc = DeclRef "i" };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          body = { desc =
            Expr (Call { callee = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal one' = 1);
      assert (Clang.Ast.int_of_literal two = 2)
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral one };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral one' };
          rhs = None;
          body = { desc =
            Expr (Call { callee = { desc = DeclRef "f" }; args = [] })}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral two };
          rhs = Some { desc = IntegerLiteral three };
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal one' = 1);
      assert (Clang.Ast.int_of_literal two = 2);
      assert (Clang.Ast.int_of_literal three = 3)
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = While {
      condition_variable = None;
      cond = { desc = IntegerLiteral one };
      body = { desc = Null }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
  | _ -> assert false

let example = "while (int i = 1) { i; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example) @@ fun ast -> match ast with
  | [{ desc = While {
      condition_variable = Some ({ desc = {
         qual_type = { desc = BuiltinType Int};
         name = "i";
         init = Some { desc = IntegerLiteral one }}});
      cond = { desc = DeclRef "i" };
      body = { desc = Compound [{ desc = Expr (DeclRef "i")}] }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Do {
      body = { desc = Null };
      cond = { desc = IntegerLiteral one }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
  | _ -> assert false

let example = "do { f(); } while (1);"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Do {
      body = { desc = Compound [{ desc =
        Expr (Call { callee = { desc = DeclRef "f" }; args = [] })}] };
      cond = { desc = IntegerLiteral one }}}] ->
      assert (Clang.Ast.int_of_literal one = 1)
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (IntegerLiteral one)}}};
      { desc = Expr (IntegerLiteral two)}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2)
  | _ -> assert false
    ]}*)
  | Goto of label_ref
(** Goto statement.
    {[
let example = "label: 1; goto label;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (IntegerLiteral one)}}};
      { desc = Goto "label" }] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false
    ]}*)
  | IndirectGoto of expr
(** Indirect goto statement (Labels as Values GNU extension).
    {[
let example = "label: 1; void *ptr = &&label; goto *ptr;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (IntegerLiteral one)}}};
      { desc = Decl [{ desc = Var {
        name = "ptr";
        qual_type = { desc = Pointer { desc = BuiltinType Void }};
        init = Some { desc = AddrLabel "label" }}}] };
      { desc = IndirectGoto { desc = DeclRef "ptr"}}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false
    ]}*)
  | Continue
(** Continue statement.
    {[
let example = "for (;;) continue;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = For { body = { desc = Continue } }}] -> ()
  | _ -> assert false
   ]}*)
  | Break
(** Break statement.
    {[
let example = "for (;;) break;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = For { body = { desc = Break } }}] -> ()
  | _ -> assert false
   ]}*)
  | GCCAsm of string * (string, qual_type) open_node list
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Decl _ };
     { desc = GCCAsm (
       "mov %1, %0\n\tadd $1, %0",
       [{ desc = "dst" }; { desc = "src" }])}] -> ()
  | _ -> assert false
   ]}*)
  | MSAsm of string
(** MS assembler statement. *)
  | Return of expr option
(** Return statement.
    {[
let example = "return;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~return_type:"void" example) @@
  fun ast -> match ast with
  | [{ desc = Return None }] -> ()
  | _ -> assert false

let example = "return 1;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Return (Some { desc = IntegerLiteral one })}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false
   ]}*)
  | Decl of decl list
  | Expr of expr_desc

and expr = (expr_desc, qual_type) open_node

and expr_desc =
  | IntegerLiteral of integer_literal
        [@printer fun fmt i ->
          let s =
            match i with
            | Int i -> string_of_int i
            | CXInt i -> ext_int_to_string i 10 true in
          fprintf fmt "%s" s]
(** Integer literal.
    {[
let example = "0;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (IntegerLiteral zero)}] ->
      assert (Clang.Ast.int_of_literal zero = 0)
  | _ -> assert false
    ]}*)
  | FloatingLiteral of floating_literal
        [@printer fun fmt f ->
          let s =
            match f with
            | Float f -> string_of_float f
            | CXFloat f -> ext_float_to_string f in
          fprintf fmt "%s" s]
(** Floating literal.
    {[
let example = "0.5;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (FloatingLiteral f)}] ->
    assert (Clang.Ast.float_of_literal f = 0.5)
  | _ -> assert false
    ]}*)
  | StringLiteral of string
(** String literal.
    {[
let example = "\"Hello!\";"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (StringLiteral "Hello!")}] -> ()
  | _ -> assert false
    ]}*)
  | CharacterLiteral of {
      kind : character_kind;
      value : int;
    }
(** Character literal.
    {[
let example = "'a';"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (CharacterLiteral { kind = Ascii; value = 0x61 })}] -> ()
  | _ -> assert false

let example = "L'a';"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (CharacterLiteral { kind = Wide; value = 0x61 })}] -> ()
  | _ -> assert false

let example = "u8'a';"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp"
      ~command_line_args:["-std=c++1z"] example) @@
  fun ast -> match ast with
  | [{ desc = Expr (CharacterLiteral { kind = UTF8; value = 0x61 })}] -> ()
  | _ -> assert false

let example = "u'a';"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (CharacterLiteral { kind = UTF16; value = 0x61 })}] -> ()
  | _ -> assert false
    ]}*)
  | ImaginaryLiteral of expr
(** Imaginary literal.
    {[
let example = "1i;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (ImaginaryLiteral { desc = IntegerLiteral one })}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let example = "2.5i;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (ImaginaryLiteral { desc = FloatingLiteral x })}] ->
      assert (Clang.Ast.float_of_literal x = 2.5)
  | _ -> assert false
    ]}*)
  | UnaryOperator of {
      kind : unary_operator_kind;
      operand : expr;
    }
(** Unary operator.
    {[
let example = "+1;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (UnaryOperator {
      kind = Plus;
      operand = { desc = IntegerLiteral one}})}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let example = "int x; &x;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr (UnaryOperator {
      kind = AddrOf;
      operand = { desc = DeclRef "x" }})}] -> ()
  | _ -> assert false
    ]}*)
  | BinaryOperator of {
      lhs : expr;
      kind : binary_operator_kind;
      rhs : expr;
    }
(** Binary operator.
    {[
let example = "1 + 2;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (BinaryOperator {
      lhs = { desc = IntegerLiteral one};
      kind = Add;
      rhs = { desc = IntegerLiteral two}})}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2)
  | _ -> assert false
    ]} *)
  | DeclRef of string
(** Declaration reference.
    {[
let example = "int i; i;"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr (DeclRef "i")}] -> ()
  | _ -> assert false
    ]} *)
  | Call of {
      callee : expr;
      args : expr list;
    }
(** Function call.
    {[
let example = "void g(int); g(1);"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr (Call {
      callee = { desc = DeclRef "g" };
      args = [{ desc = IntegerLiteral one }] })}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false
    ]} *)
  | Cast of {
      kind : cast_kind;
      qual_type : qual_type;
      operand : expr;
    }
(** Cast.

    {[
let example = {| (void * ) "Hello"; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (Cast {
      kind = CStyle;
      qual_type = { desc = Pointer _ };
      operand = { desc = StringLiteral "Hello" }})}] -> ()
  | _ -> assert false
    ]}

    Implicit casts are removed in the AST unless [~ignore_implicit_cast:false] is
    passed to the converting function.

    {[
let example = {| int i; i; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example
    ~options:(Clang.Ast.Options.make ~ignore_implicit_cast:false ())) @@
  fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr (Cast {
      kind = Implicit;
      qual_type = { desc = BuiltinType Int };
      operand = { desc = DeclRef "i" }})}] -> ()
  | _ -> assert false
    ]}
*)
  | Member of {
      base : expr;
      arrow : bool;
      field : (string, qual_type) open_node;
    }
(** Member dot or arrow
    {[
let example = {| struct s { int i } s; s.i = 0; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr (BinaryOperator {
      lhs = { desc = Member {
        base = { desc = DeclRef "s" };
        arrow = false;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral zero}})}] ->
      assert (Clang.Ast.int_of_literal zero = 0)
  | _ -> assert false

let example = {| struct s { int i } *p; p->i = 0; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr (BinaryOperator {
      lhs = { desc = Member {
        base = { desc = DeclRef "p" };
        arrow = true;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral zero}})}] ->
      assert (Clang.Ast.int_of_literal zero = 0)
  | _ -> assert false
    ]}*)
  | ArraySubscript of {
      base : expr;
      index : expr;
    }
(** Array subscript
    {[
let example = {| int a[1]; a[0] = 1; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr (BinaryOperator {
      lhs = { desc = ArraySubscript {
        base = { desc = DeclRef "a" };
        index = { desc = IntegerLiteral zero}}};
      kind = Assign;
      rhs = { desc = IntegerLiteral one}})}] ->
      assert (Clang.Ast.int_of_literal zero = 0);
      assert (Clang.Ast.int_of_literal one = 1)
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
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (ConditionalOperator {
      cond = { desc = IntegerLiteral one };
      then_branch = Some { desc = IntegerLiteral two };
      else_branch = { desc = IntegerLiteral three }})}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2);
      assert (Clang.Ast.int_of_literal three = 3)
  | _ -> assert false

let example = {| 1 ? : 3; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (ConditionalOperator {
      cond = { desc = IntegerLiteral one };
      then_branch = None;
      else_branch = { desc = IntegerLiteral three }})}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal three = 3)
  | _ -> assert false
    ]}*)
  | Paren of expr
(** Parenthesed expression.

    Parenthesed expression are removed in the AST unless ~ignore_paren:false
    is passed to the converting function.
    {[
let example = {| (1); |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example
    ~options:(Clang.Ast.Options.make ~ignore_paren:false ())) @@
  fun ast -> match ast with
  | [{ desc = Expr (Paren ({ desc = IntegerLiteral one}))}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (IntegerLiteral one)}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let example = {| int i; sizeof(i); |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example
    ~options:(Clang.Ast.Options.make ~ignore_paren:false ())) @@
  fun ast -> match ast with
  | [ { desc = Decl _ };
      { desc = Expr (UnaryExpr {
          kind = SizeOf;
          argument = ArgumentExpr { desc = Paren { desc = DeclRef "i" }}})}] ->
      ()
  | _ -> assert false
    ]}*)
  | AddrLabel of string
(** Label address (Labels as Values GNU extension).
    {[
let example = {| label: &&label; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr (AddrLabel "label")}}}] ->
      ()
  | _ -> assert false
    ]}*)
  | InitList of expr list
(** Initialization list.
    {[
let example = {| int a[2] = { 1, 2 }; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var { name = "a"; qual_type = {
      desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 2 }};
      init = Some { desc = InitList [
        { desc = IntegerLiteral one };
        { desc = IntegerLiteral two }] }}}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2)
  | _ -> assert false
    ]}*)
  | CompoundLiteral of {
      qual_type : qual_type;
      init : expr
    }
(** Compound literal [C99 6.5.2.5].
    {[
let example = {| (int []) { 1, 2 }; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [{ desc = Expr (CompoundLiteral {
      qual_type = { desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 2 }};
      init = { desc = InitList [
        { desc = IntegerLiteral one };
        { desc = IntegerLiteral two }] }})}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2)
  | _ -> assert false
    ]}*)
  | UnaryExpr of {
      kind : unary_expr_kind;
      argument : unary_expr_or_type_trait;
    }
(** Unary expr: sizeof, alignof (C++11), ...
    {[

let example = {| int i; sizeof(i); |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [ { desc = Decl _ };
      { desc = Expr (UnaryExpr {
          kind = SizeOf;
          argument = ArgumentExpr { desc = DeclRef "i" }})}] -> ()
  | _ -> assert false

let example = {| sizeof(int); |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list example) @@ fun ast -> match ast with
  | [ { desc = Expr (UnaryExpr {
          kind = SizeOf;
          argument = ArgumentType { desc = BuiltinType Int }})}] -> ()
  | _ -> assert false

let example = {| alignof(int); |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example
    ~command_line_args:["-std=c++11"]) @@
  fun ast -> match ast with
  | [ { desc = Expr (UnaryExpr {
          kind = AlignOf;
          argument = ArgumentType { desc = BuiltinType Int }})}] -> ()
  | _ -> assert false
    ]}

    From Clang>=6.0.0, [alignof] is available by default with C++.

    {[
let () =
  if Clang.get_clang_version () >= "clang version 6.0.0" then
    check Clang.Ast.pp_stmt (parse_statement_list ~filename:"<string>.cpp" example) @@ fun ast -> match ast with
    | [ { desc = Expr (UnaryExpr {
            kind = AlignOf;
            argument = ArgumentType { desc = BuiltinType Int }})}] -> ()
    | _ -> assert false
    ]}
*)
  | UnexposedExpr of {
      s : string;
    }
  | OtherExpr

and cast_kind =
  | CStyle
  | Implicit

and unary_expr_or_type_trait =
  | ArgumentExpr of expr
  | ArgumentType of qual_type

and decl = (decl_desc, qual_type) open_node

and decl_desc =
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
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Int};
        args = Some { non_variadic = []; variadic = false }};
      name = "f";
      stmt = Some { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = {| static int f(int x); |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Function {
      linkage = Internal;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Int};
        args = Some {
          non_variadic = [("x", { desc = BuiltinType Int})];
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
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var {
      linkage = External;
      qual_type = { const = false; desc = BuiltinType Int };
      name = "x";
      init = Some ({ desc = IntegerLiteral one })}}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let example = {| const int x = 1; |}

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var {
      linkage = External;
      qual_type = { const = true; desc = BuiltinType Int };
      name = "x";
      init = Some ({ desc = IntegerLiteral one })}}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false

let example = {| static int x = 1; |}

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Var {
      linkage = Internal;
      qual_type = { const = false; desc = BuiltinType Int };
      name = "x";
      init = Some ({ desc = IntegerLiteral one })}}] ->
      assert (Clang.Ast.int_of_literal one = 1)
  | _ -> assert false
    ]}*)
  | EnumDecl of {
      name : string;
      constants : enum_constant list;
    }
(** Enum declaration.
    {[
let example = {| enum e { A, B = 2, C }; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = EnumDecl {
      name = "e";
      constants = [
        { desc = { name = "A"; init = None }} as a;
        { desc = {
          name = "B";
          init = Some { desc = IntegerLiteral two }}} as b;
        { desc = { name = "C"; init = None }} as c] }}] ->
        assert (Clang.Ast.int_of_literal two = 2);
        assert (Clang.Enum_constant.get_value a = 0);
        assert (Clang.Enum_constant.get_value b = 2);
        assert (Clang.Enum_constant.get_value c = 3)
  | _ -> assert false
    ]}*)
  | Struct of {
      name : string;
      fields : field list;
    }
(** Structure declaration.
    {[
let example = {| struct s { int i; float f; }; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Struct {
      name = "s";
      fields = [
        { desc = Named { name = "i";
          qual_type = { desc = BuiltinType Int}}};
        { desc = Named { name = "f";
          qual_type = { desc = BuiltinType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| struct s { int a:1; int b:2; int c; }; |}

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Struct {
      name = "s";
      fields = [
        { desc = Named { name = "a";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral one }}} as a;
        { desc = Named { name = "b";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral two }}};
        { desc = Named { name = "c";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None}}] }}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2);
      assert (Clang.Field.get_bit_width a = 1)
  | _ -> assert false
    ]}*)
  | Union of {
      name : string;
      fields : field list;
    }
(** Union declaration.
    {[
let example = {| union u { int i; float f; }; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Union {
      name = "u";
      fields = [
        { desc = Named { name = "i";
          qual_type = { desc = BuiltinType Int}}};
        { desc = Named { name = "f";
          qual_type = { desc = BuiltinType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| union u { int a:1; int b:2; int c; }; |}

let () =
    check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Union {
      name = "u";
      fields = [
        { desc = Named { name = "a";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral one }}} as a;
        { desc = Named { name = "b";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral two }}};
        { desc = Named { name = "c";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None}}] }}] ->
      assert (Clang.Ast.int_of_literal one = 1);
      assert (Clang.Ast.int_of_literal two = 2);
      assert (Clang.Field.get_bit_width a = 1)
  | _ -> assert false
    ]}*)
  | TypedefDecl of {
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
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = TypedefDecl {
      name = "int_t";
      underlying_type = { desc = BuiltinType Int }}}] -> ()
  | _ -> assert false

let example = {| typedef union u { int i; float f } u_t; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Union {
        name = "u";
        fields = [
          { desc = Named { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Named { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }};
      { desc = TypedefDecl {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record "u" }}}}}] -> ()
  | _ -> assert false

let example = {| typedef union { int i; float f } u_t; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Union {
        name = "";
        fields = [
          { desc = Named {
              name = "i";
              qual_type = { desc = BuiltinType Int}}};
          { desc = Named {
              name = "f";
              qual_type = { desc = BuiltinType Float}}}] }};
      { desc = TypedefDecl {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record "" }}} as underlying_type }}] ->
        let fields = underlying_type |> Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Named {
                  name = "i";
                  qual_type = { desc = BuiltinType Int}}};
              { desc = Named {
                  name = "f";
                  qual_type = { desc = BuiltinType Float}}}] -> ()
          | _ -> assert false
        end
  | _ -> assert false
    ]}*)
  | OtherDecl

and field = (field_desc, qual_type) open_node

and field_desc =
  | Named of  {
      name : string;
      qual_type : qual_type;
      bitwidth : expr option;
    }
  | AnonymousUnion of field list
(** Anonymous union
    {[
let example = {| struct s { int label; union { int i; float f; };}; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Struct {
      name = "s";
      fields = [
        { desc = Named { name = "label";
          qual_type = { desc = BuiltinType Int}}};
        { desc = AnonymousUnion [
          { desc = Named { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Named { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }] }}] -> ()
  | _ -> assert false
    ]}*)
  | AnonymousStruct of field list
(** Anonymous struct
    {[
let example = {| union s { int single; struct { int i; float f; };}; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list example) @@ fun ast -> match ast with
  | [{ desc = Union {
      name = "s";
      fields = [
        { desc = Named { name = "single";
          qual_type = { desc = BuiltinType Int}}};
        { desc = AnonymousStruct [
          { desc = Named { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Named { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }] }}] -> ()
  | _ -> assert false
    ]}*)

and label_ref = string

and enum_constant = (enum_constant_desc, qual_type) open_node

and enum_constant_desc = {
    name : string;
    init : expr option;
  }

and var_decl = (var_decl_desc, qual_type) open_node

and var_decl_desc = {
    linkage : cxlinkagekind;
    name : string;
    qual_type : qual_type;
    init : expr option
  }
    [@@deriving show, eq, ord,
      visitors { variety = "iter"; ancestors = ["base_iter"] },
      visitors { variety = "reduce"; ancestors = ["base_reduce"] }]

type decoration = qual_type open_decoration

type 'a node = ('a, qual_type) open_node

type translation_unit_desc = {
    filename : string; items : decl list
  }

type translation_unit = translation_unit_desc node

(*{[
let () =
  Printf.eprintf "%d success%s and %d failure%s.\n"
    !success_count (if !success_count > 1 then "es" else "")
    !failure_count (if !failure_count > 1 then "s" else "");
  if !failure_count > 0 then
    exit 1
 ]}*)
