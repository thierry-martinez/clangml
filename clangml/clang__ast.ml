open Clang__bindings

let pp_cxint fmt cxint =
  Format.pp_print_string fmt (ext_int_to_string cxint 10 true)

let pp_cxfloat fmt cxfloat =
  Format.pp_print_string fmt (ext_float_to_string cxfloat)

(** {2 Abstractions from libclang's types} *)

(** The following types describe locations and literals that can be either
    produced by libclang or constructed from OCaml values to allow OCaml
    programs to construct parts of AST (for
    instance, to apply a transformation to the AST). *)

(** A {!type:source_location} can either be an internal
    {!type:cxsourcelocation} from libclang or a
    {!type:concrete_location}.
    {!type:concrete_location} can be obtained from
    {!type:source_location} with {!val:Clang.Ast.get_presumed_location}
    or {!val:Clang.Ast.get_expansion_location}: these functions are
    identical for {!type:source_location} constructed from
    {!type:concrete_location}, and call {!val:Clang.get_presumed_location}
    and {!val:Clang.get_expansion_location} for libclang's locations.
 *)
type concrete_location = {
  filename : string;
  line : int;
  column : int
  }

type source_location =
  | Clang of cxsourcelocation
  | Concrete of concrete_location

(*{[
[@@@ocaml.warning "-30"]
[@@@ocaml.warning "-9"]

open Stdcompat

let () =
  prerr_endline (Clang.get_clang_version ())

let success_count = ref 0 and failure_count = ref 0

let check pp parser source checker =
  prerr_endline source;
  let ast = parser source in
  try
    checker ast;
    incr success_count
  with e ->
    Printf.eprintf "failed with: %s\n" (Printexc.to_string e);
    Format.eprintf "@[parsed@ as:@ @[%a@]@]@."
      (Format.pp_print_list pp) ast;
    incr failure_count

let lift_expr = new Clangml_lift.lift_expr Location.none

let quote_decl_list l = lift_expr#list lift_expr#decl l

let check_pattern ?(result = fun _ -> ()) quoter parser source pattern =
  prerr_endline source;
  let ast = parser source in
  if
    match pattern ?quoted:(Some (quoter ast)) ast with
    | Ok bindings ->
        begin try
          result bindings;
          true
        with e ->
          Printf.eprintf "failed with: %s\n" (Printexc.to_string e);
          false
        end
    | Error failure ->
        Format.printf "@[failed:@ %a@]@." Pattern_runtime.format_failure
          failure;
        false
  then
    incr success_count
  else
    incr failure_count
]}*)

(** {2 Nodes and decorations} *)

(** AST nodes are of type ['a ]{!type:node} for some ['a] and
    carry a {!type:decoration}.
    If the node comes for a translation unit parsed by clang,
    the decoration is of the form {!const:Cursor}[ cursor],
    where [cursor] points to the corresponding node in clang
    internal AST.
    Decorations
    can be of the form {!const:Custom}[ custom_decoration],
    where the inlined record [custom_decoration] may optionnally
    carry a location, or a type, or both.

    To break type recursion between {!type:qual_type} and {!type:decoration},
    open types ['qual_type ]{!type:open_decoration} and
    [('a, 'qual_type) ]{!type:open_node} are defined first, and then
    {!type:node} and {!type:decoration} are defined as aliases
    with ['qual_type = ]{!type:qual_type}.

    Breaking recursion allows [visitors] to derive polymorphic
    visitors for [open_node] while deriving monomorphic visitors
    for the concrete AST nodes themselves.
*)

type 'qual_type open_decoration =
  | Cursor of (cxcursor [@opaque]
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0])
  | Custom of {
      location : (source_location option [@opaque]
        [@equal fun _ _ -> true]
        [@compare fun _ _ -> 0]);
      qual_type : 'qual_type option;
    }

and ('a, 'qual_type) open_node = {
    decoration : 'qual_type open_decoration
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0];
    desc : 'a;
  }
    [@@deriving show, eq, ord]

(** {2 Aliases} *)

(** The following aliases provide more readable names for some types
from libclang. *)

type elaborated_type_keyword = clang_ext_elaboratedtypekeyword
(** Keyword associated to an elaborated type: [struct], [union],
    [enum], ... *)

and character_kind = clang_ext_characterkind
(** Character kind: ASCII, UTF8, UTF16, ... *)

and unary_expr_kind = clang_ext_unaryexpr
(** Kind of unary expression: [sizeof], [alignof], ... *)

and unary_operator_kind = clang_ext_unaryoperatorkind
(** Kind of unary operator: [_++], [++_], [-_], [&_], ... *)

and binary_operator_kind = clang_ext_binaryoperatorkind
(** Kind of binary operator: [_+_], [_=_], [_+=_], [_<<_], ... *)

and attribute_kind = clang_ext_attrkind
(** Kind of attribute: [FallThrough], [NonNull], ... *)

and builtin_type = cxtypekind
(** libclang's type kinds: [Int], [Void], [Bool], ... *)

and cxx_access_specifier = cx_cxxaccessspecifier
(** C++ access specifier: [public], [private], [protected] *)

and calling_conv = cxcallingconv
(** Calling convention *)

and linkage_kind = cxlinkagekind

and integer_literal =
  | Int of int
  | CXInt of cxint

and floating_literal =
  | Float of float
  | CXFloat of cxfloat

and languages = {
    c : bool;
    cxx : bool;
  }

(** {2 Types and nodes} *)

(**
The following example declares the function [parse_declaration_list]
that returns the AST obtained from the parsing of [source] string as a
declaration list:
this function is used in the following examples to check the AST of
various programs.
    {[
let parse_declaration_list ?filename ?command_line_args ?language ?options
    ?clang_options source =
  let ast =
    Clang.Ast.parse_string ?filename ?command_line_args ?language ?options
      ?clang_options source in (*
  let tu = Clang.Ast.cursor_of_node ast |> Clang.cursor_get_translation_unit in
  Clang.seq_of_diagnostics tu |> Seq.iter (fun diagnostics ->
    prerr_endline (Clang.format_diagnostic diagnostics
      Clang.Cxdiagnosticdisplayoptions.display_source_location));
  assert (not (Clang.has_warning_or_error tu)); *)
  ast.desc.items
   ]}*)

(** {3 Qualified types } *)

and qual_type = {
    cxtype : (cxtype [@quote.opaque])
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0]
      [@opaque];
    const : bool;
(** [true] if the type is const-qualified.
      {[
let example = "const int one = 1;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "one";
      var_type = {
        const = true;
        desc = BuiltinType Int};
      var_init = Some { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = "int x;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x";
      var_type = {
        const = false;
        desc = BuiltinType Int};
      var_init = None }}] -> ()
  | _ -> assert false
     ]}*)
    volatile : bool;
(** [true] if the type is volatile-qualified.
    {[
let example = "volatile int x;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x";
      var_type = {
        volatile = true;
        desc = BuiltinType Int}}}] -> ()
  | _ -> assert false

let example = "int x;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x";
      var_type = {
        volatile = false;
        desc = BuiltinType Int}}}] -> ()
  | _ -> assert false
    ]}*)
    restrict : bool;
(** [true] if the type is restrict-qualified.
    {[
let example = "int * restrict x;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x"; var_type = {
      restrict = true;
      desc = Pointer { desc = BuiltinType Int }}}}] -> ()
  | _ -> assert false

let example = "int * x;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x"; var_type = {
      restrict = false;
      desc = Pointer { desc = BuiltinType Int }}}}] -> ()
  | _ -> assert false
    ]}*)
    desc : type_desc;
  }

and type_desc =
  | Pointer of qual_type
(** Pointer.
    {[
let example = "char *s;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "s"; var_type = { desc =
      Pointer { desc = BuiltinType Char_S }}}}] -> ()
  | _ -> assert false
    ]}*)
  | LValueReference of qual_type
  | ConstantArray of {
      element : qual_type;
      size : int;
    }
(** Constant-sized array.
    {[
let example = "char s[42];"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "s"; var_type = { desc = ConstantArray {
      element = { desc = BuiltinType Char_S };
      size = 42 }}}}] -> ()
  | _ -> assert false
    ]}*)
  | Vector of {
      element : qual_type;
      size : int;
    }
(** Vector.
    {[
let example = {|
  #include <stdint.h>
  int32_t __attribute__((vector_size(16))) v;
    |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match List.rev ast with
  | { desc = Var { var_name = "v"; var_type = { desc = Vector {
      element = { desc = Typedef (Ident "int32_t") };
      size = 4 }}}} :: _ -> ()
  | _ -> assert false
    ]}*)
  | IncompleteArray of qual_type
(** Incomplete array.
    {[
let example = "struct s { int i; char array[]; };"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl { keyword = Struct; name = "s"; fields = [
      { desc = Field { name = "i"; qual_type = { desc = BuiltinType Int}}};
      { desc = Field { name = "array"; qual_type = { desc =
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
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function { name = "f"; function_type =
      { result = { desc = BuiltinType Void };
        parameters = Some {
          non_variadic = [
            { desc = { name = "i"; qual_type = { desc = BuiltinType Int }}};
            { desc = { name = "array"; qual_type = { desc = VariableArray {
               element = { desc = BuiltinType Char_S };
               size = { desc = DeclRef (Ident "i") }}}}}];
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
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = EnumDecl _ };
     { desc = Var { var_name = "e"; var_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { desc = Enum (Ident "example") }}}}}] -> ()
  | _ -> assert false
    ]}*)
  | Enum of ident_ref
(** Enum type.
    {[
let example = "enum { A, B, C } e;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = EnumDecl _ };
     { desc = Var { var_name = "e"; var_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { desc = Enum (Ident "") } as named_type }}}}] ->
        let values =
          match Clang.Type.get_declaration named_type with
          | { desc = EnumDecl { constants }} ->
              constants |> List.map @@
              fun (constant : Clang.Ast.enum_constant) ->
                constant.desc.constant_name,
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
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "p"; var_type = { desc =
      Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some { non_variadic = []; variadic = false}}}}}}] -> ()
  | _ -> assert false
    ]}*)
  | Record of ident_ref
(** Record type (either struct or union).

    The argument is the name and is the empty string for anonymous struct or
    union.
    {[
let example = "struct { int i; float f; } s;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl { keyword = Struct }};
     { desc = Var { var_name = "s"; var_type = { desc = Elaborated {
      keyword = Struct;
      named_type = { desc = Record (Ident "") } as named_type }}}}] ->
        let fields = named_type |> Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Field {
                  name = "i";
                  qual_type = { desc = BuiltinType Int }}};
              { desc = Field {
                  name = "f";
                  qual_type = { desc = BuiltinType Float }}}] -> ()
          | _ ->
              Format.eprintf "%a@." (Format.pp_print_list
                ~pp_sep:Format.pp_print_newline
                Clang.Ast.pp_decl) fields;
              assert false
        end
  | _ -> assert false

let example = "union { int i; float f; } u;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl { keyword = Union }};
     { desc = Var { var_name = "u"; var_type = { desc = Elaborated {
      keyword = Union;
      named_type = { desc = Record (Ident "") } as named_type }}}}] ->
        let fields = named_type |> Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Field {
                  name = "i";
                  qual_type = { desc = BuiltinType Int }}};
              { desc = Field {
                  name = "f";
                  qual_type = { desc = BuiltinType Float }}}] -> ()
          | _ ->
              Format.eprintf "%a@." (Format.pp_print_list
                ~pp_sep:Format.pp_print_newline
                Clang.Ast.pp_decl) fields;
              assert false
        end
  | _ -> assert false
    ]}*)
  | Typedef of ident_ref
(** Typedef type.
    {[
let example = "typedef struct { int i; float f; } struct_t; struct_t s;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl { keyword = Struct }}; { desc = TypedefDecl _ };
     { desc = Var { var_name = "s";
       var_type = { desc = Typedef (Ident "struct_t") } as var_type }}] ->
        let fields = var_type |>
          Clang.Type.get_typedef_underlying_type |>
          Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Field {
                  name = "i";
                  qual_type = { desc = BuiltinType Int }}};
              { desc = Field {
                  name = "f";
                  qual_type = { desc = BuiltinType Float }}}] -> ()
          | _ ->
              Format.eprintf "%a@." (Format.pp_print_list
                ~pp_sep:Format.pp_print_newline
                Clang.Ast.pp_decl) fields;
              assert false
        end
  | _ -> assert false
    ]}*)
  | Complex of qual_type
(** Complex number type (C99).

    {[
let example = "double _Complex c;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { var_name = "c";
      var_type = { desc = Complex { desc = BuiltinType Double }}}} -> ()
  | _ -> assert false

let example = "float _Complex c;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { var_name = "c";
      var_type = { desc = Complex { desc = BuiltinType Float }}}} -> ()
  | _ -> assert false
    ]} *)
  | Attributed of {
      modified_type : qual_type;
      attribute_kind : attribute_kind;
    }
(** Attributed type.

    Attributed types are only visible with Clang >=8.0.0 when
    [Cxtranslationunit_flags.include_attributed_types] is set.
    Otherwise, the type is directly substituted by its modified type.

    {[
let example = "int * _Nonnull ptr;"

let () =
  if Clang.get_clang_version () >= "clang version 8.0.0" then
    let clang_options = Clang.Cxtranslationunit_flags.(
      Clang.default_editing_translation_unit_options ()
      + Clang.include_attributed_types) in
    check Clang.Ast.pp_decl (parse_declaration_list ~clang_options) example @@
    fun ast ->  match ast with
    | [{ desc = Var { var_name = "ptr";
         var_type = { desc = Attributed {
           modified_type = { desc = Pointer { desc = BuiltinType Int }};
           attribute_kind }}}}] ->
             assert (attribute_kind = Clang.type_non_null)
    | _ -> assert false
    ]} *)
  | ParenType of qual_type
(** Parenthesized type.

    Warning: parenthesized type only occurs with Clang <7.0.0 and when
    [~ignore_paren_in_types:false] argument is passed to the AST converting
    function. From 7.0.0, Clang automatically passes through parentheses in
    types.

    {[
let example = "int (*p)(void);"

let () =
  check Clang.Ast.pp_decl (parse_declaration_list
    ~options:(Clang.Ast.Options.make ~ignore_paren_in_types:false ()))
    example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "p"; var_type = { desc =
      Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some { non_variadic = []; variadic = false}}}}}}] ->
      assert (Clang.get_clang_version () >= "clang version 7.0.0")
  | [{ desc = Var { var_name = "p"; var_type = { desc =
      Pointer { desc = ParenType { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some { non_variadic = []; variadic = false}}}}}}}] ->
      assert (Clang.get_clang_version () < "clang version 7.0.0")
  | _ -> assert false
    ]}

*)
  | TemplateTypeParm of string
  | TemplateSpecialization of {
      name : template_name;
      arguments : template_argument list;
    }
  | BuiltinType of builtin_type
(** Built-in type.
    {[
let example = "_Bool s;"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { var_name = "s";
      var_type = { desc = BuiltinType Bool}}} -> ()
  | _ -> assert false
    ]}*)
  | Auto
(** Auto type. (C++11)
    {[
let example = "auto i = 1;"

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Var {
      var_name = "i";
      var_type = { desc = Auto };
      var_init = Some { desc = IntegerLiteral (Int 1)}}}]]
    ]} *)
  | UnexposedType of clang_ext_typekind
  | InvalidType

and template_name =
  | NameTemplate of decl
  | OverloadedTemplate
  | QualifiedTemplate
  | DependentTemplate
  | SubstTemplateTemplateParm
  | SubstTemplateTemplateParmPack
  | InvalidNameKind

and template_argument =
  | Type of qual_type
  | ArgumentDecl of decl
  | NullPtr of qual_type
  | TemplateTemplateArgument of template_name
  | TemplateExpansion of template_name
  | Integral of { value : integer_literal; qual_type : qual_type }
  | NonTypeTemplateArgument of qual_type
  | ExprTemplateArgument of expr

(** Function type. *)
and function_type = {
  calling_conv : calling_conv;
(** Calling convention.
    {[
let example = "void f(void);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { calling_conv = C }}}] -> ()
  | _ -> assert false

let example = {| __attribute((pcs("aapcs"))) void f(void); |}

let () =
  check Clang.Ast.pp_decl
    (parse_declaration_list ~command_line_args:["-target"; "arm"])
    example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { calling_conv = AAPCS }}}] ->
      assert (
        Clang.get_clang_version () < "clang version 3.8.0" ||
        Clang.get_clang_version () >= "clang version 3.9.0")
  | [{ desc = Function {
      name = "f";
      function_type = { calling_conv = C }}}] ->
      assert (
        Clang.get_clang_version () >= "clang version 3.8.0" &&
        Clang.get_clang_version () < "clang version 3.9.0")
  | _ -> assert false
    ]}
 *)
  result : qual_type;
(** Result type.
    {[
let example = "void f(void);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { result = { desc = BuiltinType Void }}}}] -> ()
  | _ -> assert false

let example = "f(void);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { result = { desc = BuiltinType Int }}}}] -> ()
  | _ -> assert false
    ]}*)

  parameters : parameters option;
(** Parameter types. [None] for K&R-style 'int foo()' function.
    {[
let example = "void f(void);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { parameters = Some {
        non_variadic = [];
        variadic = false }}}}] -> ()
  | _ -> assert false

let example = "void f();"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { parameters = None }}}] -> ()
  | _ -> assert false

    ]}
 *)
}

(** Function parameters. *)
and parameters = {
  non_variadic : parameter list;
(** Non-variadic parameters: the list gives for each argument its name and its
    type.

    For a function type which is not attached to an actual function declaration,
    all arguments have the empty name ([""]) and no default value ([None]),
    since Clang does not keep argument names in function types.
    {[
let example = "void f(int i);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { parameters = Some {
        non_variadic = [{ desc = {
          name = "i";
          qual_type = { desc = BuiltinType Int };
          default = None }}];
        variadic = false }}}}] -> ()
  | _ -> assert false

let example = "void f(int);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { parameters = Some {
        non_variadic = [{ desc = {
          name = "";
          qual_type = { desc = BuiltinType Int };
          default = None }}];
        variadic = false }}}}] -> ()
  | _ -> assert false

let example = "typedef void (*f)(int x);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = TypedefDecl {
      name = "f";
      underlying_type = { desc =
        Pointer { desc = FunctionType { parameters = Some {
          non_variadic = [{ desc = {
            name = "";
            qual_type = { desc = BuiltinType Int };
            default = None }}];
          variadic = false }}}}}}] -> ()
  | _ -> assert false

let example = {| void f(int i = 1) {} |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { parameters = Some {
        non_variadic = [{ desc = {
          name = "i";
          qual_type = { desc = BuiltinType Int };
          default = Some { desc = IntegerLiteral (Int 1) }}}];
        variadic = false }}}}] -> ()
  | _ -> assert false
    ]}
 *)
  variadic : bool;
(** True if the function type is variadic.
    {[
let example = "void f(int i, ...);"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Function {
      name = "f";
      function_type = { parameters = Some {
        non_variadic = [
          { desc = { name = "i"; qual_type = { desc = BuiltinType Int }}}];
        variadic = true }}}}] -> ()
  | decls ->
      List.iter (fun decl -> Format.eprintf "%a@." Clang.Ast.pp_decl decl)
        decls;
      assert false
    ]}
 *)
}

and parameter = (parameter_desc, qual_type) open_node

and parameter_desc = {
  qual_type : qual_type;
  name : string;
  default : expr option; (** C++ *)
}
(** Function or method parameter. *)

(** {3 Statements}

The following example declares the function [parse_statement_list]
that returns the AST obtained from the parsing of [source] string as a
statement list (by putting it in the context of a function):
this function is used in the following examples to check Clang.Ast.pp_decl the
AST of various types.
    {[
let parse_statement_list ?(return_type = "int") ?filename ?command_line_args
    ?language ?options source =
  match
    Printf.sprintf "%s f(void) { %s }" return_type source |>
    parse_declaration_list ?filename ?command_line_args ?language ?options
  with
  | [{ desc = Function { body = Some { desc = Compound items }}}] -> items
  | decls ->
      List.iter (fun decl ->
        Format.eprintf "%a@." Clang.Ast.pp_decl decl) decls;
      assert false
    ]}*)

and stmt = (stmt_desc, qual_type) open_node

and stmt_desc =
  | Null
(** Null statement.
    {[
let example = ";"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Null }] -> ()
  | _ -> assert false
    ]}
    *)
  | Compound of stmt list
(** Compound statement.
    {[
let example = "{}"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Compound [] }] -> ()
  | _ -> assert false

let example = "{;;}"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Compound [{ desc = Null }; { desc = Null }] }] -> ()
  | _ -> assert false
    ]}
    *)
  | For of {
      init : stmt option;
      condition_variable : var_decl option; (** C++ *)
      cond : expr option;
      inc : stmt option;
      body : stmt;
    }
(** For statement.
    {[
let example = "for (;;) {}"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = For {
      init = None;
      condition_variable = None;
      cond = None;
      inc = None;
      body = { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = "int i; for (i = 0; i < 4; i++) { i; }"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = For {
      init = Some { desc = Expr { desc = BinaryOperator {
        lhs = { desc = DeclRef (Ident "i")};
        kind = Assign;
        rhs = { desc = IntegerLiteral (Int 0)}}}};
      condition_variable = None;
      cond = Some { desc = BinaryOperator {
        lhs = { desc = DeclRef (Ident "i")};
        kind = LT;
        rhs = { desc = IntegerLiteral (Int 4)}}};
      inc = Some { desc = Expr { desc = UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef (Ident "i")}}}};
      body = { desc = Compound [{ desc =
        Expr { desc = DeclRef (Ident "i") }}] }}}] -> ()
  | _ -> assert false

let example = "for (int i = 0; i < 4; i++) { i; }"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        var_name = "i";
        var_type = { desc = BuiltinType Int};
        var_init = Some { desc = IntegerLiteral (Int 0)}}}] };
      condition_variable = None;
      cond = Some { desc = BinaryOperator {
        lhs = { desc = DeclRef (Ident "i")};
        kind = LT;
        rhs = { desc = IntegerLiteral (Int 4)}}};
      inc = Some { desc = Expr { desc = UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef (Ident "i")}}}};
      body = { desc = Compound [{ desc =
        Expr { desc = DeclRef (Ident "i") }}] }}}] -> ()
  | _ -> assert false

let example = "for (int i = 0; int j = i - 1; i--) { j; }"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list
    ~language:CXX) example @@ fun ast -> match ast with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        var_name = "i";
        var_type = { desc = BuiltinType Int};
        var_init = Some { desc = IntegerLiteral (Int 0)}}}] };
      condition_variable = Some { desc = {
        var_name = "j";
        var_type = { desc = BuiltinType Int};
        var_init = Some { desc = BinaryOperator {
          lhs = { desc = DeclRef (Ident "i")};
          kind = Sub;
          rhs = { desc = IntegerLiteral (Int 1)}}}}};
      cond = Some { desc = DeclRef (Ident "j") };
      inc = Some { desc = Expr { desc = UnaryOperator {
        kind = PostDec;
        operand = { desc = DeclRef (Ident "i")}}}};
      body = { desc = Compound [{ desc =
        Expr { desc = DeclRef (Ident "j") }}] }}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral (Int 1)};
       then_branch = { desc = Compound [{
         desc = Expr { desc = IntegerLiteral (Int 2)}}] };
       else_branch = Some { desc = Compound [{
         desc = Expr { desc = IntegerLiteral (Int 3)}}] }}}] -> ()
  | _ -> assert false

let example = "if (1) { 2; }"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral (Int 1)};
       then_branch = { desc = Compound [{
         desc = Expr { desc = IntegerLiteral (Int 2)}}] };
       else_branch = None }}] -> ()
  | _ -> assert false

let example = "if (int i = 1) { i; }"

let () =
  check Clang.Ast.pp_stmt
    (parse_statement_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = Some ({ desc = {
         var_type = { desc = BuiltinType Int};
         var_name = "i";
         var_init = Some { desc = IntegerLiteral (Int 1) }}});
       cond = { desc = DeclRef (Ident "i")};
       then_branch = { desc = Compound [{
         desc = Expr { desc = DeclRef (Ident "i") }}] };
       else_branch = None }}] -> ()
  | _ -> assert false

    ]}

    Init statements in [if] (C++17) are available since 3.9.0.

    {[
let example = "if (int i = 1; i) { i; }"

let () =
  if Clang.get_clang_version () >= "clang version 3.9.0" then
    check Clang.Ast.pp_stmt (parse_statement_list ~language:CXX) example
    @@ fun ast -> match ast with
    | [{ desc = If {
         init = Some { desc = Decl [{ desc = Var {
           var_name = "i";
           var_type = { desc = BuiltinType Int };
           var_init = Some { desc = IntegerLiteral (Int 1) }}}] };
         condition_variable = None;
         then_branch = { desc = Compound [{
           desc = Expr { desc = DeclRef (Ident "i") }}] };
         else_branch = None }}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          body = { desc =
            Expr { desc =
              Call { callee = { desc = DeclRef (Ident "f") }; args = [] }}}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 2)};
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] -> ()
  | _ -> assert false

let example =
  "switch (int i = 1) { case 1: f(); break; case 2: break; default:;}"

let () =
  check Clang.Ast.pp_stmt (parse_statement_list ~language:CXX) example
  @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = Some ({ desc = {
         var_type = { desc = BuiltinType Int};
         var_name = "i";
         var_init = Some { desc = IntegerLiteral (Int 1)}}});
      cond = { desc = DeclRef (Ident "i") };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          body = { desc =
            Expr { desc =
              Call { callee = { desc = DeclRef (Ident "f") }; args = [] }}}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 2)};
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] -> ()
  | _ -> assert false
    ]}


    Init statements in [if] (C++17) are available since 3.9.0.

    {[
let example =
  "switch (int i = 1; i) { case 1: f(); break; case 2: break; default:;}"

let () =
  if Clang.get_clang_version () >= "clang version 3.9.0" then
    check Clang.Ast.pp_stmt
    (parse_statement_list ~language:CXX) example @@
    fun ast -> match ast with
    | [{ desc = Switch {
        init = Some { desc = Decl [{ desc = Var {
           var_name = "i";
           var_type = { desc = BuiltinType Int };
           var_init = Some { desc = IntegerLiteral (Int 1)}}}] };
        condition_variable = None;
        cond = { desc = DeclRef (Ident "i") };
        body = { desc = Compound [
          { desc = Case {
            lhs = { desc = IntegerLiteral (Int 1)};
            body = { desc =
              Expr { desc = Call {
                callee = { desc = DeclRef (Ident "f") }; args = [] }}}}};
          { desc = Break };
          { desc = Case {
            lhs = { desc = IntegerLiteral (Int 2)};
            body = { desc = Break }}};
          { desc = Default { desc = Null }}] }}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          rhs = None;
          body = { desc =
            Expr { desc = Call {
              callee = { desc = DeclRef (Ident "f") }; args = [] }}}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 2)};
          rhs = Some { desc = IntegerLiteral (Int 3)};
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}] -> ()
  | _ -> assert false

let example = "switch (1) { case 1: case 2: case 3: default: ;}"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          rhs = None;
          body = { desc = Case {
            lhs = { desc = IntegerLiteral (Int 2)};
            rhs = None;
            body = { desc = Case {
              lhs = { desc = IntegerLiteral (Int 3)};
              rhs = None;
              body = { desc = Default { desc = Null }}}}}}}}] }}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = While {
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Null }}}] -> ()
  | _ -> assert false

let example = "while (int i = 1) { i; }"

let () =
  check Clang.Ast.pp_stmt
    (parse_statement_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = While {
      condition_variable = Some ({ desc = {
         var_type = { desc = BuiltinType Int};
         var_name = "i";
         var_init = Some { desc = IntegerLiteral (Int 1)}}});
      cond = { desc = DeclRef (Ident "i") };
      body = { desc = Compound [{ desc =
        Expr { desc = DeclRef (Ident "i") }}] }}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Do {
      body = { desc = Null };
      cond = { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = "do { f(); } while (1);"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Do {
      body = { desc = Compound [{ desc =
        Expr { desc = Call { callee = { desc = DeclRef (Ident "f") }; args = [] }}}] };
      cond = { desc = IntegerLiteral (Int 1)}}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = IntegerLiteral (Int 1)}}}};
      { desc = Expr { desc = IntegerLiteral (Int 2)}}] -> ()
  | _ -> assert false
    ]}*)
  | Goto of label_ref
(** Goto statement.
    {[
let example = "label: 1; goto label;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = IntegerLiteral (Int 1)}}}};
      { desc = Goto "label" }] -> ()
  | _ -> assert false
    ]}*)
  | IndirectGoto of expr
(** Indirect goto statement (Labels as Values GNU extension).
    {[
let example = "label: 1; void *ptr = &&label; goto *ptr;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = IntegerLiteral (Int 1)}}}};
      { desc = Decl [{ desc = Var {
        var_name = "ptr";
        var_type = { desc = Pointer { desc = BuiltinType Void }};
        var_init = Some { desc = AddrLabel "label" }}}] };
      { desc = IndirectGoto { desc = DeclRef (Ident "ptr")}}] -> ()
  | _ -> assert false
    ]}*)
  | Continue
(** Continue statement.
    {[
let example = "for (;;) continue;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = For { body = { desc = Continue } }}] -> ()
  | _ -> assert false
   ]}*)
  | Break
(** Break statement.
    {[
let example = "for (;;) break;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
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
  check Clang.Ast.pp_stmt (parse_statement_list ~return_type:"void") example @@
  fun ast -> match ast with
  | [{ desc = Return None }] -> ()
  | _ -> assert false

let example = "return 1;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Return (Some { desc = IntegerLiteral (Int 1)})}] -> ()
  | _ -> assert false
   ]}*)
  | Decl of decl list
  | Expr of expr
  | OtherStmt

(** {3 Expressions} *)

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
    By default, integer literals are converted if possible into {!constr:Int}
    and integers too large to be represented as [int] are not converted.
    Integer literals can be preserved as {!constr:CXInt}
    by turning {!recfield:Clang.convert_integer_literals} option false.
    {[
let example = "0;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (Int 0) }}] -> ()
  | _ -> assert false

let () =
  check Clang.Ast.pp_stmt (parse_statement_list
    ~options:(Clang.Ast.Options.make ~convert_integer_literals:false ()))
    example @@ fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (CXInt _ as zero) }}] ->
      assert (Clang.Ast.int_of_literal zero = 0)
  | _ -> assert false

let large_int = Int64.add (Int64.of_int max_int) 1L
let example = Printf.sprintf "%Ld;" large_int

let () =
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (CXInt _ as large_int') }}] ->
      assert (Clang.Ast.int64_of_literal large_int' = large_int)
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

    By default, floating literals are converted into {!constr:Float}.
    Floating literals can be preserved as {!constr:CXFloat}
    by turning {!recfield:Clang.convert_floating_literals} option false.
    {[
let example = "0.5;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = FloatingLiteral (Float 0.5) }}] -> ()
  | _ -> assert false

let () =
  check Clang.Ast.pp_stmt (parse_statement_list
    ~options:(Clang.Ast.Options.make ~convert_floating_literals:true ()))
    example @@ fun ast -> match ast with
  | [{ desc = Expr { desc = FloatingLiteral f }}] ->
    assert (Clang.Ast.float_of_literal f = 0.5)
  | _ -> assert false
    ]}*)
  | StringLiteral of string
(** String literal.
    {[
let example = "\"Hello!\";"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = StringLiteral "Hello!" }}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
        CharacterLiteral { kind = Ascii; value = 0x61 } }}] -> ()
  | _ -> assert false

let example = "L'a';"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
        CharacterLiteral { kind = Wide; value = 0x61 } }}] -> ()
  | _ -> assert false


let example = "u8'a';"

let () =
  if Clang.get_clang_version () >= "clang version 3.6" then
    check Clang.Ast.pp_stmt (parse_statement_list ~language:CXX
        ~command_line_args:["-std=c++1z"]) example @@
    fun ast -> match ast with
    | [{ desc = Expr { desc = CharacterLiteral { kind = UTF8; value = 0x61 } }}]
      -> assert (Clang.get_clang_version () >= "clang version 3.8.0")
    | [{ desc = Expr { desc =
          CharacterLiteral { kind = Ascii; value = 0x61 } }}]
      -> assert (Clang.get_clang_version () < "clang version 3.8.0")
    | _ -> assert false

let example = "u'a';"

let () =
  if Clang.get_clang_version () >= "clang version 3.6" then
    check Clang.Ast.pp_stmt parse_statement_list example
    @@ fun ast -> match ast with
    | [{ desc = Expr { desc =
          CharacterLiteral { kind = UTF16; value = 0x61 } }}]
      -> assert (Clang.get_clang_version () >= "clang version 3.8.0")
    | [{ desc = Expr { desc = CharacterLiteral { kind = UTF8; value = 0x61 } }}]
      -> assert (Clang.get_clang_version () < "clang version 3.8.0")
    | _ -> assert false
    ]}*)
  | ImaginaryLiteral of expr
(** Imaginary literal.
    {[
let example = "1i;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
      ImaginaryLiteral { desc = IntegerLiteral (Int 1)} }}] -> ()
  | _ -> assert false

let example = "2.5i;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
      ImaginaryLiteral { desc = FloatingLiteral (Float 2.5)}}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = UnaryOperator {
      kind = Plus;
      operand = { desc = IntegerLiteral (Int 1)}}}}] -> ()
  | _ -> assert false

let example = "int x; &x;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = UnaryOperator {
      kind = AddrOf;
      operand = { desc = DeclRef (Ident "x") }} }}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = BinaryOperator {
      lhs = { desc = IntegerLiteral (Int 1)};
      kind = Add;
      rhs = { desc = IntegerLiteral (Int 2)}}}}] -> ()
  | _ -> assert false

let example = "int i = 2; i *= 3;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = DeclRef (Ident "i")};
      kind = MulAssign;
      rhs = { desc = IntegerLiteral (Int 3)}}}}] -> ()
  | _ -> assert false
    ]} *)
  | DeclRef of ident_ref
(** Declaration reference.
    {[
let example = "int i; i;"

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = DeclRef (Ident "i") }}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = Call {
      callee = { desc = DeclRef (Ident "g") };
      args = [{ desc = IntegerLiteral (Int 1)}] }}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = Cast {
      kind = CStyle;
      qual_type = { desc = Pointer _ };
      operand = { desc = StringLiteral "Hello" }} }}] -> ()
  | _ -> assert false
    ]}

    Implicit casts are removed in the AST unless [~ignore_implicit_cast:false]
    is passed to the converting function.

    {[
let example = {| int i; i; |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list
    ~options:(Clang.Ast.Options.make ~ignore_implicit_cast:false ()))
    example @@
  fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = Cast {
      kind = Implicit;
      qual_type = { desc = BuiltinType Int };
      operand = { desc = DeclRef (Ident "i") }} }}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = Member {
        base = { desc = DeclRef (Ident "s") };
        arrow = false;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral (Int 0)}}}}] -> ()
  | _ -> assert false

let example = {| struct s { int i } *p; p->i = 0; |}

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = Member {
        base = { desc = DeclRef (Ident "p") };
        arrow = true;
        field = { desc = "i" }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral (Int 0)}}}}] -> ()
  | _ -> assert false
    ]}*)
  | MemberRef of string
  | ArraySubscript of {
      base : expr;
      index : expr;
    }
(** Array subscript
    {[
let example = {| int a[1]; a[0] = 1; |}

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = ArraySubscript {
        base = { desc = DeclRef (Ident "a") };
        index = { desc = IntegerLiteral (Int 0)}}};
      kind = Assign;
      rhs = { desc = IntegerLiteral (Int 1)}}}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = ConditionalOperator {
      cond = { desc = IntegerLiteral (Int 1)};
      then_branch = Some { desc = IntegerLiteral (Int 2)};
      else_branch = { desc = IntegerLiteral (Int 3)}}}}] -> ()
  | _ -> assert false

let example = {| 1 ? : 3; |}

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = ConditionalOperator {
      cond = { desc = IntegerLiteral (Int 1)};
      then_branch = None;
      else_branch = { desc = IntegerLiteral (Int 3)}}}}] -> ()
  | _ -> assert false
    ]}*)
  | Paren of expr
(** Parenthesed expression.

    Parenthesed expression are removed in the AST unless ~ignore_paren:false
    is passed to the converting function.
    {[
let example = {| (1); |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list
    ~options:(Clang.Ast.Options.make ~ignore_paren:false ()))
    example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = Paren { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (Int 1)}}] -> ()
  | _ -> assert false

let example = {| int i; sizeof(i); |}

let () =
  check Clang.Ast.pp_stmt (parse_statement_list
    ~options:(Clang.Ast.Options.make ~ignore_paren:false ()))
    example @@
  fun ast -> match ast with
  | [ { desc = Decl _ };
      { desc = Expr { desc = UnaryExpr {
          kind = SizeOf;
          argument = ArgumentExpr { desc = Paren { desc = DeclRef (Ident "i") }}} }}] ->
      ()
  | _ -> assert false
    ]}*)
  | AddrLabel of string
(** Label address (Labels as Values GNU extension).
    {[
let example = {| label: &&label; |}

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = AddrLabel "label" }}}}] ->
      ()
  | _ -> assert false
    ]}*)
  | InitList of expr list
(** Initialization list.
    {[
let example = {| int a[2] = { 1, 2 }; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Var { var_name = "a"; var_type = {
      desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 2 }};
      var_init = Some { desc = InitList [
        { desc = IntegerLiteral (Int 1)};
        { desc = IntegerLiteral (Int 2)}] }}}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = CompoundLiteral {
      qual_type = { desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 2 }};
      init = { desc = InitList [
        { desc = IntegerLiteral (Int 1)};
        { desc = IntegerLiteral (Int 2)}] }} }}] -> ()
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
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [ { desc = Decl _ };
      { desc = Expr { desc = UnaryExpr {
          kind = SizeOf;
          argument = ArgumentExpr { desc = DeclRef (Ident "i") }} }}] -> ()
  | _ -> assert false

let example = {| sizeof(int); |}

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [ { desc = Expr { desc = UnaryExpr {
          kind = SizeOf;
          argument = ArgumentType { desc = BuiltinType Int }} }}] -> ()
  | _ -> assert false

let example = {| alignof(int); |}

let () =
  check Clang.Ast.pp_stmt
    (parse_statement_list ~language:CXX
      ~command_line_args:["-std=c++11"])
    example @@
  fun ast -> match ast with
  | [ { desc = Expr { desc = UnaryExpr {
          kind = AlignOf;
          argument = ArgumentType { desc = BuiltinType Int }} }}] -> ()
  | _ -> assert false
    ]}

    From Clang>=6.0.0, [alignof] is available by default with C++.

    {[
let () =
  if Clang.get_clang_version () >= "clang version 6.0.0" then
    check Clang.Ast.pp_stmt (parse_statement_list ~language:CXX)
    example @@ fun ast -> match ast with
    | [ { desc = Expr { desc = UnaryExpr {
            kind = AlignOf;
            argument = ArgumentType { desc = BuiltinType Int }} }}] -> ()
    | _ -> assert false
    ]}
*)
  | GenericSelection of {
      controlling_expr : expr;
      assocs : (qual_type option * expr) list;
    }
(**
   Generic selection (C11).

   {[
let example = {|
   _Generic("expr", double: 1, float: 2, default: 3);
   |}

let () =
  check Clang.Ast.pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [ { desc = Expr { desc = GenericSelection {
          controlling_expr = { desc = StringLiteral "expr" };
          assocs = [
            (Some { desc = BuiltinType Double}, { desc = IntegerLiteral (Int 1)});
            (Some { desc = BuiltinType Float}, { desc = IntegerLiteral (Int 2)});
            (None, { desc = IntegerLiteral (Int 3)})] }}}] -> ()
  | _ -> assert false
   ]}
 *)
  | Predefined of {
      kind : clang_ext_predefinedexpr_identkind;
      function_name : string;
    }
(**
   Predefined identifiers.

   {[
let example = {|
  void myfunc(void)
    {
      char *_s = __func__;
    }
  |}

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Void};
        parameters = Some { non_variadic = []; variadic = false }};
      name = "myfunc";
      body = Some { desc = Compound [{
        desc = Decl [{ desc = Var {
          var_type = { desc = Pointer { desc = BuiltinType Char_S }};
          var_name = "_s";
          var_init = Some { desc = Predefined {
            kind = Func;
            function_name = "myfunc"; }}}}] }] }}}]]
   ]}*)
  | UnexposedExpr of clang_ext_stmtkind
  | UnknownExpr of cxcursorkind

and cast_kind =
  | CStyle
  | Implicit

and unary_expr_or_type_trait =
  | ArgumentExpr of expr
  | ArgumentType of qual_type

(** {3 Declarations} *)

and decl = (decl_desc, qual_type) open_node

and decl_desc =
  | TemplateDecl of {
      parameters : template_parameter list;
      decl : decl;
    }
(** Template declaration.

    {[
let example = {| template <class X, int i> int f(X); |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = [
        { desc = {
            parameter_name = "X";
            parameter_kind = Class { default = None }}};
        { desc = { parameter_name = "i"; parameter_kind = NonType {
            parameter_type = { desc = BuiltinType Int };
            default = None }}}];
      decl = { desc = Function {
        function_type = {
          calling_conv = C;
          result = { desc = BuiltinType Int};
          parameters = Some {
            non_variadic = [
              { desc = {
                  name = "";
                  qual_type = { desc = TemplateTypeParm "X"}}}];
            variadic = false }};
        name = "f";
        body = None }}}}] -> ()
  | _ -> assert false

let example = {|
  template <class X = bool, int i = 4>
  class C { X x; int v = i; };
|}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = [
        { desc = {
            parameter_name = "X";
            parameter_kind =
              Class { default = Some { desc = BuiltinType Bool }}}};
        { desc = { parameter_name = "i"; parameter_kind = NonType {
            parameter_type = { desc = BuiltinType Int };
            default = Some { desc = IntegerLiteral (Int 4)}}}}];
      decl = { desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field {
              name = "x";
              qual_type = { desc = TemplateTypeParm "X" }}};
          { desc = Field {
              name = "v";
              qual_type = { desc = BuiltinType Int }}}] }}}}] -> ()
  | _ -> assert false

let example = {|
  class C {
    template <class X>
    int f(X x);
  };

  template <class X>
  int C::f(X x)
  {
    return 0;
  } |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Class;
      name = "C";
      fields = [
        { desc = TemplateDecl {
            parameters = [
              { desc = { parameter_name = "X";
                parameter_kind = Class { default = None };
                parameter_pack = false; }}];
            decl = { desc = CXXMethod {
              type_ref = None;
              function_type = {
                result = { desc = BuiltinType Int };
                parameters = Some { non_variadic = [
                  { desc = {
                      name = "x";
                      qual_type = { desc = TemplateTypeParm "X" }}}] }};
              name = "f";
              body = None; }}}}] }};
     { desc = TemplateDecl {
         parameters = [{ desc = {
           parameter_name = "X";
           parameter_kind = Class { default = None };
           parameter_pack = false; }}];
         decl = { desc = CXXMethod {
           type_ref = Some { desc = Record (Ident "C") };
           function_type = {
             result = { desc = BuiltinType Int };
             parameters = Some { non_variadic = [
               { desc = {
                   name = "x";
                   qual_type = { desc = TemplateTypeParm "X" }}}] }};
           name = "f";
           body = Some { desc = Compound [
             { desc =
                 Return (Some { desc = IntegerLiteral (Int 0)})}] }; }}}}] -> ()
  | _ -> assert false

let example = {| template<class ... Types> struct Tuple {}; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
        parameters = [
          { desc = {
            parameter_name = "Types";
            parameter_kind = Class { default = None };
            parameter_pack = true; }}];
        decl = { desc = RecordDecl {
          keyword = Struct;
          name = "Tuple"; }}}}] -> ()
  | _ -> assert false
    ]}*)
  | Function of {
      linkage : linkage_kind;
      function_type : function_type;
      name : string;
      body : stmt option;
      deleted : bool;
    }
(** Function definition or forward declaration.
    In case of function definition, we should have
    [body = Some { desc = Compound list; _ }] for some [list].
    In case of forward declaration, [body = None].

    {[
let example = {| int f(void) {} |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Int};
        parameters = Some { non_variadic = []; variadic = false }};
      name = "f";
      body = Some { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = {| static int f(int x); |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      linkage = Internal;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Int};
        parameters = Some {
          non_variadic = [
            { desc = { name = "x"; qual_type = { desc = BuiltinType Int }}}];
          variadic = false }};
      name = "f";
      body = None }}] -> ()
  | _ -> assert false
    ]}*)
  | CXXMethod of {
      type_ref : qual_type option;
      function_type : function_type;
      name : string;
      body : stmt option;
      defaulted : bool;
      static : bool;
      binding : cxx_method_binding_kind;
      const : bool;
      deleted : bool;
    }
(** C++ method.

    {[
let example = {|
    class C {
      int f(char);
      void const_method() const {
      }
      virtual void virtual_method() {
      }
      virtual void pure_virtual_method() = 0;
      static void static_method() {
      }
      void deleted_method() =delete;
      C &operator+(C &rhs) {
      }
    };

    int C::f(char c) {
      return 0;
    }
 |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Class;
      name = "C";
      fields = [
        { desc = CXXMethod {
          type_ref = None;
          name = "f";
          function_type = {
            result = { desc = BuiltinType Int };
            parameters = Some { non_variadic = [
              { desc = {
                  name = "";
                  qual_type = { desc = BuiltinType Char_S }}}] }};
          body = None; }};
        { desc = CXXMethod {
          type_ref = None;
          name = "const_method";
          function_type = {
            result = { desc = BuiltinType Void };
            parameters = Some { non_variadic = [] }};
          body = Some { desc = Compound [] };
          const = true; }};
        { desc = CXXMethod {
          type_ref = None;
          name = "virtual_method";
          function_type = {
            result = { desc = BuiltinType Void };
            parameters = Some { non_variadic = [] }};
          body = Some { desc = Compound [] };
          binding = Virtual; }};
        { desc = CXXMethod {
          type_ref = None;
          name = "pure_virtual_method";
          function_type = {
            result = { desc = BuiltinType Void };
            parameters = Some { non_variadic = [] }};
          body = None;
          binding = PureVirtual; }};
        { desc = CXXMethod {
          type_ref = None;
          name = "static_method";
          function_type = {
            result = { desc = BuiltinType Void };
            parameters = Some { non_variadic = [] }};
          body = Some { desc = Compound [] };
          static = true; }};
        { desc = CXXMethod {
          type_ref = None;
          name = "deleted_method";
          function_type = {
            result = { desc = BuiltinType Void };
            parameters = Some { non_variadic = [] }};
          body = None;
          deleted = true; }};
        { desc = CXXMethod {
          type_ref = Some { desc = Record (Ident "C") };
          name = "operator+";
          function_type = {
            result = { desc = LValueReference { desc = Record (Ident "C") }};
            parameters = Some {
              non_variadic = [{ desc = {
                name = "rhs";
                qual_type = { desc =
                  LValueReference { desc = Record (Ident "C") }}}}] }};
          body = Some { desc = Compound [] }; }}; ] }};
      { desc = CXXMethod {
        type_ref = Some { desc = Record (Ident "C") };
        name = "f";
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some { non_variadic = [{ desc = {
            name = "c";
            qual_type = { desc = BuiltinType Char_S }}}] }};
        body = Some { desc = Compound [
          { desc = Return (Some { desc = IntegerLiteral (Int 0) })}] }}}] -> ()
  | _ -> assert false
    ]}*)
  | Var of var_decl_desc
(** Variable declaration.

    {[
let example = {| int x = 1; |}

let () =
    check Clang.Ast.pp_decl parse_declaration_list example @@
    fun ast -> match ast with
  | [{ desc = Var {
      linkage = External;
      var_type = { const = false; desc = BuiltinType Int };
      var_name = "x";
      var_init = Some { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = {| const int x = 1; |}

let () =
    check Clang.Ast.pp_decl parse_declaration_list example @@
    fun ast -> match ast with
  | [{ desc = Var {
      linkage = External;
      var_type = { const = true; desc = BuiltinType Int };
      var_name = "x";
      var_init = Some { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = {| static int x = 1; |}

let () =
    check Clang.Ast.pp_decl parse_declaration_list example @@
    fun ast -> match ast with
  | [{ desc = Var {
      linkage = Internal;
      var_type = { const = false; desc = BuiltinType Int };
      var_name = "x";
      var_init = Some ({ desc = IntegerLiteral (Int 1)})}}] -> ()
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
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = EnumDecl {
      name = "e";
      constants = [
        { desc = { constant_name = "A"; constant_init = None }} as a;
        { desc = {
          constant_name = "B";
          constant_init = Some { desc = IntegerLiteral (Int 2)}}} as b;
        { desc = { constant_name = "C"; constant_init = None }} as c] }}]]
  ~result:(fun bindings ->
    assert (Clang.Enum_constant.get_value bindings#a = 0);
    assert (Clang.Enum_constant.get_value bindings#b = 2);
    assert (Clang.Enum_constant.get_value bindings#c = 3))
    ]}*)
  | RecordDecl of {
      keyword : elaborated_type_keyword;
      name : string;
      fields : decl list;
    }
(** Record declaration ([struct] or [union]).
    {[
let example = {| struct s { int i; float f; }; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "i";
          qual_type = { desc = BuiltinType Int}}};
        { desc = Field { name = "f";
          qual_type = { desc = BuiltinType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| struct s { int a:1; int b:2; int c; }; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "a";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 1)}}} as a;
        { desc = Field { name = "b";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 2)}}};
        { desc = Field { name = "c";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None}}] }}] ->
      assert (Clang.Decl.get_field_bit_width a = 1)
  | _ -> assert false

let example = {| union u { int i; float f; }; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Union;
      name = "u";
      fields = [
        { desc = Field { name = "i";
          qual_type = { desc = BuiltinType Int}}};
        { desc = Field { name = "f";
          qual_type = { desc = BuiltinType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| union u { int a:1; int b:2; int c; }; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Union;
      name = "u";
      fields = [
        { desc = Field { name = "a";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 1)}}} as a;
        { desc = Field { name = "b";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 2)}}};
        { desc = Field { name = "c";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None}}] }}] ->
      assert (Clang.Decl.get_field_bit_width a = 1)
  | _ -> assert false

let example = {| struct s { int label; union { int i; float f; };}; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "label";
          qual_type = { desc = BuiltinType Int}}};
        { desc = RecordDecl { keyword = Union; name = ""; fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }}] }}] -> ()
  | _ -> assert false

let example = {| union s { int single; struct { int i; float f; };}; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Union;
      name = "s";
      fields = [
        { desc = Field { name = "single";
          qual_type = { desc = BuiltinType Int}}};
        { desc = RecordDecl { keyword = Struct; name = ""; fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }}] }}] -> ()
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
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = TypedefDecl {
      name = "int_t";
      underlying_type = { desc = BuiltinType Int }}}] -> ()
  | _ -> assert false

let example = {| typedef union u { int i; float f } u_t; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
        keyword = Union;
        name = "u";
        fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }};
      { desc = TypedefDecl {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record (Ident "u") }}}}}] -> ()
  | _ -> assert false

let example = {| typedef union { int i; float f } u_t; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
        keyword = Union;
        name = "";
        fields = [
          { desc = Field {
              name = "i";
              qual_type = { desc = BuiltinType Int}}};
          { desc = Field {
              name = "f";
              qual_type = { desc = BuiltinType Float}}}] }};
      { desc = TypedefDecl {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record (Ident "") }}} as underlying_type }}] ->
        let fields = underlying_type |> Clang.Type.list_of_fields in
        begin
          match fields with
          | [ { desc = Field {
                  name = "i";
                  qual_type = { desc = BuiltinType Int}}};
              { desc = Field {
                  name = "f";
                  qual_type = { desc = BuiltinType Float}}}] -> ()
          | fields ->
              Format.eprintf "%a@." (Format.pp_print_list
                ~pp_sep:Format.pp_print_newline
                Clang.Ast.pp_decl) fields;
              assert false
        end
  | _ -> assert false
    ]}*)
  | Field of  {
      name : string;
      qual_type : qual_type;
      bitwidth : expr option;
      init : expr option; (* C++11 *)
    }
(** Record (struct, union or class) field.

    {[
let example = {| struct s { int label : 1; union u { int i; float f; } data;}; |}

let () =
  check Clang.Ast.pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "label";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 1)};
          init = None; }};
        { desc = RecordDecl { keyword = Union; name = "u"; fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int};
            bitwidth = None;
            init = None; }};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float};
            bitwidth = None;
            init = None; }}] }};
        { desc = Field { name = "data";
          qual_type = { desc = Elaborated {
            keyword = Union;
            named_type = { desc = Record (Ident "u") }}};
          bitwidth = None;
          init = None; }}] }}] -> ()
  | _ -> assert false

let example = {| class C { int i = 1; }; |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Class;
      name = "C";
      fields = [
        { desc = Field { name = "i";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None;
          init = Some { desc = IntegerLiteral (Int 1)}}}] }}] -> ()
  | _ -> assert false
    ]}

    Default member initializer for bit-field is a C++2a extension
    (supported from Clang >6.0.0).

    {[

let example = {| class C { int i : 3 = 2; }; |}

let () =
  if Clang.get_clang_version () >= "clang version 6.0.0" then
    check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX
      ~command_line_args:["-std=c++2a"]) example
    @@ fun ast -> match ast with
    | [{ desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int};
            bitwidth = Some { desc = IntegerLiteral (Int 3)};
            init = Some { desc = IntegerLiteral (Int 2)}}}] }}] -> ()
    | _ -> assert false
      ]}
*)
  | CXXAccessSpecifier of cxx_access_specifier
(** C++ access specifier.

    {[
let example = {|
    class c {
      private: int private_field;
      protected: int protected_field;
      public: int public_field;
    };
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Class;
      name = "c";
      fields = [
        { desc = CXXAccessSpecifier CXXPrivate };
        { desc = Field { name = "private_field";
          qual_type = { desc = BuiltinType Int}}};
        { desc = CXXAccessSpecifier CXXProtected };
        { desc = Field { name = "protected_field";
          qual_type = { desc = BuiltinType Int}}};
        { desc = CXXAccessSpecifier CXXPublic };
        { desc = Field { name = "public_field";
          qual_type = { desc = BuiltinType Int}}}] }}] -> ()
  | _ -> assert false
    ]}
*)
  | Namespace of {
      name : string;
      declarations : decl list;
    }
(** C++ namespace.

    {[
let example = {|
    namespace example {
      int i;
    }
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = Namespace {
      name = "example";
      declarations = [
        { desc = Var { var_name = "i";
          var_type = { desc = BuiltinType Int}}}] }}] -> ()
  | _ -> assert false
    ]}
*)
  | Using of {
      namespace : string;
      decl : string option;
    }
(** C++ "using" directive and declaration.

    {[
let example = {|
    using namespace std;
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = Using {
      namespace = "std";
      decl = None }}] -> ()
  | _ -> assert false

let example = {|
    namespace std {
      void cout() {}
    }
    using std::cout;
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match List.hd (List.rev ast) with
  | { desc = Using {
      namespace = "std";
      decl = Some "cout" }} -> ()
  | _ -> assert false
    ]}
*)
  | Constructor of {
      class_name : string;
      parameters : parameters;
      initializer_list : (string * expr) list;
      body : stmt option;
      explicit : bool;
      defaulted : bool;
      deleted : bool;
    }
(**
  C++ class constructor.

    {[
let example = {|
    class C {
      int i;
      C() = delete;
      explicit C(int v) : i(v) {
      }
    };
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Field {
               name = "i";
               qual_type = { desc = BuiltinType Int }}};
           { desc = Constructor {
               class_name = "C";
               parameters = {
                 non_variadic = [];
                 variadic = false;
               };
               initializer_list = [];
               body = None;
               explicit = false;
               defaulted = false;
               deleted = true; }};
           { desc = Constructor {
               parameters = {
                 non_variadic = [{ desc = {
                   name = "v";
                   qual_type = { desc = BuiltinType Int}}}];
                 variadic = false;
               };
               initializer_list = ["i", { desc = DeclRef (Ident "v") }];
               body = Some { desc = Compound [] };
               explicit = true;
               defaulted = false;
               deleted = false; }}; ] }}] -> ()
  | _ -> assert false

let example = {|
    class C {
      C() =default;
    };
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Constructor {
               class_name = "C";
               parameters = {
                 non_variadic = [];
                 variadic = false;
               };
               initializer_list = [];
               body = None;
               explicit = false;
               defaulted = true;
               deleted = false; }}] }}] -> ()
  | _ -> assert false
   ]}*)
  | Destructor of {
      class_name : string;
      body : stmt option;
      defaulted : bool;
      deleted : bool;
    }
(**
  C++ class destructor.

    {[
let example = {|
    class C {
      ~C() {
      }
    };
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Destructor {
               class_name = "C";
               body = Some { desc = Compound [] };
               defaulted = false;
               deleted = false; }}] }}] -> ()
  | _ -> assert false

let example = {|
    class C {
      ~C() =default;
    };
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Destructor {
               class_name = "C";
               body = None;
               defaulted = true;
               deleted = false; }}] }}] -> ()
  | _ -> assert false
   ]}*)
  | LinkageSpec of {
      languages : languages;
      decls : decl list;
    }
(**
  C++ language linkage.

    {[
let example = {|
    extern "C" {
      int i;

      void f() {
      }
    }
   |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = LinkageSpec {
         languages = { c = true; cxx = false };
         decls = [
           { desc = Var {
               var_type = { desc = BuiltinType Int };
               var_name = "i"; }};
           { desc = Function {
               function_type = {
                 result = { desc = BuiltinType Void };
                 parameters = Some { non_variadic = []; variadic = false }};
               name = "f";
               body = Some { desc = Compound [] }}}] }}] -> ()
  | _ -> assert false

let example = {|
    extern "C++" {
      int i;

      void f() {
      }
    }
   |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = LinkageSpec {
         languages = { c = false; cxx = true };
         decls = [
           { desc = Var {
               var_type = { desc = BuiltinType Int };
               var_name = "i"; }};
           { desc = Function {
               function_type = {
                 result = { desc = BuiltinType Void };
                 parameters = Some { non_variadic = []; variadic = false }};
               name = "f";
               body = Some { desc = Compound [] }}}] }}] -> ()
  | _ -> assert false
   ]}*)
  | TemplateTemplateParameter of string
  | Friend of friend_decl
(**
  C++ friend declaration.

   {[
let example = {|
     class C {
       friend void f();
       friend class B;
     };
   |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Friend (FriendDecl { desc = Function {
               function_type = {
                 result = { desc = BuiltinType Void };
                 parameters = Some { non_variadic = []; variadic = false }};
               name = "f";
               body = None }})};
           { desc = Friend (FriendType { desc = Elaborated {
               keyword = Class;
               named_type = { desc = Record (Ident "B") }}})}] }}] -> ()
  | _ -> assert false
   ]}

   {[
let example = {|
     template <typename T> class C {
       friend T; // only in C++0x
     };
   |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
         parameters = [{ desc = {
           parameter_name = "T";
           parameter_kind = Class _ }}];
         decl = { desc = RecordDecl {
           keyword = Class;
           name = "C";
           fields = [
             { desc = Friend (FriendType { desc =
                 TemplateTypeParm "T" })}] }}}}] -> ()
  | _ -> assert false
   ]}

   {[
let example = {|
     class C {
       template <typename T> friend class B;
     };
   |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [{ desc = Friend (FriendDecl { desc = TemplateDecl {
           parameters = [{ desc = {
             parameter_name = "T";
             parameter_kind = Class _ }}];
           decl = { desc = RecordDecl {
             keyword = Class;
             name = "B";
             fields = [] }}}})}] }}] -> ()
  | _ -> assert false
   ]}*)
  | EmptyDecl
(**
  Empty declaration.

    {[
let example = ";"

let () =
  check Clang.Ast.pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = EmptyDecl }] -> ()
  | _ -> assert false
    ]}
*)
  | UnknownDecl of cxcursorkind * clang_ext_declkind

and ident_ref =
  | Ident of string
  | NamespaceRef of {
      namespace_ref : ident_ref;
      ident : string;
    }
(**
  Identifier qualified by a namespace reference (C++).

    {[
let example = {|
  namespace ns1 {
    namespace ns2 {
      int f() {
        return 0;
      }
    }
  }

  int g() {
    return ns1::ns2::f();
  }
|}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [_; { desc =
      Function {
        name = "g";
        body = Some { desc = Compound [
          { desc = Return (Some { desc = Call {
              callee = { desc = DeclRef (
                NamespaceRef {
                  namespace_ref = NamespaceRef {
                    namespace_ref = Ident "ns1";
                    ident = "ns2";
                  };
                  ident = "f";
                })};
              args = []; }})}] }}}] -> ()
  | _ -> assert false
    ]}
*)
  | TypeRef of {
      type_ref : ident_ref;
      qual_type : qual_type;
      ident : string;
    }
(**
  Identifier qualified by a type reference (C++).

  E.g., enumeration in a nested name specifier (C++11 extension).

    {[
let example = {|
  class C {
  public:
    enum E {
      A, B
    };
  };

  C::E g() {
    return C::E::A;
  }
|}

let () =
  if Clang.get_clang_version () >= "clang version 3.7.0" then
    check Clang.Ast.pp_decl
      (parse_declaration_list ~command_line_args:["-std=c++11"]) example @@
    fun ast -> match ast with
    | [_; { desc =
        Function {
          name = "g";
          body = Some { desc = Compound [
            { desc = Return (Some { desc = DeclRef (
                TypeRef {
                  type_ref = TypeRef {
                    type_ref = Ident "C";
                    qual_type = { desc = Record (Ident "C") };
                    ident = "E";
                  };
                  qual_type = { desc = Enum (Ident "E") };
                  ident = "A" })})}] }}}] -> ()
    | _ -> assert false
    ]}
*)

and friend_decl =
  | FriendDecl of decl
  | FriendType of qual_type

and label_ref = string

and enum_constant = (enum_constant_desc, qual_type) open_node

and enum_constant_desc = {
    constant_name : string;
    constant_init : expr option;
  }

and var_decl = (var_decl_desc, qual_type) open_node

and var_decl_desc = {
    linkage : linkage_kind;
    var_name : string;
    var_type : qual_type;
    var_init : expr option;
  }

and cxx_method_binding_kind = NonVirtual | Virtual | PureVirtual
(** C++ method binding kind *)

and template_parameter = (template_parameter_desc, qual_type) open_node
(** C++ template parameter *)

and template_parameter_desc = {
    parameter_name : string;
    parameter_kind : template_parameter_kind;
    parameter_pack : bool; (** C++11 *)
  }

and template_parameter_kind =
  | Class of {
      default : qual_type option;
    }
(** Class (or typename) template parameter.

    {[
let example = {|
  template <class X, typename Y = bool>
  class C { X x; Y y; };
|}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = [
        { desc = {
            parameter_name = "X";
            parameter_kind = Class { default = None }}};
        { desc = {
            parameter_name = "Y";
            parameter_kind =
              Class { default = Some { desc = BuiltinType Bool }}}}];
      decl = { desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field {
              name = "x";
              qual_type = { desc = TemplateTypeParm "X" }}};
          { desc = Field {
              name = "y";
              qual_type = { desc = TemplateTypeParm "Y" }}}] }}}}] -> ()
  | _ -> assert false
    ]}*)
  | NonType of {
      parameter_type : qual_type;
      default : expr option;
    }
(** Non type template parameter.

    {[
let example = {|
  template <int i = 4>
  class C { int v = i; };
|}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = [
        { desc = { parameter_name = "i"; parameter_kind = NonType {
            parameter_type = { desc = BuiltinType Int };
            default = Some { desc = IntegerLiteral (Int 4)}}}}];
      decl = { desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field {
              name = "v";
              qual_type = { desc = BuiltinType Int }}}] }}}}] -> ()
  | _ -> assert false
    ]}*)
  | Template of {
      parameters : template_parameter list;
      default : string option;
    }
(** Template template parameter.

    {[
let example = {|
  template<typename T> class Default {};

  template <template<typename> class T = Default>
  class C {
    T<bool> x;
    T<int> y;
  };
    |}

let () =
  check Clang.Ast.pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
       parameters = [{ desc = {
         parameter_name = "T";
         parameter_kind = Class { default = None };
         parameter_pack = false; }}];
       decl = { desc = RecordDecl {
         keyword = Class;
         name = "Default";
         fields = [] }}}};
     { desc = TemplateDecl {
       parameters = [{ desc = {
         parameter_name = "T";
         parameter_kind = Template {
           parameters = [{ desc = {
             parameter_name = "";
             parameter_kind = Class { default = None };
             parameter_pack = false; }}];
         default = Some "Default" };
         parameter_pack = false; }}];
       decl = { desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Field {
               name = "x";
               qual_type = { desc = TemplateSpecialization {
                 name = NameTemplate { desc = TemplateTemplateParameter "T" };
                 arguments = [Type { desc = BuiltinType Bool }] }}}};
           { desc = Field {
               name = "y";
               qual_type = { desc = TemplateSpecialization {
                 name = NameTemplate { desc = TemplateTemplateParameter "T" };
                 arguments = [Type { desc = BuiltinType Int }] }}}}] }}}}] -> ()
  | _ -> assert false

let example = {|
  template <template<int... > class T>
    class C {
      T<1, 2, 3> x;
    };
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
     [{ desc = TemplateDecl {
       parameters = [{ desc = {
         parameter_name = "T";
         parameter_kind = Template {
           parameters = [{ desc = {
             parameter_name = "";
             parameter_kind = NonType {
               parameter_type = { desc = BuiltinType Int };
               default = None };
             parameter_pack = true }}];
           default = None };
         parameter_pack = false }}];
       decl = { desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Field {
               name = "x";
               qual_type = { desc = TemplateSpecialization {
                 name = NameTemplate { desc = TemplateTemplateParameter "T" };
                 arguments = [
                   ExprTemplateArgument { desc = IntegerLiteral (Int 1) };
                   ExprTemplateArgument { desc = IntegerLiteral (Int 2) };
                   ExprTemplateArgument { desc = IntegerLiteral (Int 3) }]
            }}}}] }}}}]]
    ]}*)

(** {3 Translation units} *)

and translation_unit = (translation_unit_desc, qual_type) open_node

and translation_unit_desc = {
    filename : string; items : decl list
  }
    [@@deriving show, eq, ord]

type 'a node = ('a, qual_type) open_node

type decoration = qual_type open_decoration

(*{[
let () =
  Printf.eprintf "%d success%s and %d failure%s.\n"
    !success_count (if !success_count > 1 then "es" else "")
    !failure_count (if !failure_count > 1 then "s" else "");
  if !failure_count > 0 then
    exit 1
 ]}*)
