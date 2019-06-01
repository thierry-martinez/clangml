let lift_expr = new Clangml_lift.lift_expr Location.none

let check_pattern (quoter : 'obj -> Parsetree.expression) (x : 'obj)
    (pattern : ('obj, 'a) Pattern_runtime.matcher) =
  match pattern ~quoted:(quoter x) x with
  | Ok result -> result
  | Error failure ->
      Format.fprintf Format.err_formatter "%a@."
        Pattern_runtime.pp_failure failure;
      failwith "check_pattern_expr"

let check_pattern_expr expr pattern = check_pattern lift_expr#expr expr pattern

let check_pattern_decl decl pattern = check_pattern lift_expr#decl decl pattern

let check_pattern_tu expr tu = check_pattern lift_expr#translation_unit expr tu

let parse_string ?(command_line_args = []) s =
  let command_line_args =
    Clang.Command_line.include_directory Clang.includedir ::
    Clang.Command_line.language CXX ::
    command_line_args in
  let ast = Clang.Ast.parse_string ~command_line_args s in
  let tu = Clang.Ast.cursor_of_node ast |> Clang.cursor_get_translation_unit in
  Clang.seq_of_diagnostics tu |> Seq.iter (fun diagnostics ->
    prerr_endline (Clang.format_diagnostic diagnostics
      Clang.Cxdiagnosticdisplayoptions.display_source_location));
  assert (not (Clang.has_severity Clang.error tu));
  ast

(* 5.1.2 This *)

let () =
  let ast = parse_string {|
    class Outer {
      unsigned int sz = sizeof(*this);
      void f() {
        int b[sizeof(*this)];
      }
    };
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      class Outer {
        unsigned int sz = [%int? sz];
        void f() {
          [%decl? d]
        }
      };
    |}]] in
  check_pattern_expr bindings#sz [%pattern? {
    desc = UnaryExpr {
      kind = SizeOf;
      argument = ArgumentExpr { desc = UnaryOperator {
        kind = Deref;
        operand = { desc = This }}}}}];
  check_pattern_decl bindings#d [%pattern? {
    desc = Var {
      var_name = "b";
      var_type = { desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 4 }}}}]

(* 5.1.5 Lambda expressions *)

(* C++ 17: auto in function return type *)

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
      ~command_line_args:[Clang.Command_line.standard Cxx17] {|
      void f() {
        auto x1 = [](int i){ return i; };
        int j;
        auto x3 = [=]()->auto&& { return j; };
      }
    |} in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++17") {|
        void f() {
          auto x1 = [%(void *(*)())? x1];
          int j;
          auto x3 = [%(void *(*)())? x3];
        }
      |}]] in
    check_pattern_expr bindings#x1 [%pattern? {
      desc = Lambda {
        capture_default = CaptureNone;
        captures = [];
        parameters = Some [{
          desc = {
            qual_type = { desc = BuiltinType Int };
            name = "i"; }}];
        result_type = None;
        body = { desc = Compound [{ desc =
          Return (Some { desc = DeclRef (Ident "i")})}]}}}];
    check_pattern_expr bindings#x3 [%pattern? {
      desc = Lambda {
        capture_default = ByCopy;
        captures = [{
          capture_kind = ByCopy;
          implicit = true;
          captured_var_name = Some "j";
          pack_expansion = false; }];
        parameters = Some [];
        result_type =
          Some { desc = LValueReference { desc = BuiltinType Int }};
        body = { desc = Compound [{ desc =
          Return (Some { desc = DeclRef (Ident "j")})}]}}}]
  end]

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
        ~command_line_args:[Clang.Command_line.standard Cxx17] "
      template<class... Args>
      void f(Args... args) {
        auto lm = [&, args...] { return g(args...); };
        lm();
      }
    " in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++17") {|
        template<class... Args>
        void f(Args... args) {
          auto lm = [%(void *(*)())? lm];
          lm();
        }
      |}]] in
    check_pattern_expr bindings#lm [%pattern? {
      desc = Lambda {
        capture_default = ByRef;
        captures = [{
          capture_kind = ByCopy;
          implicit = false;
          captured_var_name = Some "args";
          pack_expansion = true; }];
        parameters = None;
        result_type = None;
        body = { desc = Compound [{ desc =
          Return (Some { desc = Call {
            callee = { desc = DeclRef (Ident "g")};
            args = [{ desc = PackExpansionExpr {
              desc = DeclRef (Ident "args")}}]}})}]}}}]
  end]

(* 5.1.6 Fold expressions *)

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
        ~command_line_args:[Clang.Command_line.standard Cxx17] {|
      template<typename ...Args>
      bool f(Args ...args) {
        return (true && ... && args);
      }
    |} in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++17") {|
        template<typename ...Args>
        bool f(Args ...args) {
          return [%bool? fold];
        }
      |}]] in
    check_pattern_expr bindings#fold [%pattern? {
      desc = Fold {
        lhs = { desc = BoolLiteral true };
        operator = LAnd;
        rhs = { desc = DeclRef (Ident "args") }}}]
  end]

(* 5.2.8 Type identification *)

let () =
  let ast = parse_string {|
    #include <typeinfo>
    class D { public: D() {} };
    D d1;
    const D d2;

    void f() {
      typeid(d1) == typeid(d2);
      typeid(D) == typeid(const D);
      typeid(D) == typeid(d2);
      typeid(D) == typeid(const D&);
    }
  |} in
  match List.rev ast.desc.items with
  | f :: _ ->
    let bindings =
      check_pattern_decl f [%pattern? [%cpp-decl {|
        void f() {
          [%bool? e1];
          [%bool? e2];
          [%bool? e3];
          [%bool? e4];
        }
      |}]] in
    check_pattern_expr bindings#e1 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef (BinaryOperatorRef EQ)};
        args = [
          { desc = Typeid (ArgumentExpr { desc = DeclRef (Ident "d1")})};
          { desc = Typeid (ArgumentExpr { desc = DeclRef (Ident "d2")})}]}}];
    check_pattern_expr bindings#e2 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef (BinaryOperatorRef EQ)};
        args = [
          { desc = Typeid (ArgumentType { desc = Record (Ident "D")})};
          { desc = Typeid (ArgumentType { desc = Record (Ident "D")})}]}}];
    check_pattern_expr bindings#e3 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef (BinaryOperatorRef EQ)};
        args = [
          { desc = Typeid (ArgumentType { desc = Record (Ident "D")})};
          { desc = Typeid (ArgumentExpr { desc = DeclRef (Ident "d2")})}]}}];
    check_pattern_expr bindings#e4 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef (BinaryOperatorRef EQ)};
        args = [
          { desc = Typeid (ArgumentType { desc = Record (Ident "D")})};
          { desc = Typeid (ArgumentType { desc = Record (Ident "D")})}]}}]
  | _ -> assert false

(* 5.3.3 Sizeof *)

let () =
  let ast = parse_string {|
    #include <cstdlib>

    template<class... Types>
    struct count {
      static const std::size_t value = sizeof...(Types);
    };
  |} in
  match List.rev ast.desc.items with
  | d :: _ ->
    let bindings =
      check_pattern_decl d [%pattern? [%cpp-decl
      {|
        #include <cstdlib>
      |} {|
        template<class... Types>
        struct count {
          static const std::size_t value = [%(std::size_t)? sz];
        };
      |}]] in
    check_pattern_expr bindings#sz [%pattern? {
      desc = SizeOfPack (Ident "Types")}]
  | _ -> assert false

(* 5.3.4 New *)

let () =
  let ast = parse_string
    ~command_line_args:[Clang.Command_line.standard Cxx11] {|
    void f() {
      new auto(1);
      auto x = new auto('a');
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++11") {|
      void f() {
        [%(void *)? n1];
        auto x = [%(void *)? n2];
      }
    |}]] in
  check_pattern_expr bindings#n1 [%pattern? {
    desc = New {
      qual_type = { desc = Auto };
      init = Some { desc = IntegerLiteral (Int 1) }}}];
  check_pattern_expr bindings#n2 [%pattern? {
    desc = New {
      qual_type = { desc = Auto };
      init = Some { desc = CharacterLiteral { kind = Ascii; value = 97 }}}}]

let () =
  let ast = parse_string {|
    void f() {
      new (int (*[10])());
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++11") {|
      void f() {
        [%(void *)? n];
      }
    |}]] in
  check_pattern_expr bindings#n [%pattern? {
    desc = New {
      qual_type = { desc = Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some {
          non_variadic = [];
          variadic = false; }}}};
      init = None}}]

let () =
  let ast = parse_string {|
    struct T {
      static void* operator new(unsigned long sz);
      static void* operator new(unsigned long sz, int arg, void (*f)());
      static void* operator new[](unsigned long sz);
      static void* operator new[](unsigned long sz, int arg, void (*f)());
    };
    void f() {}
    void g() {
      new T;
      new(2,f) T;
      new T[5];
      new(2,f) T[5];
    }
  |} in
  match List.rev ast.desc.items with
  | d :: _ ->
    let bindings =
      check_pattern_decl d [%pattern? [%cpp-decl {|
        void g() {
          [%(void *)? n1];
          [%(void *)? n2];
          [%(void *)? n3];
          [%(void *)? n4];
        }
      |}]] in
    check_pattern_expr bindings#n1 [%pattern? {
      desc = New {
        placement_args = [];
        qual_type = { desc = Record (Ident "T")};
        array_size = None;
        init = None }}];
    check_pattern_expr bindings#n2 [%pattern? {
      desc = New {
        placement_args = [
          { desc = IntegerLiteral (Int 2)};
          { desc = DeclRef (Ident "f") }];
        qual_type = { desc = Record (Ident "T")};
        array_size = None;
        init = None }}];
    check_pattern_expr bindings#n3 [%pattern? {
      desc = New {
        placement_args = [];
        qual_type = { desc = Record (Ident "T")};
        array_size = Some { desc = IntegerLiteral (Int 5) };
        init = None }}];
    check_pattern_expr bindings#n4 [%pattern? {
      desc = New {
        placement_args = [
          { desc = IntegerLiteral (Int 2)};
          { desc = DeclRef (Ident "f") }];
        qual_type = { desc = Record (Ident "T")};
        array_size = Some { desc = IntegerLiteral (Int 5) };
        init = None }}]
  | _ -> assert false

(* 5.5 Pointer-to-member operators *)

let () =
  let ast = parse_string {|
    struct S {
      S() : i(0) { }
      mutable int i;
    };

    void f() {
      const S cs;
      int S::* pm = &S::i;
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      struct S {
        S() : i(0) { }
        mutable int i;
      };

      void f() {
        const S cs;
        [%decl? d]
      }
    |}]] in
  check_pattern_decl bindings#d [%pattern? {
    desc = Var {
      var_type = { desc = MemberPointer {
        class_ = { desc = Record (Ident "S")};
        pointee = { desc = BuiltinType Int }}};
      var_name = "pm";
      var_init = Some { desc = UnaryOperator {
        kind = AddrOf;
        operand = {
          desc = DeclRef TypeRef {
            type_ref = Ident "S";
            qual_type = { desc = Record (Ident "S")};
            ident = "i" }}}}}}]

let () =
  let ast = parse_string {|
    struct S {
      void f(int) const;
      S() {};
    };
    void f() {
      const S cs;
      const S* ptr_to_obj = &cs;
      void (S::*ptr_to_mfct)(int) const = &S::f;
      (ptr_to_obj->*ptr_to_mfct)(10);
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      struct S {
        void f(int) const;
        S() {};
      };
      void f() {
        const S cs;
        const S* ptr_to_obj = &cs;
        [%decl? d]
        [%int? e];
      }
    |}]] in
  check_pattern_decl bindings#d [%pattern? {
    desc = Var {
      var_type = { desc = MemberPointer {
        pointee = { desc = FunctionType {
          result = { desc = BuiltinType Void };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = BuiltinType Int }}}]}}};
        class_ = { desc = Record (Ident "S")}}};
      var_name = "ptr_to_mfct";
      var_init = Some { desc = UnaryOperator {
        kind = AddrOf;
        operand = {
          desc = DeclRef TypeRef {
            type_ref = Ident "S";
            qual_type = { desc = Record (Ident "S")};
            ident = "f" }}}}}}];
  check_pattern_expr bindings#e [%pattern? {
    desc = Call {
      callee = { desc = BinaryOperator {
        lhs = { desc = DeclRef (Ident "ptr_to_obj")};
        kind = PtrMemI;
        rhs = { desc = DeclRef (Ident "ptr_to_mfct")}}};
      args = [{ desc = IntegerLiteral (Int 10)}]}}]

(* 5.19 Constant expression *)

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
      ~command_line_args:[Clang.Command_line.standard Cxx17] {|
      int x;

      struct A {
        constexpr A(bool b) : m(b?42:x) { }
        int m;
      };

      constexpr int v = A(true).m;

      constexpr int f2(int k) {
        int x = k;
        return x;
      }

      constexpr int incr(int &n) {
        return ++n;
      }

      constexpr int h(int k) {
        int x = incr(k);
        return x;
      }

      constexpr int y = h(1);
    |} in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu {|
        int x;
        struct A {
          [%decl? a]
          int m;
        };
        [%decl? v]
        [%decl? f2]
        [%decl? incr]
        [%decl? h]
        [%decl? y]
      |}]] in
    check_pattern_decl bindings#a [%pattern? {
      desc = Constructor {
        class_name = "A";
        parameters = {
          non_variadic = [{ desc = {
            qual_type = { desc = BuiltinType Bool };
            name = "b" }}]};
        initializer_list = [("m", {
          desc = ConditionalOperator {
            cond = { desc = DeclRef (Ident "b")};
            then_branch = Some { desc = IntegerLiteral (Int 42)};
            else_branch = { desc = DeclRef (Ident "x")}}})];
        body = Some { desc = Compound []};
        constexpr = true; }}];
    check_pattern_decl bindings#v [%pattern? {
      desc = Var {
        var_name = "v";
        var_type = { desc = BuiltinType Int };
        var_init = Some { desc = Member {
          base = { desc = Cast {
            kind = Functional;
            qual_type = { desc = Record (Ident "A") };
            operand = { desc = Construct {
              name = "A";
              args = [{ desc = BoolLiteral true }]}}}};
          arrow = false;
          field = { desc = "m" }}};
        constexpr = true }}];
    check_pattern_decl bindings#f2 [%pattern? {
      desc = Function {
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = BuiltinType Int };
              name = "k" }}]}};
        name = "f2";
        constexpr = true }}];
    check_pattern_decl bindings#incr [%pattern? {
      desc = Function {
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = LValueReference { desc = BuiltinType Int }};
              name = "n" }}]}};
        name = "incr";
        constexpr = true }}];
    check_pattern_decl bindings#h [%pattern? {
      desc = Function {
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = BuiltinType Int };
              name = "k" }}]}};
        name = "h";
        constexpr = true }}];
    check_pattern_decl bindings#y [%pattern? {
      desc = Var {
        var_type = { desc = BuiltinType Int };
        var_name = "y";
        var_init = Some { desc = Call {
          callee = { desc = DeclRef (Ident "h")};
          args = [{ desc = IntegerLiteral (Int 1)}]; }};
        constexpr = true }}]
  end]
