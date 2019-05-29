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

let check_pattern_tu expr tu = check_pattern lift_expr#translation_unit expr tu

let run_llvm_config llvm_config arguments =
  let command = String.concat " " (llvm_config :: arguments) in
  let output = Unix.open_process_in command in
  let result = input_line output in
  if Unix.close_process_in output <> Unix.WEXITED 0 then
    failwith (Printf.sprintf "%s: execution failed" command);
  result

let parse_string ?(command_line_args = []) s =
  let llvm_config = Clangml_config.llvm_config in
  let llvm_version = run_llvm_config llvm_config ["--version"] in
  let llvm_include_dir = run_llvm_config llvm_config ["--includedir"] in
  let clang_include_dir =
    List.fold_left Filename.concat llvm_include_dir
      [".."; "lib"; "clang"; llvm_version; "include"] in
  let command_line_args =
    Clang.Command_line.include_directory clang_include_dir ::
    Clang.Command_line.language CXX ::
    command_line_args in
  Clang.Ast.parse_string ~command_line_args s

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
(*
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++17") {|
      class Outer {
|} in
    check_pattern_expr bindings#x1 [%pattern? {
      desc = Lambda {
*)
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

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
          captured_var_name = Some "j" }];
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
    Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast
  end]

(* 5.1.6 Fold expressions *)

let () =
  let ast = parse_string {|
    template<typename ...Args>
    bool f(Args ...args) {
      return (true && ... && args);
    }
  |} in
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

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
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

(* 5.3.3 Sizeof *)

let () =
  let ast = parse_string {|
    template<class... Types>
    struct count {
      static const std::size_t value = sizeof...(Types);
    };
  |} in
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

(* 5.3.4 New *)

let () =
  let ast = parse_string
    ~command_line_args:[Clang.Command_line.standard Cxx11] {|
    void
    f()
    {
      new auto(1);
      auto x = new auto('a');
    }
  |} in
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

let () =
  let ast = parse_string {|
    void
    f()
    {
      new (int (*[10])());
    }
  |} in
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

let () =
  let ast = parse_string {|
    void *malloc(unsigned long);
    struct T {
      static void* operator new(unsigned long sz);
      static void* operator new(unsigned long sz, int arg, void (*f)());
      static void* operator new[](unsigned long sz);
      static void* operator new[](unsigned long sz, int arg, void (*f)());
    };
    void
    f()
    {
    }
    void
    g()
    {
      new T;
      new(2,f) T;
      new T[5];
      new(2,f) T[5];
    }
  |} in
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

(* 5.5 Pointer-to-member operators *)

let () =
  let ast = parse_string {|
    struct S {
      S() : i(0) { }
      mutable int i;
    };

    void
    f()
    {
      const S cs;
      int S::* pm = &S::i;
    }
  |} in
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

let () =
  let ast = parse_string {|
    struct S {
      void f(int) const;
      S() {};
    };
    void
    f()
    {
      const S cs;
      const S* ptr_to_obj = &cs;
      void (S::*ptr_to_mfct)(int) const = &S::f;
      (ptr_to_obj->*ptr_to_mfct)(10);
    }
  |} in
  Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast

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
    Format.fprintf Format.err_formatter "%a@." Clang.Ast.pp_translation_unit ast
  end]
