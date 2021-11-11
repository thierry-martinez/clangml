let function_body_has_loop body =
  let module CallVisitor : Refl.Visit.VisitorS
  with type 'a Applicative.t = bool = struct
    module Applicative = Traverse.Applicative.Exists

    let hook : type a . a Refl.refl -> (a -> bool) -> a -> bool =
    fun refl super x ->
      match refl with
      | Clang.Ast.Refl_stmt ->
        begin match x.desc with
        | While _
        | For _
        | Do _ ->
            true
        | _ ->
            super x
        end
      | _ -> super x
  end in
  let module Visit : Refl.Visit.VisitS with
  type 'a Visitor.t = 'a -> bool =
      Refl.Visit.Make (CallVisitor) in
  Visit.visit [%refl: Clang.Ast.stmt] [] body

module StringSet = Set.Make (String)

module FunctionSet = struct
  type t = StringSet.t

  let zero = StringSet.empty

  let ( + ) = StringSet.union

  let pp fmt function_set =
    function_set |> StringSet.iter (fun name ->
      Format.fprintf fmt "%s@," name)
end

module rec FunctionVisitor : Refl.Visit.VisitorS
with type 'a Applicative.t = FunctionSet.t = struct
  module Applicative = Traverse.Applicative.Reduce (FunctionSet)

  let hook : type a . a Refl.refl -> (a -> FunctionSet.t) -> a -> FunctionSet.t =
  fun refl super x ->
    let function_set = super x in
    match refl with
    | Clang.Ast.Refl_function_decl ->
        begin match x.body with
        | None -> function_set
        | Some body ->
            let name =
              match x.name with
              | IdentifierName name -> name
              | _ -> failwith "IdentifierName expected" in
            if function_body_has_loop body then
              StringSet.add name function_set
            else
              function_set
        end
    | _ ->
        function_set
end
and VisitFunctions : Refl.Visit.VisitS with
  type 'a Visitor.t = 'a -> FunctionSet.t =
      Refl.Visit.Make (FunctionVisitor)

let analyze tu =
  VisitFunctions.visit [%refl: Clang.Ast.translation_unit] []
    (Clang.Ast.of_cxtranslationunit tu)

let main files =
  Clangml_tools_common.command_line (fun _language command_line_args ->
    let options = Clang.default_editing_translation_unit_options () in
    files |> List.iter (fun file ->
      let tu = Clang.parse_file ~command_line_args file ~options in
      let function_set = analyze tu in
      Format.printf "@[<v2>%s:@,@[<v>%a@]@]@." file
        FunctionSet.pp function_set))

let files =
  let doc = "C source files to analyze" in
  Cmdliner.Arg.(
    value & pos_all non_dir_file [] &
    info [] ~docv:"FILE" ~doc)

let options =
  Clangml_tools_common.options
    Cmdliner.Term.(const main $ files)

let info =
  let doc = "print call graph of given C source files" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "functions_with_loop" ~doc
    ~exits:Cmdliner.Term.default_exits ~man

let () =
  Cmdliner.Term.eval (options, info) |>
  Cmdliner.Term.exit
