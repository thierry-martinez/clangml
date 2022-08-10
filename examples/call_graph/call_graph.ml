module StringMap = Map.Make (String)

module StringSet = Set.Make (String)

module CallSet = struct
  type t = StringSet.t

  let zero = StringSet.empty

  let ( + ) = StringSet.union
end

let call_set_of_function_body body =
  let module CallVisitor : Refl.Visit.VisitorS
  with type 'a Applicative.t = CallSet.t = struct
    module Applicative = Traverse.Applicative.Reduce (CallSet)

    let hook : type a . a Refl.refl -> (a -> CallSet.t) -> a -> CallSet.t =
    fun refl super x ->
      let call_set = super x in
      match refl with
      | Clang.Ast.Refl_expr ->
        begin match x.desc with
        | Call { callee; _ } ->
            begin match callee.desc with
            | DeclRef { name = IdentifierName name; _ } ->
                StringSet.add name call_set
            | _ ->
                Format.eprintf
                  "%a warning: call to anonymous function is ignored@."
                  (Clang.Ast.pp_source_location ?options:None ?ranges:None)
                  (Clang.Ast.location_of_node x);
                call_set
            end
        | _ ->
            call_set
        end
      | _ -> call_set
  end in
  let module Visit : Refl.Visit.VisitS with
  type 'a Visitor.t = 'a -> CallSet.t =
      Refl.Visit.Make (CallVisitor) in
  Visit.visit [%refl: Clang.Ast.stmt] [] body

module CallGraph = struct
  type t = StringSet.t StringMap.t

  let zero = StringMap.empty

  let ( + ) = StringMap.union (fun _key s1 s2 -> Some (StringSet.union s1 s2))

  let pp fmt call_graph =
    call_graph |> StringMap.iter (fun src s ->
      s |> StringSet.iter (fun tgt ->
        Format.fprintf fmt "@[%s@ ->@ %s@]@," src tgt))
end

module rec FunctionVisitor : Refl.Visit.VisitorS
with type 'a Applicative.t = CallGraph.t = struct
  module Applicative = Traverse.Applicative.Reduce (CallGraph)

  let hook : type a . a Refl.refl -> (a -> CallGraph.t) -> a -> CallGraph.t =
  fun refl super x ->
    let call_graph = super x in
    match refl with
    | Clang.Ast.Refl_function_decl ->
        begin match x.body with
        | None -> call_graph
        | Some body ->
            let name =
              match x.name with
              | IdentifierName name -> name
              | _ -> failwith "IdentifierName expected" in
            StringMap.singleton name (call_set_of_function_body body)
        end
    | _ ->
        call_graph
end
and VisitFunctions : Refl.Visit.VisitS with
  type 'a Visitor.t = 'a -> CallGraph.t =
      Refl.Visit.Make (FunctionVisitor)

let analyze tu =
  VisitFunctions.visit [%refl: Clang.Ast.translation_unit] []
    (Clang.Ast.of_cxtranslationunit tu)

let main files =
  Clangml_tools_common.command_line (fun _language command_line_args ->
    let options = Clang.default_editing_translation_unit_options () in
    files |> List.iter (fun file ->
      let tu = Clang.parse_file ~command_line_args file ~options in
      let call_graph = analyze tu in
      Format.printf "@[<v2>%s:@,@[%a@]@]@." file CallGraph.pp call_graph))

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
  Cmdliner.Cmd.info "call_graph" ~doc ~man

let () =
  exit (Cmdliner.Cmd.eval (Cmdliner.Cmd.v info options))
