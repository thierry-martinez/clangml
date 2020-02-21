let placeholder_hashtbl_sz = 17

type antiquotation = {
    antiquotation_type : Ppx_lexer.antiquotation_type;
    placeholder : string;
    payload : Parsetree.payload Location.loc;
    pos_begin : int;
    pos_end : int;
  }

let extract_antiquotations s =
  let code_buffer = Buffer.create (String.length s) in
  let antiquotations_ref = ref [] in
  let antiquotation_count_ref = ref 0 in
  let lexbuf = Lexing.from_string s in
  let rec copy_to_result () =
    let start_pos = lexbuf.lex_curr_p.pos_cnum in
    let has_antiquotation = Ppx_lexer.main lexbuf in
    let read_code_length = lexbuf.lex_start_p.pos_cnum - start_pos in
    Buffer.add_substring code_buffer s start_pos read_code_length;
    match has_antiquotation with
    | None -> ()
    | Some antiquotation_type ->
        let lbracket_count_ref = ref 0 in
        let pattern, (token_buf : Parser.token option) =
          match Lexer.token lexbuf with
          | QUESTION -> true, None
          | token -> false, Some token in
        let token_buf_ref = ref token_buf in
        let lexer lexbuf : Parser.token =
          let token =
            match !token_buf_ref with
            | None -> Lexer.token lexbuf
            | Some token_buf ->
                token_buf_ref := None;
                token_buf in
          match token with
          | LBRACKET
          | LBRACKETAT
          | LBRACKETATAT
          | LBRACKETATATAT
          | LBRACKETPERCENT
          | LBRACKETPERCENTPERCENT ->
              incr lbracket_count_ref;
              token
          | RBRACKET ->
              let lbracket_count = !lbracket_count_ref in
              if lbracket_count = 0 then
                EOF
              else
                begin
                  lbracket_count_ref := lbracket_count - 1;
                  token
                end
          | _ -> token in
        let loc_start = lexbuf.lex_curr_p in
        let payload : Parsetree.payload =
          if pattern then PPat (Parser.parse_pattern lexer lexbuf, None)
          else PStr (Parser.implementation lexer lexbuf) in
        let loc_end = lexbuf.lex_curr_p in
        let loc = { Location.loc_start; loc_end; loc_ghost = false } in
        let payload = { Location.loc; txt = payload } in
        let index = !antiquotation_count_ref in
        antiquotation_count_ref := succ index;
        let placeholder = Printf.sprintf "__antiquotation_%d" index in
        let placeholder_code =
          match antiquotation_type with
          | Typename -> placeholder
          | Decl -> Printf.sprintf "int %s;" placeholder
          | Expression antiquotation_type ->
              Printf.sprintf "(\"%s\", (%s) 0)" placeholder
                antiquotation_type in
        Buffer.add_char code_buffer ' ';
        let pos_begin = Buffer.length code_buffer in
        Buffer.add_string code_buffer placeholder_code;
        let pos_end = Buffer.length code_buffer in
        Buffer.add_char code_buffer ' ';
        antiquotations_ref :=
          { antiquotation_type; placeholder; payload; pos_begin; pos_end } ::
          !antiquotations_ref;
        copy_to_result () in
  copy_to_result ();
  Buffer.contents code_buffer, List.rev !antiquotations_ref

type kind =
  | Expr
  | Qual_type
  | Decl
  | Stmt
  | Translation_unit

let kind_of_string s =
  match s with
  | "expr" | "e" -> Expr
  | "qual_type" | "type" | "t" -> Qual_type
  | "decl" | "d" -> Decl
  | "stmt" | "s" -> Stmt
  | "translation_unit" | "tu" -> Translation_unit
  | _ -> invalid_arg "kind_of_string"

module String_hashtbl = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash = Hashtbl.hash
end)

let rec remove_placeholders antiquotations items =
  match antiquotations, items with
  | [], _ -> items
  | { antiquotation_type = Typename; _ } :: antiquotations, _ :: items ->
      remove_placeholders antiquotations items
  | { antiquotation_type = (Decl | Expression _); _ } :: antiquotations,
    items ->
      remove_placeholders antiquotations items
  | _ -> assert false

type arguments = {
    preamble : string list;
    standard : Clang.standard option;
    return_type : string option;
  }

let empty_arguments = {
  preamble = [];
  standard = None;
  return_type = None;
}

let find_and_remove table ident =
  let result = String_hashtbl.find_opt table ident in
  if result <> None then
    String_hashtbl.remove table ident;
  result

let is_empty table =
  String_hashtbl.length table = 0

type extension =
  | Quotation of {
      language : Clang.language;
      payload : Parsetree.payload;
      loc : Location.t;
    }
  | If_standard of {
      name : string;
      expr : Parsetree.expression;
      loc : Location.t;
    }

module Make (Target : Metapp.ValueS) = struct
  module Lift = Refl.Lift.Make (Target)

  let extract_payload language (mapper : Ast_mapper.mapper) ~loc
      (payload : Parsetree.payload) =
    let kind, arguments, code =
      match
        match payload with
        | PStr [%str - [%e? { pexp_desc = Pexp_apply (
              { pexp_desc =
                  Pexp_ident { txt = Lident kind; loc = kind_loc }; _},
                args); _ }]] ->
            let kind =
              try kind_of_string kind
              with Invalid_argument _ ->
                Location.raise_errorf ~loc:kind_loc "unknown kind" in
            let code, rev_args =
              match List.rev args with
              | [] ->
                  Location.raise_errorf ~loc "Code expected"
              | (_, code) :: rev_args ->
                  Metapp.string_of_arbitrary_expression code, rev_args in
            let handle_arg arguments (_, (arg : Parsetree.expression)) =
              let loc = arg.pexp_loc in
              match arg with
              | [%expr return [%e? arg ]] ->
                  if arguments.return_type <> None then
                    Location.raise_errorf ~loc
                      "Return type already given";
                  { arguments with
                    return_type =
                      Some (Metapp.string_of_arbitrary_expression arg) }
              | [%expr standard [%e? arg ]] ->
                  if arguments.standard <> None then
                    Location.raise_errorf ~loc
                      "Standard already given";
                  let arg =
                    arg |> Metapp.string_of_arbitrary_expression in
                  let standard = arg |> Clang.ext_lang_standard_of_name in
                  if standard = InvalidLang then
                    Location.raise_errorf ~loc
                      "Unknown standard: %s" arg;
                  { arguments with standard = Some standard }
              | _ ->
                  let arg = Metapp.string_of_arbitrary_expression arg in
                  { arguments with
                    preamble = arg :: arguments.preamble } in
            let arguments =
              List.fold_left handle_arg empty_arguments rev_args in
            Some (kind, arguments, code)
        | _ -> None
      with
      | Some (kind, arguments, code) ->
          kind, arguments, code
      | None ->
          Location.raise_errorf ~loc
            "ClangML quotations have to be of the form [%%c-kind {| code |}]" in
    let code, antiquotations = extract_antiquotations code in
    let buffer = Buffer.create (String.length code) in
    antiquotations |> List.iter (
    fun { antiquotation_type; placeholder; _ } ->
      match antiquotation_type with
      | Typename ->
          Buffer.add_string buffer
            (Printf.sprintf "typedef void *%s;" placeholder);
      | Decl | Expression _ -> ());
    let return_type =
      match arguments.return_type with
      | None -> "void"
      | Some return_type -> return_type in
    let function_declaration =
      Printf.sprintf "%s f(void) {" return_type in
    let placeholder_hashtbl = String_hashtbl.create placeholder_hashtbl_sz in
    let hook : type a . a Refl.type_name -> (a -> Target.t) -> a -> Target.t =
      fun name lifter x ->
        match
          match name, x with
          | Clang.Ast.Name_expr, { desc = BinaryOperator {
              lhs = { desc = StringLiteral { bytes } };
              kind = Comma;
              rhs = { desc = Cast { operand =
                { desc = IntegerLiteral (Int 0); _ }; _ }; _ }}; _} ->
              find_and_remove placeholder_hashtbl bytes
          | Clang.Ast.Name_decl, (
                { desc = Var {
                  var_name = ident;
                  var_type = { desc = BuiltinType Int }}}
              | { desc = Field {
                  name = ident;
                  qual_type = { desc = BuiltinType Int }}}) ->
              find_and_remove placeholder_hashtbl ident
          | Clang.Ast.Name_qual_type,
            { desc = Typedef { name = IdentifierName ident }} ->
              find_and_remove placeholder_hashtbl ident
          | _ -> None
        with
        | Some loc_payload ->
            Metapp.with_loc Target.of_payload loc_payload
        | None ->
            match name with
            | Clang.Ast.Name_opaque_type_loc ->
                Target.choice
                  (fun () -> [%expr None])
                  (fun () -> [%pat? _])
            | Clang.Ast.Name_opaque_cxtype ->
                Target.choice
                  (fun () -> [%expr get_cursor_type (get_null_cursor ())])
                  (fun () -> [%pat? _])
            | Clang.Ast.Name_open_decoration ->
                Target.choice
                  (fun () ->
                    [%expr Custom { location = None; qual_type = None }])
                  (fun () -> [%pat? _])
            | _ -> lifter x in
    let hook = { Lift.hook } in
    let prelude, prelude', postlude,
      (extraction : Clang.Ast.translation_unit -> Target.t) =
      match kind with
      | Expr ->
          function_declaration, "", ";}",
          begin fun ast ->
            match List.rev ast.desc.items with
            | { desc = Function
                  { body = Some {
                    desc = Compound stmts; _}; _}; _} :: _ ->
                      begin match List.rev stmts with
                      | { desc = Expr item; _ } :: _ ->
                          Lift.lift ~hook [%refl: Clang.Ast.expr] [] item
                      | _ -> assert false
                      end
            | _ -> assert false
          end
      | Stmt ->
          function_declaration, "", "}",
          begin fun ast ->
            match List.rev ast.desc.items with
            | { desc = Function
                  { body = Some
                      { desc = Compound [item]; _}; _}; _} :: _ ->
                        Lift.lift ~hook [%refl: Clang.Ast.stmt] [] item
            | _ -> assert false
          end
      | Qual_type ->
          "void f(void) {", "void *x; (", ") x; }",
          begin fun ast ->
            match List.rev ast.desc.items with
            | { desc = Function
                  { body = Some {
                    desc = Compound [_;
                      { desc = Expr
                          { desc = Cast { qual_type; _ };
                            _}; _}]; _}; _}; _} :: _ ->
                              Lift.lift ~hook [%refl: Clang.Ast.qual_type] []
                                qual_type
            | _ -> assert false
          end
      | Decl ->
          "", "", "", begin fun ast ->
            Lift.lift ~hook [%refl: Clang.Ast.decl] []
              begin match
                List.rev (remove_placeholders antiquotations ast.desc.items)
              with
              | result :: _ -> result
              | _ -> assert false
              end
          end
      | Translation_unit ->
          "", "", "", begin fun ast ->
            Lift.lift ~hook [%refl: Clang.Ast.translation_unit] []
              { ast with desc =
                { ast.desc with items =
                  remove_placeholders antiquotations ast.desc.items }}
          end in
    Buffer.add_string buffer prelude;
    arguments.preamble |> List.iter (fun s ->
      Buffer.add_string buffer s;
      Buffer.add_char buffer ';');
    Buffer.add_string buffer prelude';
    Buffer.add_string buffer code;
    Buffer.add_string buffer postlude;
    let code = Buffer.contents buffer in
    let command_line_args = [Clang.Command_line.language language] in
    let command_line_args =
      match arguments.standard with
      | None -> command_line_args
      | Some standard ->
          Clang.Command_line.standard standard :: command_line_args in
    let command_line_args =
      List.map Clang.Command_line.include_directory
        (Clang.default_include_directories ())
      @ command_line_args in
    let ast = Clang.Ast.parse_string ~command_line_args code in
    Clang.Ast.format_diagnostics Clang.error Format.err_formatter ast
      ~pp:begin fun pp fmt () ->
        Format.fprintf fmt
          "@[%a@]@[Compiling quotation code:@ %s@]@ %a@."
          Location.print_loc loc code pp ()
      end;
    if Clang.Ast.has_severity Clang.error ast then
      Location.raise_errorf ~loc "clang error while compiling quotation code";
    antiquotations |> List.iter begin fun antiquotation ->
      String_hashtbl.add placeholder_hashtbl antiquotation.placeholder
        { antiquotation.payload with
          txt = mapper.payload mapper antiquotation.payload.txt }
    end;
    let result = extraction ast in
    assert (is_empty placeholder_hashtbl);
    result

  let mapper (mapper : Ast_mapper.mapper) (t : Target.t) =
    match
      Option.bind (Target.destruct_extension t) (fun e ->
        match e with
        | ({ loc; txt = "if" }, payload) ->
            begin match payload with
            | PStr [%str standard [%e? name] available [%e? expr]] ->
                Some
                  (If_standard {
                     name = Metapp.string_of_arbitrary_expression name;
                     expr; loc })
            | _ -> None
            end
        | ({ loc; txt }, payload) ->
            begin match Clang.language_of_string txt with
            | exception (Invalid_argument _) -> None
            | language -> Some (Quotation { language; loc; payload })
            end)
    with
    | None ->
        Target.mapper.get Ast_mapper.default_mapper mapper t
    | Some (Quotation { language; loc; payload }) ->
        extract_payload language mapper ~loc payload
    | Some (If_standard { name; expr; loc }) ->
        if name |> Clang.ext_lang_standard_of_name = InvalidLang then
          Target.of_unit ()
        else
          let t =
            Target.choice (fun () -> expr) (fun () ->
              failwith "\"if standard\" not available in patterns") in
          Target.mapper.get Ast_mapper.default_mapper mapper t
end

(*
module Remove_placeholder (X : Lifter) = struct
  type t = X.t

  class lifter subst_payload table = object
    inherit X.lifter as super

    method expr (expr : Clang.Ast.expr) =
      match
        match expr with
        | { desc = BinaryOperator {
              lhs = { desc = StringLiteral { bytes } };
              kind = Comma;
              rhs = { desc = Cast { operand =
                { desc = IntegerLiteral (Int 0); _ }; _ }; _ }}; _} ->
            find_and_remove table bytes
        | _ -> None
      with
      | Some payload ->
          subst_payload payload
      | None -> super#expr expr

    method decl (decl : Clang.Ast.decl) =
      match
        match decl with
        | { desc = Var {
            var_name = ident;
            var_type = { desc = BuiltinType Int }}}
        | { desc = Field {
            name = ident;
            qual_type = { desc = BuiltinType Int }}} ->
            find_and_remove table ident
        | _ -> None
      with
      | Some payload ->
          subst_payload payload
      | None -> super#decl decl

    method qual_type (qual_type : Clang.Ast.qual_type) =
      match
        match qual_type with
        | { desc = Typedef { name = IdentifierName ident }} ->
            find_and_remove table ident
        | _ -> None
      with
      | Some payload ->
          subst_payload payload
      | None -> super#qual_type qual_type
  end
end
*)

module MapperExp = Make (Metapp.Exp)

module MapperPat = Make (Metapp.Pat)

let ppx_pattern_mapper = {
  Ast_mapper.default_mapper with
  expr = MapperExp.mapper;
  pat = MapperPat.mapper
}

let () =
  Migrate_parsetree.Driver.register ~name:"clangml.ppx" ~position:(-10)
    (module Migrate_parsetree.OCaml_current)
    (fun _ _ -> ppx_pattern_mapper)
