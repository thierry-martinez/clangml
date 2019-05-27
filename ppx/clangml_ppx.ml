[@@@warning "-30"]

type antiquotation_type =
  | Typename
  | Expression of string

type antiquotation = {
    antiquotation_type : antiquotation_type;
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
        let antiquotation_type =
          match antiquotation_type with
          | "typename" | " class" -> Typename
          | _ -> Expression antiquotation_type in
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
          | Expression antiquotation_type ->
              Printf.sprintf "((%s) %s)" antiquotation_type placeholder in
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

module String_map = Map.Make (String)

let string_of_expression (expression : Parsetree.expression) =
  match expression with
  | { pexp_desc = Pexp_constant (Pconst_string (s, _)); _ } -> s
  | _ ->
      Format.asprintf "%a" Pprintast.expression expression

let rec remove_placeholders antiquotations items =
  match antiquotations, items with
  | [], _ -> items
  | _ :: antiquotations, _ :: items ->
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

let extract_payload language (mapper : Ast_mapper.mapper) ~loc
    (payload : Parsetree.payload) =
  let kind, arguments, code =
    match
      match payload with
      | PStr [%str - [%e? { pexp_desc = Pexp_apply (
            { pexp_desc = Pexp_ident { txt = Lident kind; loc = kind_loc }; _},
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
                string_of_expression code, rev_args in
          let handle_arg arguments (_, (arg : Parsetree.expression)) =
            let loc = arg.pexp_loc in
            match arg with
            | [%expr return [%e? arg ]] ->
                if arguments.return_type <> None then
                  Location.raise_errorf ~loc
                    "Return type already given";
                { arguments with
                  return_type = Some (string_of_expression arg) }
            | _ ->
                let arg = string_of_expression arg in
                match Clang.ext_lang_standard_of_name arg with
                | Invalid ->
                    { arguments with
                      preamble = arg :: arguments.preamble }
                | standard ->
                    if arguments.standard <> None then
                      Location.raise_errorf ~loc
                        "Standard already given";
                    { arguments with standard = Some standard } in
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
    | Expression _ ->
        Buffer.add_string buffer
          (Printf.sprintf "void * const %s = 0;" placeholder));
  let return_type =
    match arguments.return_type with
    | None -> "void"
    | Some return_type -> return_type in
  let function_declaration =
    Printf.sprintf "%s f(void) {" return_type in
  let prelude, prelude', postlude,
    (extraction :
       _ -> Clang.Ast.translation_unit -> _) =
    match kind with
    | Expr ->
        function_declaration, "", ";}",
        begin fun lift ast ->
          match List.rev ast.desc.items with
          | { desc = Function
                { body = Some {
                  desc = Compound stmts; _}; _}; _} :: _ ->
                    begin match List.rev stmts with
                    | { desc = Expr item; _ } :: _ ->
                        lift#expr item
                    | _ -> assert false
                    end
          | _ -> assert false
        end
    | Stmt ->
        function_declaration, "", "}",
        begin fun lift ast ->
          match List.rev ast.desc.items with
          | { desc = Function
                { body = Some
                    { desc = Compound [item]; _}; _}; _} :: _ ->
                      lift#stmt item
          | _ -> assert false
        end
    | Qual_type ->
        "void f(void) {", "void *x; (", ") x; }",
        begin fun lift ast ->
          match List.rev ast.desc.items with
          | { desc = Function
                { body = Some {
                  desc = Compound [_;
                    { desc = Expr
                        { desc = Cast { qual_type; _ };
                          _}; _}]; _}; _}; _} :: _ ->
                            lift#qual_type qual_type
          | _ -> assert false
        end
    | Decl ->
        "", "", "", begin fun lift ast ->
          lift#decl
            begin match
              List.rev (remove_placeholders antiquotations ast.desc.items) with
            | result :: _ -> result
            | _ -> assert false
            end
        end
    | Translation_unit ->
        "", "", "", begin fun lift ast ->
          lift#translation_unit
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
  let ast =
    Clang.Ast.parse_string
      ~command_line_args code in
  let tu = Clang.Ast.cursor_of_node ast |> Clang.cursor_get_translation_unit in
  let has_diagnostics = ref false in
  Clang.seq_of_diagnostics tu |> Seq.iter begin fun diagnostics ->
    if
      List.mem (Clang.get_diagnostic_severity diagnostics) Clang.error then
      begin
        if not !has_diagnostics then
          begin
            Format.fprintf Format.err_formatter
              "@[%a@]@[Compiling quotation code:@ %s@]@."
              Location.print loc code;
            has_diagnostics := true;
          end;
        prerr_endline
          (Clang.format_diagnostic diagnostics
            Clang.Cxdiagnosticdisplayoptions.display_source_location)
      end
  end;
  if Clang.has_severity Clang.error tu then
    Location.raise_errorf ~loc "clang error while compiling quotation code";
  let placeholder_map = List.fold_left (fun map antiquotation ->
    String_map.add antiquotation.placeholder
      { antiquotation.payload with
        txt = mapper.payload mapper antiquotation.payload.txt }
      map) String_map.empty antiquotations in
  ast, extraction, placeholder_map

module type Lifter = sig
  type t

  class lifter : object
    inherit [t] Clangml_lift.lift
    inherit [t] Ppxlib_traverse_builtins.std_lifters
  end
end

module Remove_placeholder (X : Lifter) = struct
  type t = X.t

  class lifter subst_payload map = object
    inherit X.lifter as super

    method expr (expr : Clang.Ast.expr) =
      match
        match expr with
        | { desc =
              Cast { operand = { desc = DeclRef (Ident ident); _ }; _ }; _ } ->
            String_map.find_opt ident map
        | _ -> None
      with
      | Some payload -> subst_payload payload
      | None -> super#expr expr

    method qual_type (qual_type : Clang.Ast.qual_type) =
      match
        match qual_type with
        | { desc = Typedef (Ident ident) } ->
            String_map.find_opt ident map
        | _ -> None
      with
      | Some payload -> subst_payload payload
      | None -> super#qual_type qual_type
  end
end

let rec expr_mapper (mapper : Ast_mapper.mapper) (expr : Parsetree.expression) =
  match
    match expr.pexp_desc with
    | Pexp_extension ({ loc; txt }, payload) ->
        begin match Clang.language_of_string txt with
        | exception (Invalid_argument _) -> None
        | language -> Some (language, loc, payload)
        end
    | _ -> None
  with
  | None ->
      Ast_mapper.default_mapper.expr mapper expr
  | Some (language, loc, payload) ->
      let ast, extraction, placeholder_map =
        extract_payload language mapper ~loc payload in
      let module Expr_remove = Remove_placeholder
          (struct
            type t = Parsetree.expression

            class lifter = object
              inherit Clangml_lift.lift_expr Location.none
            end
          end) in
      let subst_payload (payload : Parsetree.payload Location.loc) =
        match payload.txt with
        | PStr [{ pstr_desc = Pstr_eval (e, _); _ }] ->
            e
        | _ ->
            raise (Location.Error (Location.error ~loc:payload.loc
              "Expression expected in anti-quotation")) in
      extraction (new Expr_remove.lifter subst_payload placeholder_map) ast

and pat_mapper (mapper : Ast_mapper.mapper) (pat : Parsetree.pattern) =
  match
    match pat.ppat_desc with
    | Ppat_extension ({ loc; txt }, payload) ->
        begin match Clang.language_of_string txt with
        | exception (Invalid_argument _) -> None
        | language -> Some (language, loc, payload)
        end
    | _ -> None
  with
  | None ->
    Ast_mapper.default_mapper.pat mapper pat
  | Some (language, loc, payload) ->
      let ast, extraction, placeholder_map =
        extract_payload language mapper ~loc payload in
      let module Pat_remove = Remove_placeholder
          (struct
            type t = Parsetree.pattern

            class lifter = object
              inherit Clangml_lift.lift_pattern Location.none
            end
          end) in
      let subst_payload (payload : Parsetree.payload Location.loc) =
        match payload.txt with
        | PPat (p, None) ->
            p
        | _ ->
            raise (Location.Error (Location.error ~loc:payload.loc
              "Pattern expected in anti-quotation")) in
      extraction (new Pat_remove.lifter subst_payload placeholder_map) ast

let ppx_pattern_mapper = {
  Ast_mapper.default_mapper with
  expr = expr_mapper;
  pat = pat_mapper
}

let () =
  Migrate_parsetree.Driver.register ~name:"clangml.ppx" ~position:(-10)
    (module Migrate_parsetree.OCaml_current)
    (fun _ _ -> ppx_pattern_mapper)
