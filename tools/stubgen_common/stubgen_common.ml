module Clang_helper = Clang_helper

module String_utils = String_utils

let run_command llvm_config arguments =
  let command = String.concat " " (llvm_config :: arguments) in
  let output = Unix.open_process_in command in
  let result = input_line output in
  if Unix.close_process_in output <> Unix.WEXITED 0 then
    failwith (Printf.sprintf "%s: execution failed" command);
  result

let option_apply f s =
  match f s with
  | None -> s
  | Some result -> result

let macos_sdk_include_path =
  "/Library/Developer/CommandLineTools/SDKs/MacOSX10.14.sdk/usr/include/"

let parse_llvm_version llvm_version : Clangml_config.version option =
  match List.map int_of_string (String.split_on_char '.' llvm_version) with
  | [major; minor] -> Some { major; minor; subminor = 0 }
  | [major; minor; subminor] -> Some { major; minor; subminor }
  | _ | exception Failure _ -> None

let prepare_clang_options cflags llvm_config =
  let llvm_flags, llvm_version =
    match llvm_config with
    | None -> [], None
    | Some llvm_config ->
        let llvm_version = run_command llvm_config ["--version"] in
        let llvm_prefix = run_command llvm_config ["--prefix"] in
        let llvm_cflags = run_command llvm_config ["--cflags"] in
        let llvm_version =
          option_apply (String_utils.remove_suffix ~suffix:"svn") llvm_version in
        let llvm_version =
          option_apply (String_utils.remove_suffix ~suffix:"git") llvm_version in
        let llvm_version =
          option_apply (String_utils.remove_suffix ~suffix:"rc") llvm_version in
        let parsed_llvm_version = parse_llvm_version llvm_version in
        let equivalent_llvm_version =
          match parsed_llvm_version with
          | Some { major = 3; minor = 4; subminor = _ } -> "3.4.2"
          | Some { major = 3; minor = 5; subminor = _ } -> "3.5.2"
          | Some { major = 3; minor = 6; subminor = _ } -> "3.6.2"
          | Some { major = 3; minor = 7; subminor = _ } -> "3.7.1"
          | Some { major = 8; minor = 8; subminor = _ } -> "3.8.1"
          | Some { major = 9; minor = 9; subminor = _ } -> "3.9.1"
          | Some { major = 4; minor = 0; subminor = _ } -> "4.0.1"
          | Some { major = 5; minor = 0; subminor = _ } -> "5.0.2"
          | Some { major = 6; minor = 0; subminor = _ } -> "6.0.1"
          | Some { major = 7; minor = (0 | 1); subminor = _ } -> "7.1.0"
          | Some { major = 8; minor = 0; subminor = _ } -> "8.0.1"
          | Some { major = 9; minor = 0; subminor = _ } -> "9.0.1"
          | Some { major = 10; minor = 0; subminor = _ } -> "10.0.1"
          | Some { major = 11; minor = 0; subminor = 1 } -> "11.1.0"
          | Some { major = 12; minor = 0; subminor = _ } -> "12.0.1"
          | Some { major = 13; minor = 0; subminor = _ } -> "13.0.1"
          | Some { major = 14; minor = 0; subminor = _ } -> "14.0.0"
          | Some { major = 15; minor = 0; subminor = _ } -> "15.0.0"
          | Some { major = 16; minor = 0; subminor = _ } -> "16.0.0"
          | Some { major = 17; minor = 0; subminor = _ } -> "17.0.0"
          | _ -> llvm_version in
        let llvm_version_dir =
          match parsed_llvm_version with
          | Some { major; _ } when major >= 16 -> string_of_int major
          | _ -> llvm_version in
        let version_option =
          String.map (fun c -> if c = '.' then '_' else c)
            equivalent_llvm_version in
        let ocaml_lib = run_command "opam" ["var"; "ocaml:lib"] in
        String.split_on_char ' ' llvm_cflags @
        ["-I"; List.fold_left Filename.concat llvm_prefix
           ["lib"; "clang"; llvm_version_dir; "include"];
         "-I"; macos_sdk_include_path;
         "-I"; ocaml_lib;
         "-DLLVM_VERSION_" ^ version_option],
        Some equivalent_llvm_version in
  let cflags = cflags |> List.map @@ String.split_on_char ',' |> List.flatten in
  cflags @ llvm_flags, llvm_version

let option_cflags =
  let doc = "Pass option to the C compiler" in
  Cmdliner.Arg.(
    value & opt_all string [] & info ["cc"] ~docv:"FLAGS" ~doc)

let option_llvm_config =
  let doc = "Path to llvm-config" in
  Cmdliner.Arg.(
    value & opt (some non_dir_file) None &
    info ["llvm-config"] ~docv:"LLVM_CONFIG" ~doc)

let option_prefix =
  let doc = "Prefix path for output files" in
  Cmdliner.Arg.(
    required & pos 0 (some string) None & info [] ~docv:"PREFIX" ~doc)

let options main = Cmdliner.Term.(
    const main $ option_cflags $ option_llvm_config $ option_prefix)

let uncamelcase s =
  let result = Buffer.create 17 in
  let previous_lowercase = ref false in
  let add_char c =
    match c with
    | 'A' .. 'Z' ->
        if !previous_lowercase then
          begin
            previous_lowercase := false;
            Buffer.add_char result '_'
          end;
        Buffer.add_char result (Char.lowercase_ascii c)
    | '_' ->
        previous_lowercase := false;
        Buffer.add_char result '_'
    | _ ->
        previous_lowercase := true;
        Buffer.add_char result c in
  String.iter add_char s;
  Buffer.contents result

let warning_text tool_name =
  Printf.sprintf "This file is auto-generated by %s tool.
It should not be modified by hand and it should not be versioned
(except by continuous integration on the dedicated bootstrap branch)."
    tool_name

let output_warning_ml channel tool_name =
  Printf.fprintf channel "\
(* %s *)
" (Pcre.replace ~pat:"\n" ~templ:"\n   " (warning_text tool_name))

let output_warning_c channel tool_name =
  Printf.fprintf channel "\
/* %s */
" (Pcre.replace ~pat:"\n" ~templ:"\n * " (warning_text tool_name))
