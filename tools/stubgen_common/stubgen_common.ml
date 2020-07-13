let run_llvm_config llvm_config arguments =
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

let string_remove_prefix ~prefix s =
  let ls = String.length s and lprefix = String.length prefix in
  if lprefix <= ls && String.sub s 0 lprefix = prefix then
    Some (String.sub s lprefix (ls - lprefix))
  else
    None

let string_remove_suffix ~suffix s =
  let ls = String.length s and lsuffix = String.length suffix in
  if lsuffix <= ls && String.sub s (ls - lsuffix) (lsuffix) = suffix then
    Some (String.sub s 0 (ls - lsuffix))
  else
    None

let macos_sdk_include_path =
  "/Library/Developer/CommandLineTools/SDKs/MacOSX10.14.sdk/usr/include/"

let prepare_clang_options cflags llvm_config =
  let llvm_flags, llvm_version =
    match llvm_config with
    | None -> [], None
    | Some llvm_config ->
        let llvm_version = run_llvm_config llvm_config ["--version"] in
        let llvm_prefix = run_llvm_config llvm_config ["--prefix"] in
        let llvm_cflags = run_llvm_config llvm_config ["--cflags"] in
        let llvm_version =
          option_apply (string_remove_suffix ~suffix:"svn") llvm_version in
        let llvm_version =
          option_apply (string_remove_suffix ~suffix:"git") llvm_version in
        let equivalent_llvm_version =
          match llvm_version with
          | "3.4"
          | "3.4.1" -> "3.4.2"
          | "3.5.0"
          | "3.5.1" -> "3.5.2"
          | "3.6.0"
          | "3.6.1" -> "3.6.2"
          | "3.7.0" -> "3.7.1"
          | "3.8.0" -> "3.8.1"
          | "3.9.0" -> "3.9.1"
          | "4.0.0" -> "4.0.1"
          | "5.0.0"
          | "5.0.1" -> "5.0.2"
          | "6.0.0" -> "6.0.1"
          | "7.0.0" -> "7.1.0"
          | "7.0.1" -> "7.1.0"
          | "8.0.0" -> "8.0.1"
          | "9.0.0" -> "9.0.1"
          | "10.0.0" -> "10.0.1"
          | _ -> llvm_version in
        let version_option =
          String.map (fun c -> if c = '.' then '_' else c)
            equivalent_llvm_version in
        String.split_on_char ' ' llvm_cflags @
        ["-I"; List.fold_left Filename.concat llvm_prefix
           ["lib"; "clang"; llvm_version; "include"]; "-I";
         macos_sdk_include_path;
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