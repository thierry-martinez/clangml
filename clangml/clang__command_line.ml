let include_directory directory =
  "--include-directory=" ^ directory

let include_barrier = "--include-barrier"

let language lang =
  "--language=" ^ Clang__utils.string_of_language lang

let standard std =
  "--std=" ^ Clang__bindings.ext_lang_standard_get_name std
