let include_directory directory =
  "--include-directory=" ^ directory

let include_barrier = "--include-barrier"

let language lang =
  "--language=" ^ Clang__utils.string_of_language lang
