type version = {
    major : int;
    minor : int;
    patch : int;
  }

val llvm_config : string

val version_string : string

val version : version

val includedir : string
