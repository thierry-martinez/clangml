[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]
[%%metaload "config/clangml_config.cmxs"]

type t =
  | C89
  | C94
  | Gnu89
  | C99
  | Gnu99
  | C11
  | Gnu11
  | C17
  | Gnu17
  | C2x
  | Gnu2x
  | Cxx98
  | Gnucxx98
  | Cxx11
  | Gnucxx11
  | Cxx14
  | Gnucxx14
  | Cxx17
  | Gnucxx17
  | Cxx20
  | Gnucxx20
  | Cxx23
  | Gnucxx23
  | Cxx26
  | Gnucxx26
  | Opencl10
  | Opencl11
  | Opencl12
  | Opencl20
  | Opencl30
  | Openclcpp10
  | Openclcpp2021
  | Cuda
  | Hip
  | Hlsl
  | Hlsl2015
  | Hlsl2016
  | Hlsl2017
  | Hlsl2018
  | Hlsl2021
  | Hlsl202x
      [@@deriving refl]

exception Unavailable of t

let to_clang : t -> Clang__bindings.clang_ext_langstandards = function
  | C89 -> C89
  | C94 -> C94
  | Gnu89 -> Gnu89
  | C99 -> C99
  | Gnu99 -> Gnu99
  | C11 -> C11
  | Gnu11 -> Gnu11
  | C17 ->
      [%meta if Clangml_config.version.major >= 6 then [%expr
        C17]
      else [%expr
        raise (Unavailable C17)]]
  | Gnu17 ->
      [%meta if Clangml_config.version.major >= 6 then [%expr
        Gnu17]
      else [%expr
        raise (Unavailable Gnu17)]]
  | C2x ->
      [%meta if Clangml_config.version.major >= 9 then [%expr
        C2x]
      else [%expr
        raise (Unavailable C2x)]]
  | Gnu2x ->
      [%meta if Clangml_config.version.major >= 9 then [%expr
        Gnu2x]
      else [%expr
        raise (Unavailable Gnu2x)]]
  | Cxx98 -> Cxx98
  | Gnucxx98 -> Gnucxx98
  | Cxx11 -> Cxx11
  | Gnucxx11 -> Gnucxx11
  | Cxx14 ->
      [%meta if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Cxx14]
      else [%expr
        Cxx1y]]
  | Gnucxx14 ->
      [%meta if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Gnucxx14]
      else [%expr
        Gnucxx1y]]
  | Cxx17 ->
      [%meta if Clangml_config.version.major >= 5 then [%expr
        Cxx17]
      else if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Cxx1z]
      else [%expr
        raise (Unavailable Cxx17)]]
  | Gnucxx17 ->
      [%meta if Clangml_config.version.major >= 5 then [%expr
        Gnucxx17]
      else if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Gnucxx1z]
      else [%expr
        raise (Unavailable Gnucxx17)]]
  | Cxx20 ->
      [%meta if Clangml_config.version.major >= 10 then [%expr
        Cxx20]
      else if Clangml_config.version.major >= 5 then [%expr
        Cxx2a]
      else [%expr
        raise (Unavailable Cxx20)]]
  | Gnucxx20 ->
      [%meta if Clangml_config.version.major >= 10 then [%expr
        Gnucxx20]
      else if Clangml_config.version.major >= 5 then [%expr
        Gnucxx2a]
      else [%expr
        raise (Unavailable Gnucxx20)]]
  | Cxx23 ->
      [%meta if Clangml_config.version.major >= 17 then [%expr
        Cxx23]
      else if Clangml_config.version.major >= 12 then [%expr
        Cxx2b]
      else [%expr
        raise (Unavailable Cxx23)]]
  | Gnucxx23 ->
      [%meta if Clangml_config.version.major >= 17 then [%expr
        Gnucxx23]
      else if Clangml_config.version.major >= 12 then [%expr
        Gnucxx2b]
      else [%expr
        raise (Unavailable Gnucxx23)]]
  | Cxx26 ->
      [%meta if Clangml_config.version.major >= 17 then [%expr
        Cxx26]
      else [%expr
        raise (Unavailable Cxx26)]]
  | Gnucxx26 ->
      [%meta if Clangml_config.version.major >= 17 then [%expr
        Gnucxx26]
      else [%expr
        raise (Unavailable Gnucxx26)]]
  | Opencl10 ->
      [%meta if Clangml_config.version.major >= 5 then [%expr
        Opencl10]
      else [%expr
        raise (Unavailable Opencl10)]]
  | Opencl11 -> Opencl11
  | Opencl12 -> Opencl12
  | Opencl20 ->
      [%meta if
        Clangml_config.version >= { major = 3; minor = 6; subminor = 0 }
      then [%expr
        Opencl20]
      else [%expr
        raise (Unavailable Opencl20)]]
  | Opencl30 ->
      [%meta if Clangml_config.version.major >= 12 then [%expr
        Opencl30]
      else [%expr
        raise (Unavailable Opencl30)]]
  | Openclcpp10 ->
      [%meta if Clangml_config.version.major >= 14 then [%expr
        Openclcpp10]
      else if Clangml_config.version.major >= 7 then [%expr
        Openclcpp]
      else [%expr
        raise (Unavailable Openclcpp10)]]
  | Openclcpp2021 ->
      [%meta if Clangml_config.version.major >= 14 then [%expr
        Openclcpp2021]
      else [%expr
        raise (Unavailable Openclcpp2021)]]
  | Cuda -> 
      [%meta if Clangml_config.version.major >= 17 then [%expr
        raise (Unavailable Cuda)]
      else [%expr
        Cuda]]
  | Hip ->
      [%meta if Clangml_config.version.major >= 17 then [%expr
        raise (Unavailable Hip)]
      else if Clangml_config.version.major >= 7 then [%expr
        Hip]
      else [%expr
        raise (Unavailable Hip)]]
  | Hlsl ->
      [%meta if Clangml_config.version.major >= 15 then [%expr
        Hlsl]
      else [%expr
        raise (Unavailable Hlsl)]]
  | Hlsl2015 ->
      [%meta if Clangml_config.version.major >= 15 then [%expr
        Hlsl2015]
      else [%expr
        raise (Unavailable Hlsl2015)]]
  | Hlsl2016 ->
      [%meta if Clangml_config.version.major >= 15 then [%expr
        Hlsl2016]
      else [%expr
        raise (Unavailable Hlsl2016)]]
  | Hlsl2017 ->
      [%meta if Clangml_config.version.major >= 15 then [%expr
        Hlsl2017]
      else [%expr
        raise (Unavailable Hlsl2017)]]
  | Hlsl2018 ->
      [%meta if Clangml_config.version.major >= 15 then [%expr
        Hlsl2018]
      else [%expr
        raise (Unavailable Hlsl2018)]]
  | Hlsl2021 ->
      [%meta if Clangml_config.version.major >= 15 then [%expr
        Hlsl2021]
      else [%expr
        raise (Unavailable Hlsl2021)]]
  | Hlsl202x ->
      [%meta if Clangml_config.version.major >= 15 then [%expr
        Hlsl202x]
      else [%expr
        raise (Unavailable Hlsl202x)]]
