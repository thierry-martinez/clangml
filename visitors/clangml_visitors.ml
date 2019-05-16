[%%rewrite
  module%import Clang__bindings = struct
    type cxcursor =
      Clang__bindings.cxcursor [@opaque] [@@rewrite] [@@remove]
    type cxcursorkind =
      Clang__bindings.cxcursorkind [@opaque] [@@rewrite] [@@remove]
    type clang_ext_declkind =
      Clang__bindings.clang_ext_declkind [@opaque] [@@rewrite] [@@remove]
    type clang_ext_typekind =
      Clang__bindings.clang_ext_typekind [@opaque] [@@rewrite] [@@remove]
    type cxfloat =
      Clang__bindings.cxfloat [@opaque] [@@rewrite] [@@remove]
    type cxint =
      Clang__bindings.cxint [@opaque] [@@rewrite] [@@remove]
    type cxlinkagekind =
      Clang__bindings.cxlinkagekind [@opaque] [@@rewrite] [@@remove]
    type cxcallingconv =
      Clang__bindings.cxcallingconv [@opaque] [@@rewrite] [@@remove]
    type cx_cxxaccessspecifier =
      Clang__bindings.cx_cxxaccessspecifier
          [@opaque] [@@rewrite] [@@remove]
    type cxtypekind =
      Clang__bindings.cxtypekind [@opaque] [@@rewrite] [@@remove]
    type clang_ext_attrkind =
      Clang__bindings.clang_ext_attrkind [@opaque] [@@rewrite] [@@remove]
    type clang_ext_binaryoperatorkind =
      Clang__bindings.clang_ext_binaryoperatorkind
          [@opaque] [@@rewrite] [@@remove]
    type clang_ext_unaryoperatorkind =
      Clang__bindings.clang_ext_unaryoperatorkind
          [@opaque] [@@rewrite] [@@remove]
    type clang_ext_unaryexpr =
      Clang__bindings.clang_ext_unaryexpr [@opaque] [@@rewrite] [@@remove]
    type clang_ext_characterkind =
      Clang__bindings.clang_ext_characterkind [@opaque]
          [@@rewrite] [@@remove]
    type clang_ext_elaboratedtypekeyword =
      Clang__bindings.clang_ext_elaboratedtypekeyword
          [@opaque] [@@rewrite] [@@remove]
    type cxsourcelocation =
      Clang__bindings.cxsourcelocation [@opaque] [@@rewrite] [@@remove]
  end

  module%import Clang__ast = struct
    [%%recursive
      type concrete_location = _
      type source_location = _
      type 'qual_type open_decoration = _ and co]
      [@@deriving
        visitors { variety = "iter"; name = "base_iter"; polymorphic = true }]

    [%%recursive
      type translation_unit = _ and co]
      [@@deriving
        visitors { variety = "iter"; ancestors = ["base_iter"] }]

  end]
