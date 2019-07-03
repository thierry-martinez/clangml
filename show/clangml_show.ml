type 'a pp = Format.formatter -> 'a -> unit

let opaque fmt _ =
  Format.pp_print_string fmt "<opaque>"

module%import Clang = struct
  module%override Bindings = struct
    [%%recursive
      type cxerrorcode
      and clang_ext_langstandards
      and clang_ext_elaboratedtypekeyword
      and clang_ext_characterkind
      and clang_ext_unaryexpr
      and clang_ext_unaryoperatorkind
      and clang_ext_binaryoperatorkind
      and clang_ext_attrkind
      and cxtypekind
      and cx_cxxaccessspecifier
      and cxcallingconv
      and cxlinkagekind
      and clang_ext_predefinedexpr_identkind
      and clang_ext_lambdacapturedefault
      and clang_ext_lambdacapturekind
      and clang_ext_typekind
      and clang_ext_stmtkind
      and cxcursorkind
      and clang_ext_declkind = _
   ][@@deriving show]

    let pp_cxcursor = opaque

    let pp_cxtype = opaque

    let pp_cxint fmt i =
      Format.pp_print_string fmt (Clang.string_of_cxint i)

    let pp_cxfloat fmt i =
      Format.pp_print_string fmt (Clang.string_of_cxfloat i)
  end

  module%override Types = struct
    module Clang__ = struct
      module Clang__bindings = struct
        include Bindings
      end
  
      module Clang__types = struct
        include Clang.Types
      end
    end

    [%%recursive [%%types]]
      [@@deriving show]
  end

  module%override Ast = struct
    module Clang__ = struct
      module Clang__bindings = struct
        include Bindings
      end
  
      module Clang__types = struct
        include Types
      end

      module Clang__ast = struct
        include Clang.Ast
      end
    end

    type concrete_location = _

    type source_location = _

    let pp_source_location = opaque

    [%%recursive [%%types]]
      [@@deriving show]
  end
end

include Ast

let pp_type = pp_qual_type
