open Ppx_compare_lib.Builtin

let ignore_equal _ _ = true

let ignore_compare _ _ = 0

module type S = sig
  include Set.OrderedType

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module%import Clang = struct
  module%override Bindings = struct
    [%%recursive
      type clang_ext_langstandards
      and clang_ext_elaboratedtypekeyword
      and clang_ext_characterkind
      and clang_ext_unaryexpr
      and clang_ext_unaryoperatorkind
      and clang_ext_binaryoperatorkind
      and clang_ext_attrkind
      and clang_ext_overloadedoperatorkind
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
   ][@@deriving equal, compare]

    let equal_cxcursor = ignore_equal

    let compare_cxcursor = ignore_compare

    let equal_cxtype = ignore_equal

    let compare_cxtype = ignore_compare

    let equal_cxsourcelocation = ignore_equal

    let compare_cxsourcelocation = ignore_compare
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
      [@@deriving equal, compare]
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

    [%%recursive [%%types]]
      [@@deriving equal, compare]
  end
end

module Make (X : Set.OrderedType) : S with type t = X.t = struct
  include X

  module Set = Set.Make (X)

  module Map = Map.Make (X)
end

module Decl = Make (struct
  type t = Clang.Decl.t

  let compare = Ast.compare_decl

  let equal = Ast.equal_decl
end)

module Type = Make (struct
  type t = Clang.Type.t

  let compare = Ast.compare_qual_type

  let equal = Ast.equal_qual_type
end)

module Expr = Make (struct
  type t = Clang.Expr.t

  let compare = Ast.compare_expr

  let equal = Ast.equal_expr
end)

module Stmt = Make (struct
  type t = Clang.Stmt.t

  let compare = Ast.compare_stmt

  let equal = Ast.equal_stmt
end)

module Enum_constant = Make (struct
  type t = Clang.Enum_constant.t

  let compare = Ast.compare_enum_constant

  let equal = Ast.equal_enum_constant
end)

module Translation_unit = Make (struct
  type t = Clang.Translation_unit.t

  let compare = Ast.compare_translation_unit

  let equal = Ast.equal_translation_unit
end)
