open Clang__bindings

type qual_type = {
    cxtype : cxtype;
    const : bool;
    desc : type_desc;
  }

and type_desc =
  | IncompleteArray of {
      element : qual_type;
    }
  | Pointer of {
      pointee : qual_type;
    }
  | Elaborated of {
      keyword : clang_ext_elaboratedtypekeyword;
      named_type : qual_type;
    }
  | Enum of {
      name : string;
    }
  | Struct of {
      name : string;
    }
  | Typedef of {
      name : string;
    }
  | OtherType of {
      kind : cxtypekind;
    }

type stmt =
  | Null
  | Compound of {
      cxcursor : cxcursor;
      items : stmt list;
    }
  | For of {
      cxcursor : cxcursor;
      init : stmt option;
      condition_variable : decl_stmt option;
      cond : stmt option;
      inc : stmt option;
      body : stmt;
    }
  | Decl of decl_stmt
  | Return of {
      cxcursor : cxcursor;
      value : expr
    }
  | Expr of expr

and expr =
  | IntegerLiteral of {
      cxcursor : cxcursor;
      s : string;
    }
  | UnaryOperator of {
      cxcursor : cxcursor;
      kind : clang_ext_unaryoperatorkind;
      operand : expr;
    }
  | BinaryOperator of {
      cxcursor : cxcursor;
      lhs : expr;
      kind : clang_ext_binaryoperatorkind;
      rhs : expr;
    }
  | DeclRef of {
      cxcursor : cxcursor;
      s : string;
    }
  | Call of {
      cxcursor : cxcursor;
      f : expr;
      args : expr list;
    }
  | UnexposedExpr of {
      cxcursor : cxcursor;
      s : string;
    }
  | OtherExpr of cxcursor

and decl_stmt =
  | Function of {
      cxcursor : cxcursor;
      result : qual_type;
      name : string;
      args : (string * qual_type) list;
      stmt : stmt;
    }
  | Var of {
      cxcursor : cxcursor;
      name : string;
      qual_type : qual_type;
      init : expr option
    }
  | Enum of {
      cxcursor : cxcursor;
      name : string;
      constants : enum_constant list;
    }
  | Struct of {
      cxcursor : cxcursor;
      name : string;
      fields : (string * qual_type) list;
    }
  | Typedef of {
      cxcursor : cxcursor;
      name : string;
      underlying_type : qual_type;
    }
  | OtherDecl of cxcursor

and enum_constant = {
    cxcursor : cxcursor;
    name : string;
    init : expr option;
    value : int;
  }

type translation_unit = {
    cxcursor : cxcursor; filename : string; items : decl_stmt list
  }
