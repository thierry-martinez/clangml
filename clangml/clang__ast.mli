[@@@ocaml.warning "-30"]

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

type 'a node = {
    cxcursor : cxcursor;
    desc : 'a;
  }

type stmt = stmt_desc node
and stmt_desc =
  | Null
  | Compound of {
      items : stmt list;
    }
  | For of {
      init : stmt option;
      condition_variable : var_decl option;
      cond : stmt option;
      inc : stmt option;
      body : stmt;
    }
  | If of {
      init : stmt option;
      condition_variable : var_decl option;
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
  | Switch of {
      init : stmt option;
      condition_variable : var_decl option;
      cond : expr;
      body : stmt;
    }
  | Case of {
      lhs : expr;
      rhs : expr option;
      body : stmt;
    }
  | Default of {
      body : stmt;
    }
  | While of {
      condition_variable : var_decl option;
      cond : expr;
      body : stmt;
    }
  | Do of {
      body : stmt;
      cond : expr;
    }
  | Label of {
      label : label_ref;
      body : stmt;
    }
  | Goto of {
      label : label_ref;
    }
  | IndirectGoto of {
      target : expr;
    }
  | Continue
  | Break
  | Asm
  | Decl of decl_stmt_desc
  | Return of {
      value : expr
    }
  | Expr of expr_desc

and expr = expr_desc node

and expr_desc =
  | IntegerLiteral of {
      s : string;
    }
  | UnaryOperator of {
      kind : clang_ext_unaryoperatorkind;
      operand : expr;
    }

  | BinaryOperator of {
      lhs : expr;
      kind : clang_ext_binaryoperatorkind;
      rhs : expr;
    }
  | DeclRef of {
      s : string;
    }
  | Call of {
      f : expr;
      args : expr list;
    }
  | UnexposedExpr of {
      s : string;
    }
  | OtherExpr

and decl_stmt = decl_stmt_desc node

and decl_stmt_desc =
  | Function of {
      result : qual_type;
      name : string;
      args : (string * qual_type) list;
      stmt : stmt;
    }
  | Var of var_decl_desc
  | Enum of {
      name : string;
      constants : enum_constant list;
    }
  | Struct of {
      name : string;
      fields : (string * qual_type) list;
    }
  | Typedef of {
      name : string;
      underlying_type : qual_type;
    }
  | OtherDecl

and label_ref = label_ref_desc node

and label_ref_desc = {
    name : string;
  }

and enum_constant = enum_constant_desc node

and enum_constant_desc = {
    name : string;
    init : expr option;
  }

and var_decl = var_decl_desc node

and var_decl_desc = {
    name : string;
    qual_type : qual_type;
    init : expr option
  }

type translation_unit_desc = {
    filename : string; items : decl_stmt list
  }

type translation_unit = translation_unit_desc node
