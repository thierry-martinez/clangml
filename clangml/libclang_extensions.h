#include <stdbool.h>

CXString clang_ext_IntegerLiteral_getValueAsString(
  CXCursor c, unsigned Radix, bool isSigned);

enum clang_ext_UnaryOperatorKind {
#define UNARY_OPERATION(Name, Spelling) CLANG_EXT_UNARY_OPERATOR_##Name,
#include <clang/AST/OperationKinds.def>
};

enum clang_ext_UnaryOperatorKind clang_ext_UnaryOperator_getOpcode(CXCursor c);

CXString clang_ext_UnaryOperator_getOpcodeSpelling(
  enum clang_ext_UnaryOperatorKind Kind);

enum clang_ext_BinaryOperatorKind {
#define BINARY_OPERATION(Name, Spelling) CLANG_EXT_BINARY_OPERATOR_##Name,
#include <clang/AST/OperationKinds.def>
};

enum clang_ext_BinaryOperatorKind clang_ext_BinaryOperator_getOpcode(
  CXCursor c);

CXString clang_ext_BinaryOperator_getOpcodeSpelling(
  enum clang_ext_BinaryOperatorKind Kind);

enum clang_ext_ForStmtChild {
  CLANG_EXT_FOR_STMT_INIT = 1,
  CLANG_EXT_FOR_STMT_CONDITION_VARIABLE = 2,
  CLANG_EXT_FOR_STMT_COND = 4,
  CLANG_EXT_FOR_STMT_INC = 8
};

unsigned clang_ext_ForStmt_getChildrenSet(CXCursor c);

enum clang_ext_IfStmtChild {
  CLANG_EXT_IF_STMT_INIT = 1,
  CLANG_EXT_IF_STMT_CONDITION_VARIABLE = 2,
  CLANG_EXT_IF_STMT_ELSE = 4
};

unsigned clang_ext_IfStmt_getChildrenSet(CXCursor c);

enum clang_ext_SwitchStmtChild {
  CLANG_EXT_SWITCH_STMT_INIT = 1,
  CLANG_EXT_SWITCH_STMT_CONDITION_VARIABLE = 2
};

unsigned clang_ext_SwitchStmt_getChildrenSet(CXCursor c);

enum clang_ext_WhileStmtChild {
  CLANG_EXT_WHILE_STMT_CONDITION_VARIABLE = 1
};

unsigned clang_ext_WhileStmt_getChildrenSet(CXCursor c);

/* From clang/AST/Type.h */
enum clang_ext_ElaboratedTypeKeyword {
  ETK_Struct,
  ETK_Interface,
  ETK_Union,
  ETK_Class,
  ETK_Enum,
  ETK_Typename,
  ETK_None
};

enum clang_ext_ElaboratedTypeKeyword clang_ext_ElaboratedType_getKeyword(
  CXType c);

CXString clang_ext_ElaboratedType_getKeywordSpelling(
  enum clang_ext_ElaboratedTypeKeyword keyword);

bool clang_ext_VarDecl_hasInit(CXCursor c);

bool clang_ext_MemberRefExpr_isArrow(CXCursor c);
