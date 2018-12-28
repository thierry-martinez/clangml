#include <stdbool.h>
#include <stdint.h>

typedef struct {
  void *data;
} CXInt;

bool
clang_equal_cxint(CXInt a, CXInt b);

int
clang_compare_cxint(CXInt a, CXInt b);

CXInt
clang_ext_IntegerLiteral_getValue(CXCursor c);

void
clang_ext_Int_dispose(CXInt c);

bool
clang_ext_Int_isValid(CXInt c);

CXString
clang_ext_Int_toString(CXInt c, unsigned Radix, bool isSigned);

double
clang_ext_Int_roundToDouble(CXInt c, bool isSigned);

float
clang_ext_Int_bitsToFloat(CXInt c);

unsigned
clang_ext_Int_getBitWidth(CXInt c);

unsigned
clang_ext_Int_getActiveBits(CXInt c);

unsigned
clang_ext_Int_getMinSignedBits(CXInt c);

bool
clang_ext_Int_getBoolValue(CXInt c);

int
clang_ext_Int_getSExtValue(CXInt c);

int64_t
clang_ext_Int_getSExtValue64(CXInt c);

typedef struct {
  void *data;
} CXFloat;

bool
clang_equal_cxfloat(CXFloat a, CXFloat b);

int
clang_compare_cxfloat(CXFloat a, CXFloat b);

CXFloat
clang_ext_FloatingLiteral_getValue(CXCursor c);

void
clang_ext_Float_dispose(CXFloat c);

bool
clang_ext_Float_isValid(CXFloat f);

CXString
clang_ext_Float_toString(CXFloat f);

double
clang_ext_Float_convertToDouble(CXFloat f);

CXString
clang_ext_StringLiteral_GetString(CXCursor c);

enum clang_ext_UnaryOperatorKind {
  #define UNARY_OPERATION(Name, Spelling) CLANG_EXT_UNARY_OPERATOR_##Name,
  #ifdef LLVM_3_8
    UNARY_OPERATION(UO_PostInc, "UO_PostInc")
    UNARY_OPERATION(UO_PostDec, "UO_PostDec")
    UNARY_OPERATION(UO_PreInc, "UO_PreInc")
    UNARY_OPERATION(UO_PreDec, "UO_PreDec")
    UNARY_OPERATION(UO_AddrOf, "UO_AddrOf")
    UNARY_OPERATION(UO_Deref, "UO_Deref")
    UNARY_OPERATION(UO_Plus, "UO_Plus")
    UNARY_OPERATION(UO_Minus, "UO_Minus")
    UNARY_OPERATION(UO_Not, "UO_Not")
    UNARY_OPERATION(UO_LNot, "UO_LNot")
    UNARY_OPERATION(UO_Real, "UO_Real")
    UNARY_OPERATION(UO_Imag, "UO_Imag")
    UNARY_OPERATION(UO_Extension, "UO_Extension")
    UNARY_OPERATION(UO_Coawait, "UO_Coawait")
  #else
    #include <clang/AST/OperationKinds.def>
  #endif
};

enum clang_ext_UnaryOperatorKind
clang_ext_UnaryOperator_getOpcode(CXCursor c);

CXString
clang_ext_UnaryOperator_getOpcodeSpelling(
  enum clang_ext_UnaryOperatorKind Kind);

enum clang_ext_BinaryOperatorKind {
  #define BINARY_OPERATION(Name, Spelling) CLANG_EXT_BINARY_OPERATOR_##Name,
  #ifdef LLVM_3_8
     BINARY_OPERATION(BO_PtrMemD, "BO_PtrMemD")
     BINARY_OPERATION(BO_PtrMemI, "BO_PtrMemI")    
     BINARY_OPERATION(BO_Mul, "BO_Mul")
     BINARY_OPERATION(BO_Div, "BO_Div")
     BINARY_OPERATION(BO_Rem, "BO_Rem")    
     BINARY_OPERATION(BO_Add, "BO_Add")
     BINARY_OPERATION(BO_Sub, "BO_Sub")            
     BINARY_OPERATION(BO_Shl, "BO_Shl")
     BINARY_OPERATION(BO_Shr, "BO_Shr")            
     BINARY_OPERATION(BO_LT, "BO_LT")
     BINARY_OPERATION(BO_GT, "BO_GT")
     BINARY_OPERATION(BO_LE, "BO_LE")
     BINARY_OPERATION(BO_GE, "BO_GE")
     BINARY_OPERATION(BO_EQ, "BO_EQ")
     BINARY_OPERATION(BO_NE, "BO_NE")              
     BINARY_OPERATION(BO_And, "BO_And")                    
     BINARY_OPERATION(BO_Xor, "BO_Xor")                    
     BINARY_OPERATION(BO_Or, "BO_Or")                     
     BINARY_OPERATION(BO_LAnd, "BO_LAnd")                   
     BINARY_OPERATION(BO_LOr, "BO_LOr")                    
     BINARY_OPERATION(BO_Assign, "BO_Assign")
     BINARY_OPERATION(BO_MulAssign, "BO_MulAssign")   
     BINARY_OPERATION(BO_DivAssign, "BO_DivAssign")
     BINARY_OPERATION(BO_RemAssign, "BO_RemAssign")
     BINARY_OPERATION(BO_AddAssign, "BO_AddAssign")
     BINARY_OPERATION(BO_SubAssign, "BO_SubAssign")
     BINARY_OPERATION(BO_ShlAssign, "BO_ShlAssign")
     BINARY_OPERATION(BO_ShrAssign, "BO_ShrAssign")
     BINARY_OPERATION(BO_AndAssign, "BO_AndAssign")
     BINARY_OPERATION(BO_XorAssign, "BO_XorAssign")
     BINARY_OPERATION(BO_OrAssign, "BO_OrAssign")
     BINARY_OPERATION(BO_Comma, "BO_Comma")
  #else
    #include <clang/AST/OperationKinds.def>
  #endif
};

enum clang_ext_BinaryOperatorKind
clang_ext_BinaryOperator_getOpcode(
  CXCursor c);

CXString
clang_ext_BinaryOperator_getOpcodeSpelling(
  enum clang_ext_BinaryOperatorKind Kind);

enum clang_ext_ForStmtChild {
  CLANG_EXT_FOR_STMT_INIT = 1,
  CLANG_EXT_FOR_STMT_CONDITION_VARIABLE = 2,
  CLANG_EXT_FOR_STMT_COND = 4,
  CLANG_EXT_FOR_STMT_INC = 8
};

unsigned
clang_ext_ForStmt_getChildrenSet(CXCursor c);

enum clang_ext_IfStmtChild {
  CLANG_EXT_IF_STMT_INIT = 1,
  CLANG_EXT_IF_STMT_CONDITION_VARIABLE = 2,
  CLANG_EXT_IF_STMT_ELSE = 4
};

unsigned
clang_ext_IfStmt_getChildrenSet(CXCursor c);

CXCursor
clang_ext_IfStmt_getInit(CXCursor c);

enum clang_ext_SwitchStmtChild {
  CLANG_EXT_SWITCH_STMT_INIT = 1,
  CLANG_EXT_SWITCH_STMT_CONDITION_VARIABLE = 2
};

unsigned
clang_ext_SwitchStmt_getChildrenSet(CXCursor c);

CXCursor
clang_ext_SwitchStmt_getInit(CXCursor c);

enum clang_ext_WhileStmtChild {
  CLANG_EXT_WHILE_STMT_CONDITION_VARIABLE = 1
};

unsigned
clang_ext_WhileStmt_getChildrenSet(CXCursor c);

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

enum clang_ext_ElaboratedTypeKeyword
clang_ext_ElaboratedType_getKeyword(CXType c);

CXString
clang_ext_ElaboratedType_getKeywordSpelling(
  enum clang_ext_ElaboratedTypeKeyword keyword);

bool
clang_ext_VarDecl_hasInit(CXCursor c);

bool
clang_ext_MemberRefExpr_isArrow(CXCursor c);

CXString
clang_ext_Stmt_GetClassName(CXCursor c);

int
clang_ext_Stmt_GetClassKind(CXCursor c);

enum clang_ext_CursorKind {
  ECK_ImplicitCastExpr,
  ECK_BinaryConditionalOperator,
  ECK_Unknown
};

enum clang_ext_CursorKind
clang_ext_GetCursorKind(CXCursor c);

enum clang_ext_TypeKind {
  ETK_Invalid,
  ETK_Paren,
  ETK_Unknown
};

enum clang_ext_TypeKind
clang_ext_GetTypeKind(CXType c);

CXType
clang_ext_GetInnerType(CXType c);

CXCursor
clang_ext_VariableArrayType_GetSizeExpr(CXType c);

CXString
clang_ext_AsmStmt_GetAsmString(CXCursor c);

enum clang_ext_CharacterKind {
  ECK_Ascii,
  ECK_Wide,
  ECK_UTF8,
  ECK_UTF16,
  ECK_UTF32
};

enum clang_ext_CharacterKind
clang_ext_CharacterLiteral_GetCharacterKind(CXCursor c);

unsigned
clang_ext_CharacterLiteral_GetValue(CXCursor c);

/* From clang/Basic/TypeTraits.h */
enum clang_ext_UnaryExpr {
  UETT_SizeOf,
  UETT_AlignOf,
  UETT_VecStep,
  UETT_OpenMPRequiredSimdAlign,
};

enum clang_ext_UnaryExpr
clang_ext_UnaryExpr_GetKind(CXCursor c);

CXType
clang_ext_UnaryExpr_GetArgumentType(CXCursor c);
