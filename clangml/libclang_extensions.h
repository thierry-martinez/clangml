#include <stdbool.h>
#include <stdint.h>

#ifdef LLVM_VERSION_3_4_2
#define LLVM_VERSION_BEFORE_3_5_0
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_5_2
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_6_2
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_7_1
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_8_1
#define LLVM_VERSION_BEFORE_3_9_0
#endif

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
  #include "clangml_OperationKinds.def"
};

enum clang_ext_UnaryOperatorKind
clang_ext_UnaryOperator_getOpcode(CXCursor c);

CXString
clang_ext_UnaryOperator_getOpcodeSpelling(
  enum clang_ext_UnaryOperatorKind Kind);

enum clang_ext_BinaryOperatorKind {
  #define BINARY_OPERATION(Name, Spelling) CLANG_EXT_BINARY_OPERATOR_##Name,
  #include "clangml_OperationKinds.def"
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
  ECK_UnaryExprOrTypeTraitExpr, /* for Clang <3.9.0 */
  ECK_EmptyDecl,
  ECK_LinkageSpecDecl,
  ECK_Unknown
};

enum clang_ext_CursorKind
clang_ext_GetCursorKind(CXCursor c);

enum clang_ext_DeclKind {
  CLANG_EXT_DECL_Invalid,
  #define DECL(Derived, Base) CLANG_EXT_DECL_##Derived,
  #define ABSTRACT_DECL(Decl)
  #include <clang/AST/DeclNodes.inc>
  CLANG_EXT_DECL_Unknown
};

enum clang_ext_DeclKind
clang_ext_Decl_GetKind(CXCursor);

enum clang_ext_TypeKind {
  CLANG_EXT_TYPE_Invalid,
  #define TYPE(Class, Base) CLANG_EXT_TYPE_##Class,
  #define ABSTRACT_TYPE(Class, Base)
  #include <clang/AST/TypeNodes.def>
  CLANG_EXT_TYPE_Unknown
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

CXType
clang_ext_Type_getNamedType(CXType CT);

enum clang_ext_AttrKind {
  CLANG_EXT_ATTR_NoAttr = -1,
  #define ATTR(Name) CLANG_EXT_ATTR_##Name,
  #include <clang/Basic/AttrList.inc>
};

enum clang_ext_AttrKind
clang_ext_Type_GetAttributeKind(CXType CT);

CXString
clang_ext_AttrKind_GetSpelling(enum clang_ext_AttrKind AttrKind);

unsigned
clang_ext_CXXMethod_isDefaulted(CXCursor C);

unsigned
clang_ext_CXXMethod_isConst(CXCursor C);

unsigned
clang_ext_CXXConstructor_isExplicit(CXCursor C);

unsigned
clang_ext_FunctionDecl_isDeleted(CXCursor C);

unsigned
clang_ext_FunctionDecl_getNumParams(CXCursor C);

CXCursor
clang_ext_FunctionDecl_getParamDecl(CXCursor C, unsigned i);

/* Adapted from DeclCXX.h:LinkageSpecDecl:LanguageIDs */
enum clang_ext_LanguageIDs {
  CLANG_EXT_LANG_C = 0x0002,
  CLANG_EXT_LANG_CXX = 0x0004
};

unsigned
clang_ext_LinkageSpecDecl_getLanguageIDs(CXCursor C);

CXType
clang_ext_TemplateTypeParmDecl_getDefaultArgument(CXCursor C);

struct clang_ext_TemplateName {
  const void *data;
  CXTranslationUnit TU;
};

void
clang_ext_TemplateName_dispose(struct clang_ext_TemplateName);

enum clang_ext_TemplateName_NameKind {
  CLANG_EXT_Template,
  CLANG_EXT_OverloadedTemplate,
  CLANG_EXT_QualifiedTemplate,
  CLANG_EXT_DependentTemplate,
  CLANG_EXT_SubstTemplateTemplateParm,
  CLANG_EXT_SubstTemplateTemplateParmPack,
  CLANG_EXT_InvalidNameKind
};

enum clang_ext_TemplateName_NameKind
clang_ext_TemplateName_getKind(struct clang_ext_TemplateName);

CXCursor
clang_ext_TemplateName_getAsTemplateDecl(struct clang_ext_TemplateName);

struct clang_ext_TemplateArgument {
  const void *data;
  CXTranslationUnit TU;
};

void
clang_ext_TemplateArgument_dispose(struct clang_ext_TemplateArgument);

enum CXTemplateArgumentKind
clang_ext_TemplateArgument_getKind(struct clang_ext_TemplateArgument);

CXType
clang_ext_TemplateArgument_getAsType(struct clang_ext_TemplateArgument);

CXCursor
clang_ext_TemplateArgument_getAsDecl(struct clang_ext_TemplateArgument);

CXType
clang_ext_TemplateArgument_getNullPtrType(struct clang_ext_TemplateArgument);

struct clang_ext_TemplateName
clang_ext_TemplateArgument_getAsTemplateOrTemplatePattern(
  struct clang_ext_TemplateArgument);

CXInt
clang_ext_TemplateArgument_getAsIntegral(struct clang_ext_TemplateArgument);

CXType
clang_ext_TemplateArgument_getIntegralType(struct clang_ext_TemplateArgument);

CXType
clang_ext_TemplateArgument_getNonTypeTemplateArgumentType(
  struct clang_ext_TemplateArgument);

CXCursor
clang_ext_TemplateArgument_getAsExpr(struct clang_ext_TemplateArgument);

struct clang_ext_TemplateName
clang_ext_TemplateSpecializationType_getTemplateName(CXType CT);

unsigned
clang_ext_TemplateSpecializationType_getNumArgs(CXType);

struct clang_ext_TemplateArgument
clang_ext_TemplateSpecializationType_getArgument(CXType, unsigned);
