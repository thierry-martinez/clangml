#include <stdbool.h>
#include <stdint.h>

#ifdef LLVM_VERSION_3_4_2
#define LLVM_VERSION_BEFORE_3_5_0
#define LLVM_VERSION_BEFORE_3_6_0
#endif
#ifdef LLVM_VERSION_3_5_2
#define LLVM_VERSION_BEFORE_3_6_0
#endif
#ifdef LLVM_VERSION_BEFORE_3_6_0
#define LLVM_VERSION_BEFORE_3_7_0
#endif
#ifdef LLVM_VERSION_3_6_2
#define LLVM_VERSION_BEFORE_3_7_0
#endif
#ifdef LLVM_VERSION_BEFORE_3_7_0
#define LLVM_VERSION_BEFORE_3_8_0
#endif
#ifdef LLVM_VERSION_3_7_1
#define LLVM_VERSION_BEFORE_3_8_0
#endif
#ifdef LLVM_VERSION_BEFORE_3_8_0
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_8_1
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_BEFORE_3_9_0
#define LLVM_VERSION_BEFORE_4_0_0
#endif
#ifdef LLVM_VERSION_3_9_1
#define LLVM_VERSION_BEFORE_4_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_4_0_0
#define LLVM_VERSION_BEFORE_5_0_0
#endif
#ifdef LLVM_VERSION_4_0_1
#define LLVM_VERSION_BEFORE_5_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_5_0_0
#define LLVM_VERSION_BEFORE_6_0_0
#endif
#ifdef LLVM_VERSION_5_0_2
#define LLVM_VERSION_BEFORE_6_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_6_0_0
#define LLVM_VERSION_BEFORE_7_0_0
#endif
#ifdef LLVM_VERSION_6_0_1
#define LLVM_VERSION_BEFORE_7_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_7_0_0
#define LLVM_VERSION_BEFORE_8_0_0
#endif
#ifdef LLVM_VERSION_7_1_0
#define LLVM_VERSION_BEFORE_8_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_8_0_0
#define LLVM_VERSION_BEFORE_9_0_0
#endif
#ifdef LLVM_VERSION_8_0_1
#define LLVM_VERSION_BEFORE_9_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_9_0_0
#define LLVM_VERSION_BEFORE_10_0_0
#endif
#ifdef LLVM_VERSION_9_0_1
#define LLVM_VERSION_BEFORE_10_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_10_0_0
#define LLVM_VERSION_BEFORE_11_0_0
#endif
#ifdef LLVM_VERSION_10_0_1
#define LLVM_VERSION_BEFORE_11_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_11_0_0
#define LLVM_VERSION_BEFORE_11_1_0
#endif
#ifdef LLVM_VERSION_11_0_0
#define LLVM_VERSION_BEFORE_11_1_0
#endif
#ifdef LLVM_VERSION_BEFORE_11_1_0
#define LLVM_VERSION_BEFORE_12_0_0
#endif
#ifdef LLVM_VERSION_11_1_0
#define LLVM_VERSION_BEFORE_12_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_12_0_0
#define LLVM_VERSION_BEFORE_13_0_0
#endif
#ifdef LLVM_VERSION_12_0_1
#define LLVM_VERSION_BEFORE_13_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_13_0_0
#define LLVM_VERSION_BEFORE_14_0_0
#endif
#ifdef LLVM_VERSION_13_0_1
#define LLVM_VERSION_BEFORE_14_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_14_0_0
#define LLVM_VERSION_BEFORE_15_0_0
#endif
#ifdef LLVM_VERSION_14_0_0
#define LLVM_VERSION_BEFORE_15_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_15_0_0
#define LLVM_VERSION_BEFORE_16_0_0
#endif
#ifdef LLVM_VERSION_15_0_0
#define LLVM_VERSION_BEFORE_16_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_16_0_0
#define LLVM_VERSION_BEFORE_17_0_0
#endif
#ifdef LLVM_VERSION_16_0_0
#define LLVM_VERSION_BEFORE_17_0_0
#endif

CXVersion
clang_ext_getVersion();

typedef struct {
  void *data;
} CXInt;

bool
clang_equal_cxint(CXInt, CXInt);

int
clang_compare_cxint(CXInt, CXInt);

CXInt
clang_ext_IntegerLiteral_getValue(CXCursor);

void
clang_ext_Int_dispose(CXInt);

bool
clang_ext_Int_isValid(CXInt);

CXString
clang_ext_Int_toString(CXInt, unsigned Radix, bool isSigned);

double
clang_ext_Int_roundToDouble(CXInt, bool isSigned);

float
clang_ext_Int_bitsToFloat(CXInt);

unsigned
clang_ext_Int_getBitWidth(CXInt);

unsigned
clang_ext_Int_getActiveBits(CXInt);

unsigned
clang_ext_Int_getMinSignedBits(CXInt);

unsigned
clang_ext_Int_getSignificantBits(CXInt);

bool
clang_ext_Int_getBoolValue(CXInt);

int
clang_ext_Int_getZExtValue(CXInt);

int
clang_ext_Int_getSExtValue(CXInt);

uint64_t
clang_ext_Int_getZExtValue64(CXInt);

int64_t
clang_ext_Int_getSExtValue64(CXInt);

typedef struct {
  void *data;
} CXFloat;

bool
clang_equal_cxfloat(CXFloat, CXFloat);

int
clang_compare_cxfloat(CXFloat, CXFloat);

CXFloat
clang_ext_FloatingLiteral_getValue(CXCursor);

void
clang_ext_Float_dispose(CXFloat);

bool
clang_ext_Float_isValid(CXFloat);

CXString
clang_ext_Float_toString(CXFloat);

enum clang_ext_fltSemantics {
  CLANG_EXT_fltSemantics_IEEEhalf,
  CLANG_EXT_fltSemantics_IEEEsingle,
  CLANG_EXT_fltSemantics_IEEEdouble,
  CLANG_EXT_fltSemantics_IEEEquad,
  CLANG_EXT_fltSemantics_PPCDoubleDouble,
  CLANG_EXT_fltSemantics_x87DoubleExtended,
  CLANG_EXT_fltSemantics_Bogus,
  CLANG_EXT_fltSemantics_Invalid
};

enum clang_ext_fltSemantics
clang_ext_Float_getSemantics(CXFloat);

float
clang_ext_Float_convertToFloat(CXFloat);

double
clang_ext_Float_convertToDouble(CXFloat);

CXString
clang_ext_StringLiteral_GetString(CXCursor);

CXString
clang_ext_StringLiteral_getBytes(CXCursor);

unsigned int
clang_ext_StringLiteral_getByteLength(CXCursor);

unsigned int
clang_ext_StringLiteral_getCharByteWidth(CXCursor);

/* Copied from Expr.h:StringLiteral:StringKind */
enum clang_ext_StringKind {
  clang_ext_StringKind_Ordinary,
  clang_ext_StringKind_Wide,
  clang_ext_StringKind_UTF8,
  clang_ext_StringKind_UTF16,
  clang_ext_StringKind_UTF32,
  clang_ext_StringKind_InvalidStringKind
};

enum clang_ext_StringKind
clang_ext_StringLiteral_getKind(CXCursor);

#ifdef LLVM_VERSION_BEFORE_3_9_0
#define CLANG_EXT_UNARY_OPERATOR_InvalidUnaryOperator \
  CLANG_EXT_UNARY_OPERATOR_UO_Invalid
#endif

enum clang_ext_UnaryOperatorKind {
  #define UNARY_OPERATION(Name, Spelling) CLANG_EXT_UNARY_OPERATOR_##Name,
  #include "clangml_OperationKinds.def"

  CLANG_EXT_UNARY_OPERATOR_InvalidUnaryOperator
};

enum clang_ext_UnaryOperatorKind
clang_ext_UnaryOperator_getOpcode(CXCursor);

CXString
clang_ext_UnaryOperator_getOpcodeSpelling(
  enum clang_ext_UnaryOperatorKind Kind);

#ifdef LLVM_VERSION_BEFORE_3_9_0
#define CLANG_EXT_BINARY_OPERATOR_InvalidBinaryOperator \
  CLANG_EXT_BINARY_OPERATOR_BO_Invalid
#endif

enum clang_ext_BinaryOperatorKind {
  #define BINARY_OPERATION(Name, Spelling) CLANG_EXT_BINARY_OPERATOR_##Name,
  #include "clangml_OperationKinds.def"
  CLANG_EXT_BINARY_OPERATOR_InvalidBinaryOperator
};

enum clang_ext_BinaryOperatorKind
clang_ext_BinaryOperator_getOpcode(
  CXCursor);

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
clang_ext_ForStmt_getChildrenSet(CXCursor);

enum clang_ext_IfStmtChild {
  CLANG_EXT_IF_STMT_INIT = 1,
  CLANG_EXT_IF_STMT_CONDITION_VARIABLE = 2,
  CLANG_EXT_IF_STMT_ELSE = 4
};

unsigned
clang_ext_IfStmt_getChildrenSet(CXCursor);

CXCursor
clang_ext_IfStmt_getInit(CXCursor);

CXCursor
clang_ext_IfStmt_getConditionVariable(CXCursor);

CXCursor
clang_ext_IfStmt_getCond(CXCursor);

CXCursor
clang_ext_IfStmt_getThen(CXCursor);

CXCursor
clang_ext_IfStmt_getElse(CXCursor);

enum clang_ext_SwitchStmtChild {
  CLANG_EXT_SWITCH_STMT_INIT = 1,
  CLANG_EXT_SWITCH_STMT_CONDITION_VARIABLE = 2
};

unsigned
clang_ext_SwitchStmt_getChildrenSet(CXCursor);

CXCursor
clang_ext_SwitchStmt_getInit(CXCursor);

enum clang_ext_WhileStmtChild {
  CLANG_EXT_WHILE_STMT_CONDITION_VARIABLE = 1
};

unsigned
clang_ext_WhileStmt_getChildrenSet(CXCursor);

/* From clang/AST/Type.h */
enum clang_ext_ElaboratedTypeKeyword {
  ETK_Struct,
  ETK_Interface,
  ETK_Union,
  ETK_Class,
  ETK_Enum,
  ETK_Typename,
  ETK_NoKeyword
};

enum clang_ext_ElaboratedTypeKeyword
clang_ext_ElaboratedType_getKeyword(CXType c);

CXString
clang_ext_ElaboratedType_getKeywordSpelling(
  enum clang_ext_ElaboratedTypeKeyword keyword);

bool
clang_ext_VarDecl_hasInit(CXCursor);

bool
clang_ext_VarDecl_isConstexpr(CXCursor);

bool
clang_ext_MemberRefExpr_isArrow(CXCursor);

CXString
clang_ext_Stmt_GetClassName(CXCursor);

int
clang_ext_Stmt_GetClassKind(CXCursor);

enum clang_ext_CursorKind {
  ECK_ImplicitCastExpr,
  ECK_BinaryConditionalOperator,
  ECK_UnaryExprOrTypeTraitExpr, /* for Clang <3.9.0 */
  ECK_EmptyDecl,
  ECK_LinkageSpecDecl,
  ECK_Unknown
};

enum clang_ext_CursorKind
clang_ext_GetCursorKind(CXCursor);

enum clang_ext_DeclKind {
  CLANG_EXT_DECL_InvalidDecl,
  #define DECL(Class, _Base) CLANG_EXT_DECL_##Class,
  #define ABSTRACT_DECL(_Decl)
  #include <clang/AST/DeclNodes.inc>
  CLANG_EXT_DECL_UnknownDecl
};

enum clang_ext_DeclKind
clang_ext_Decl_GetKind(CXCursor);

unsigned
clang_ext_Decl_visitAttributes(
  CXCursor parent, CXCursorVisitor visitor, CXClientData client_data);

bool
clang_ext_Decl_isImplicit(CXCursor);

bool
clang_ext_RecordDecl_isInjectedClassName(CXCursor);

unsigned
clang_ext_CXXRecordDecl_visitBases(
  CXCursor parent, CXCursorVisitor visitor, CXClientData client_data);

enum clang_ext_StmtKind {
  CLANG_EXT_STMT_InvalidStmt,
  #define STMT(Class, _Base) CLANG_EXT_STMT_##Class,
  #define ABSTRACT_STMT(_Stmt)
  #include <clang/AST/StmtNodes.inc>
  CLANG_EXT_STMT_UnknownStmt
};

enum clang_ext_StmtKind
clang_ext_Stmt_GetKind(CXCursor);

#ifdef LLVM_VERSION_BEFORE_10_0_0
  #define TYPENODES_INC <clang/AST/TypeNodes.def>
#else
  #define TYPENODES_INC <clang/AST/TypeNodes.inc>
#endif

enum clang_ext_TypeKind {
  CLANG_EXT_TYPE_InvalidType,
  #define TYPE(Class, _Base) CLANG_EXT_TYPE_##Class,
  #define ABSTRACT_TYPE(_Class, _Base)
  #include TYPENODES_INC
  CLANG_EXT_TYPE_UnknownType
};

enum clang_ext_TypeKind
clang_ext_Type_GetKind(CXType c);

enum clang_ext_TypeKind
clang_ext_GetTypeKind(CXType c);

CXType
clang_ext_GetInnerType(CXType c);

CXCursor
clang_ext_DeclaratorDecl_GetSizeExpr(CXCursor);

CXCursor
clang_ext_VariableArrayType_GetSizeExpr(CXType c);

/* Copied from Expr.h:CharacterLiteral:CharacterKind */
enum clang_ext_CharacterKind {
  clang_ext_CharacterKind_Ascii,
  clang_ext_CharacterKind_Wide,
  clang_ext_CharacterKind_UTF8,
  clang_ext_CharacterKind_UTF16,
  clang_ext_CharacterKind_UTF32,
  clang_ext_CharacterKind_InvalidCharacterKind
};

enum clang_ext_CharacterKind
clang_ext_CharacterLiteral_GetCharacterKind(CXCursor);

unsigned
clang_ext_CharacterLiteral_GetValue(CXCursor);

/* From clang/Basic/TypeTraits.h */
enum clang_ext_UnaryExpr {
  UETT_SizeOf,
  UETT_AlignOf,
  UETT_VecStep,
  UETT_OpenMPRequiredSimdAlign,
  UETT_PreferredAlignOf,
};

enum clang_ext_UnaryExpr
clang_ext_UnaryExpr_GetKind(CXCursor);

bool
clang_ext_UnaryExpr_isArgumentType(CXCursor);

struct clang_ext_TypeLoc {
  const void *data;
  CXTranslationUnit tu;
};

struct clang_ext_TypeLoc
clang_ext_UnaryExpr_getArgumentTypeLoc(CXCursor);

CXType
clang_ext_Type_getNamedType(CXType CT);

enum clang_ext_AttrKind {
  CLANG_EXT_ATTR_NoAttr = -1,
  #define ATTR(Name) CLANG_EXT_ATTR_##Name,
  #include <clang/Basic/AttrList.inc>
};

CXString
clang_ext_AttrKind_GetSpelling(enum clang_ext_AttrKind AttrKind);

unsigned
clang_ext_CXXMethod_isDefaulted(CXCursor);

unsigned
clang_ext_CXXMethod_isConst(CXCursor);

unsigned
clang_ext_CXXConstructor_isExplicit(CXCursor);

unsigned
clang_ext_FunctionDecl_isDeleted(CXCursor);

unsigned
clang_ext_FunctionDecl_getNumParams(CXCursor);

CXCursor
clang_ext_FunctionDecl_getParamDecl(CXCursor, unsigned int);

bool
clang_ext_FunctionDecl_isConstexpr(CXCursor);

bool
clang_ext_FunctionDecl_hasWrittenPrototype(CXCursor);

bool
clang_ext_FunctionDecl_doesThisDeclarationHaveABody(CXCursor);

CXCursor
clang_ext_FunctionDecl_getBody(CXCursor);

/* Adapted from DeclCXX.h:LinkageSpecDecl:LanguageIDs */
#ifdef LLVM_VERSION_BEFORE_10_0_0
enum clang_ext_LanguageIDs {
  CLANG_EXT_LANG_C = 0x0002,
  CLANG_EXT_LANG_CXX = 0x0004
};
#else
enum clang_ext_LanguageIDs {
  CLANG_EXT_LANG_C = 0x0001,
  CLANG_EXT_LANG_CXX = 0x0002
};
#endif

unsigned
clang_ext_LinkageSpecDecl_getLanguageIDs(CXCursor);

CXType
clang_ext_TemplateTypeParmDecl_getDefaultArgument(CXCursor);

CXCursor
clang_ext_NonTypeTemplateParmDecl_getDefaultArgument(CXCursor);

struct clang_ext_TemplateName {
  const void *data;
  CXTranslationUnit TU;
};

void
clang_ext_TemplateName_dispose(struct clang_ext_TemplateName);

enum clang_ext_TemplateName_NameKind {
  CLANG_EXT_Template,
  CLANG_EXT_OverloadedTemplate,
  CLANG_EXT_AssumedTemplate,
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

#ifdef LLVM_VERSION_BEFORE_3_6_0
enum CXTemplateArgumentKind {
  CXTemplateArgumentKind_Null,
  CXTemplateArgumentKind_Type,
  CXTemplateArgumentKind_Declaration,
  CXTemplateArgumentKind_NullPtr,
  CXTemplateArgumentKind_Integral,
  CXTemplateArgumentKind_Template,
  CXTemplateArgumentKind_TemplateExpansion,
  CXTemplateArgumentKind_Expression,
  CXTemplateArgumentKind_Pack,
  CXTemplateArgumentKind_Invalid
};
#endif

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

CXCursor
clang_ext_TemplateArgument_getAsExpr(struct clang_ext_TemplateArgument);

unsigned int
clang_ext_TemplateArgument_getPackSize(struct clang_ext_TemplateArgument);

struct clang_ext_TemplateArgument
clang_ext_TemplateArgument_getPackArgument(
  struct clang_ext_TemplateArgument, unsigned int);

struct clang_ext_TemplateArgument
clang_ext_TemplateArgument_getPackExpansionPattern(
  struct clang_ext_TemplateArgument);

struct clang_ext_TemplateName
clang_ext_TemplateSpecializationType_getTemplateName(CXType);

unsigned
clang_ext_TemplateSpecializationType_getNumArgs(CXType);

struct clang_ext_TemplateArgument
clang_ext_TemplateSpecializationType_getArgument(CXType, unsigned int);

CXCursor
clang_ext_FriendDecl_getFriendDecl(CXCursor);

CXType
clang_ext_FriendDecl_getFriendType(CXCursor);

CXCursor
clang_ext_FieldDecl_getInClassInitializer(CXCursor);

CXType
clang_ext_GenericSelectionExpr_getAssocType(CXCursor, unsigned int);

bool
clang_ext_TemplateParm_isParameterPack(CXCursor);

CXCursor
clang_ext_TemplateDecl_getTemplatedDecl(CXCursor);

/* Copied from AST/Expr.h */
enum clang_ext_PredefinedExpr_IdentKind {
  clang_ext_PredefinedExpr_Func,
  clang_ext_PredefinedExpr_Function,
  clang_ext_PredefinedExpr_LFunction,
  clang_ext_PredefinedExpr_FuncDName,
  clang_ext_PredefinedExpr_FuncSig,
  clang_ext_PredefinedExpr_LFuncSig,
  clang_ext_PredefinedExpr_PrettyFunction,
  clang_ext_PredefinedExpr_PrettyFunctionNoVirtual,
  clang_ext_PredefinedExpr_InvalidPredefinedExpr
};

enum clang_ext_PredefinedExpr_IdentKind
clang_ext_PredefinedExpr_getIdentKind(CXCursor);

#ifndef LLVM_VERSION_BEFORE_3_6_0
CXString
clang_ext_PredefinedExpr_getFunctionName(CXCursor);
#endif

CXString
clang_ext_PredefinedExpr_ComputeName(
  enum clang_ext_PredefinedExpr_IdentKind kind,
  CXCursor decl);

bool
clang_ext_LambdaExpr_isMutable(CXCursor);

bool
clang_ext_LambdaExpr_hasExplicitParameters(CXCursor);

bool
clang_ext_LambdaExpr_hasExplicitResultType(CXCursor);

/* Copied from Basic/Lambda.h.
   clang_ext_LCD_None is renamed clang_ext_LCD_CaptureNone to avoid name clash */
enum clang_ext_LambdaCaptureDefault {
  clang_ext_LCD_CaptureNone,
  clang_ext_LCD_ByCopy,
  clang_ext_LCD_ByRef
};

enum clang_ext_LambdaCaptureDefault
clang_ext_LambdaExpr_getCaptureDefault(CXCursor);

struct clang_ext_LambdaCapture {
  const void *data;
  CXTranslationUnit TU;
};

void
clang_ext_LambdaExpr_getCaptures(
  CXCursor, void (*)(struct clang_ext_LambdaCapture, void *), void *);

CXCursor
clang_ext_LambdaExpr_getCallOperator(CXCursor);

/* Copied from Basic/Lambda.h */
enum clang_ext_LambdaCaptureKind {
  clang_ext_LCK_This,
  clang_ext_LCK_StarThis,
  clang_ext_LCK_ByCopy,
  clang_ext_LCK_ByRef,
  clang_ext_LCK_VLAType
};

enum clang_ext_LambdaCaptureKind
clang_ext_LambdaCapture_getKind(struct clang_ext_LambdaCapture capture);

CXCursor
clang_ext_LambdaCapture_getCapturedVar(struct clang_ext_LambdaCapture capture);

bool
clang_ext_LambdaCapture_isImplicit(struct clang_ext_LambdaCapture capture);

bool
clang_ext_LambdaCapture_isPackExpansion(struct clang_ext_LambdaCapture capture);

void
clang_ext_LambdaCapture_dispose(struct clang_ext_LambdaCapture capture);

struct clang_ext_TypeLoc
clang_ext_CXXNewExpr_getAllocatedTypeLoc(CXCursor);

CXCursor
clang_ext_CXXNewExpr_getArraySize(CXCursor);

unsigned int
clang_ext_CXXNewExpr_getNumPlacementArgs(CXCursor);

CXCursor
clang_ext_CXXNewExpr_getPlacementArg(CXCursor, unsigned int);

CXCursor
clang_ext_CXXNewExpr_getInitializer(CXCursor);

bool
clang_ext_CXXDeleteExpr_isGlobalDelete(CXCursor);

bool
clang_ext_CXXDeleteExpr_isArrayForm(CXCursor);

bool
clang_ext_CXXTypeidExpr_isTypeOperand(CXCursor);

struct clang_ext_TypeLoc
clang_ext_CXXTypeidExpr_getTypeOperand(CXCursor);

CXCursor
clang_ext_CXXTypeidExpr_getExprOperand(CXCursor);

#ifdef LLVM_VERSION_BEFORE_10_0_0
  #define LANGSTANDARDS_DEF <clang/Frontend/LangStandards.def>
#else
  #define LANGSTANDARDS_DEF <clang/Basic/LangStandards.def>
#endif

enum clang_ext_langstandards {
  #define FOREACH_STANDARD(Ident, Name) \
    CLANG_EXT_LANGSTANDARDS_##Ident,
  #ifdef LLVM_VERSION_BEFORE_5_0_0
  #define LANGSTANDARD(Ident, Name, _Desc, _Features) \
    FOREACH_STANDARD(Ident, Name)
  #else
  #define LANGSTANDARD(Ident, Name, _Lang, _Desc, _Features) \
    FOREACH_STANDARD(Ident, Name)
  #endif
  #include LANGSTANDARDS_DEF
  #undef FOREACH_STANDARD
  CLANG_EXT_LANGSTANDARDS_InvalidLang
};

const char *
clang_ext_LangStandard_getName(enum clang_ext_langstandards s);

enum clang_ext_langstandards
clang_ext_LangStandard_ofName(const char *s);

CXType
clang_ext_PackExpansion_getPattern(CXType c);

bool
clang_ext_CXXFoldExpr_isRightFold(CXCursor);

enum clang_ext_BinaryOperatorKind
clang_ext_CXXFoldExpr_getOperator(CXCursor);

bool
clang_ext_CXXBoolLiteralExpr_getValue(CXCursor);

CXCursor
clang_ext_CallExpr_getCallee(CXCursor);

unsigned int
clang_ext_CallExpr_getNumArgs(CXCursor);

CXCursor
clang_ext_CallExpr_getArg(CXCursor, unsigned int i);

CXCursor
clang_ext_SizeOfPackExpr_getPack(CXCursor);

CXCursor
clang_ext_DecltypeType_getUnderlyingExpr(CXType t);

bool
clang_ext_NamespaceDecl_isInline(CXCursor);

typedef enum CXVisitorResult (*CXDeclContextVisitor)(
  CXCursor, CXClientData client_data);

enum clang_ext_OverloadedOperatorKind {
  CLANG_EXT_OVERLOADED_OPERATOR_InvalidOverloadedOperator,
  #define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
    CLANG_EXT_OVERLOADED_OPERATOR_##Name,
  #include <clang/Basic/OperatorKinds.def>
};

const char *
clang_ext_OverloadedOperator_getSpelling(
  enum clang_ext_OverloadedOperatorKind kind);

struct clang_ext_DeclarationName {
  const void *data;
  CXTranslationUnit tu;
};

void
clang_ext_DeclarationName_dispose(struct clang_ext_DeclarationName name);

/* Copied from clang/AST/DeclarationName.h */
enum clang_ext_DeclarationNameKind {
  CLANG_EXT_DECLARATION_NAME_Identifier,
  CLANG_EXT_DECLARATION_NAME_ObjCZeroArgSelector,
  CLANG_EXT_DECLARATION_NAME_ObjCOneArgSelector,
  CLANG_EXT_DECLARATION_NAME_ObjCMultiArgSelector,
  CLANG_EXT_DECLARATION_NAME_CXXConstructorName,
  CLANG_EXT_DECLARATION_NAME_CXXDestructorName,
  CLANG_EXT_DECLARATION_NAME_CXXConversionFunctionName,
  CLANG_EXT_DECLARATION_NAME_CXXDeductionGuideName,
  CLANG_EXT_DECLARATION_NAME_CXXOperatorName,
  CLANG_EXT_DECLARATION_NAME_CXXLiteralOperatorName,
  CLANG_EXT_DECLARATION_NAME_CXXUsingDirective,
  CLANG_EXT_DECLARATION_NAME_InvalidDeclarationName
};

enum clang_ext_DeclarationNameKind
clang_ext_DeclarationName_getKind(
  struct clang_ext_DeclarationName);

enum clang_ext_OverloadedOperatorKind
clang_ext_DeclarationName_getCXXOverloadedOperator(
  struct clang_ext_DeclarationName);

CXType
clang_ext_DeclarationName_getCXXNameType(
  struct clang_ext_DeclarationName);

CXString
clang_ext_DeclarationName_getAsIdentifier(
  struct clang_ext_DeclarationName name);

CXCursor
clang_ext_DeclarationName_getCXXDeductionGuideTemplate(
  struct clang_ext_DeclarationName name);

CXString
clang_ext_DeclarationName_getCXXLiteralIdentifier(
  struct clang_ext_DeclarationName name);

struct clang_ext_DeclarationName
clang_ext_Decl_getName(CXCursor);

CXCursor
clang_ext_UsingDirectiveDecl_getNominatedNamespace(CXCursor);

struct clang_ext_NestedNameSpecifier {
  const void *data;
  CXTranslationUnit tu;
};

/* Copied from clang/AST/NestedNameSpecifier.h */
enum clang_ext_NestedNameSpecifierKind {
  CLANG_EXT_NESTED_NAME_SPECIFIER_InvalidNestedNameSpecifier,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Identifier,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Namespace,
  CLANG_EXT_NESTED_NAME_SPECIFIER_NamespaceAlias,
  CLANG_EXT_NESTED_NAME_SPECIFIER_TypeSpec,
  CLANG_EXT_NESTED_NAME_SPECIFIER_TypeSpecWithTemplate,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Global,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Super
};

enum clang_ext_NestedNameSpecifierKind
clang_ext_NestedNameSpecifier_getKind(
  struct clang_ext_NestedNameSpecifier);

struct clang_ext_NestedNameSpecifier
clang_ext_NestedNameSpecifier_getPrefix(
  struct clang_ext_NestedNameSpecifier);

CXString
clang_ext_NestedNameSpecifier_getAsIdentifier(
  struct clang_ext_NestedNameSpecifier);

CXCursor
clang_ext_NestedNameSpecifier_getAsNamespace(
  struct clang_ext_NestedNameSpecifier);

CXType
clang_ext_NestedNameSpecifier_getAsType(
  struct clang_ext_NestedNameSpecifier);

struct clang_ext_NestedNameSpecifierLoc {
  void *data;
  CXTranslationUnit tu;
};

void
clang_ext_NestedNameSpecifierLoc_dispose(
  struct clang_ext_NestedNameSpecifierLoc);

struct clang_ext_NestedNameSpecifier
clang_ext_NestedNameSpecifierLoc_getNestedNameSpecifier(
  struct clang_ext_NestedNameSpecifierLoc);

struct clang_ext_NestedNameSpecifierLoc
clang_ext_NestedNameSpecifierLoc_getPrefix(
  struct clang_ext_NestedNameSpecifierLoc);

struct clang_ext_TypeLoc
clang_ext_NestedNameSpecifierLoc_getAsTypeLoc(
  struct clang_ext_NestedNameSpecifierLoc);

struct clang_ext_NestedNameSpecifierLoc
clang_ext_Decl_getNestedNameSpecifierLoc(CXCursor);

struct clang_ext_NestedNameSpecifierLoc
clang_ext_TypeLoc_getQualifierLoc(struct clang_ext_TypeLoc);

struct clang_ext_NestedNameSpecifier
clang_ext_Type_getQualifier(CXType);

bool
clang_ext_TagDecl_isCompleteDefinition(CXCursor);

struct clang_ext_TypeLoc
clang_ext_CXXPseudoDestructorExpr_getDestroyedTypeLoc(CXCursor);

unsigned int
clang_ext_Cursor_getNumTemplateArgs(CXCursor);

struct clang_ext_TemplateArgument
clang_ext_Cursor_getTemplateArg(CXCursor, unsigned int);

struct clang_ext_TemplateParameterList {
  const void *data;
  CXTranslationUnit tu;
};

void
clang_ext_TemplateParameterList_dispose(
  struct clang_ext_TemplateParameterList);

unsigned int
clang_ext_TemplateParameterList_size(
  struct clang_ext_TemplateParameterList);

CXCursor
clang_ext_TemplateParameterList_getParam(
  struct clang_ext_TemplateParameterList, unsigned int);

CXCursor
clang_ext_TemplateParameterList_getRequiresClause(
  struct clang_ext_TemplateParameterList);

struct clang_ext_TemplateParameterList
clang_ext_TemplateDecl_getTemplateParameters(CXCursor);

unsigned int
clang_ext_TemplateDecl_getParameterCount(CXCursor);

CXCursor
clang_ext_TemplateDecl_getParameter(CXCursor, unsigned int);

CXCursor
clang_ext_SubstNonTypeTemplateParmExpr_getReplacement(CXCursor);

unsigned int
clang_ext_AttributedStmt_GetAttributeCount(CXCursor);

enum clang_ext_AttrKind
clang_ext_AttributedStmt_GetAttributeKind(CXCursor, unsigned int);

void
clang_ext_AttributedStmt_getAttrs(
  CXCursor, void (*)(CXCursor, void *), void *);

unsigned int
clang_ext_DecompositionDecl_GetBindingsCount(CXCursor);

CXCursor
clang_ext_DecompositionDecl_GetBindings(CXCursor, unsigned int);

enum clang_ext_AttrKind
clang_ext_Attr_GetKind(CXCursor);

/* From clang/Basic/ExceptionSpecificationType.h */
enum clang_ext_ExceptionSpecificationType {
  CLANG_EXT_EST_NoExceptionSpecification,
  CLANG_EXT_EST_DynamicNone,
  CLANG_EXT_EST_Dynamic,
  CLANG_EXT_EST_MSAny,
  CLANG_EXT_EST_NoThrow, /* Clang >=9.0.0 */
  CLANG_EXT_EST_BasicNoexcept,
  CLANG_EXT_EST_DependentNoexcept,
  /* EST_ComputedNoexcept (Clang <7.0.0) is mapped to
     CLANG_EXT_EST_DependentNoexcept */
  CLANG_EXT_EST_NoexceptFalse,
  CLANG_EXT_EST_NoexceptTrue,
  CLANG_EXT_EST_Unevaluated,
  CLANG_EXT_EST_Uninstantiated,
  CLANG_EXT_EST_Unparsed /* Clang >=3.6.0 */
};

enum clang_ext_ExceptionSpecificationType
clang_ext_FunctionProtoType_getExceptionSpecType(CXType);

unsigned int
clang_ext_FunctionProtoType_getNumExceptions(CXType);

CXType
clang_ext_FunctionProtoType_getExceptionType(CXType, unsigned int);

CXCursor
clang_ext_FunctionProtoType_getNoexceptExpr(CXType);

CXString
clang_ext_AsmStmt_GetAsmString(CXCursor);

unsigned int
clang_ext_AsmStmt_getNumOutputs(CXCursor);

CXString
clang_ext_AsmStmt_getOutputConstraint(CXCursor, unsigned int);

CXCursor
clang_ext_AsmStmt_getOutputExpr(CXCursor, unsigned int);

unsigned int
clang_ext_AsmStmt_getNumInputs(CXCursor);

CXString
clang_ext_AsmStmt_getInputConstraint(CXCursor, unsigned int);

CXCursor
clang_ext_AsmStmt_getInputExpr(CXCursor, unsigned int);

enum clang_ext_TypeLoc_Class {
  #define TYPELOC(Class, Base) CLANG_EXT_TYPELOC_##Class,
  #define ABSTRACT_TYPELOC(Class, Base)
  #include <clang/AST/TypeLocNodes.def>
  CLANG_EXT_TYPELOC_InvalidTypeLoc
};

struct clang_ext_TypeLoc
clang_ext_DeclaratorDecl_getTypeLoc(CXCursor);

void
clang_ext_TypeLoc_dispose(struct clang_ext_TypeLoc);

enum clang_ext_TypeLoc_Class
clang_ext_TypeLoc_getClass(struct clang_ext_TypeLoc);

CXType
clang_ext_TypeLoc_getType(struct clang_ext_TypeLoc);

CXCursor
clang_ext_ArrayTypeLoc_getSizeExpr(struct clang_ext_TypeLoc);

struct clang_ext_TypeLoc
clang_ext_ArrayTypeLoc_getElementLoc(struct clang_ext_TypeLoc);

struct clang_ext_TypeLoc
clang_ext_ParenTypeLoc_getInnerLoc(struct clang_ext_TypeLoc);

struct clang_ext_TypeLoc
clang_ext_PointerLikeTypeLoc_getPointeeLoc(struct clang_ext_TypeLoc);

struct clang_ext_TypeLoc
clang_ext_MemberPointerTypeLoc_getClassLoc(struct clang_ext_TypeLoc);

struct clang_ext_TypeLoc
clang_ext_QualifiedTypeLoc_getUnqualifiedLoc(struct clang_ext_TypeLoc tl);

struct clang_ext_TypeLoc
clang_ext_FunctionTypeLoc_getReturnLoc(struct clang_ext_TypeLoc);

unsigned
clang_ext_FunctionTypeLoc_getNumParams(struct clang_ext_TypeLoc);

CXCursor
clang_ext_FunctionTypeLoc_getParam(struct clang_ext_TypeLoc, unsigned int);

CXCursor
clang_ext_InitListExpr_getSyntacticForm(CXCursor);

CXCursor
clang_ext_InitListExpr_getSemanticForm(CXCursor);

unsigned int
clang_ext_InitListExpr_getNumInits(CXCursor);

CXCursor
clang_ext_InitListExpr_getInit(CXCursor, unsigned int);

enum clang_ext_DesignatedInitExpr_DesignatorKind {
  clang_ext_FieldDesignator,
  clang_ext_ArrayDesignator,
  clang_ext_ArrayRangeDesignator
};

unsigned int
clang_ext_DesignatedInitExpr_size(CXCursor);

enum clang_ext_DesignatedInitExpr_DesignatorKind
clang_ext_DesignatedInitExpr_getKind(CXCursor, unsigned int);

CXCursor
clang_ext_DesignatedInitExpr_getFieldDecl(CXCursor, unsigned int);

CXCursor
clang_ext_DesignatedInitExpr_getField(CXCursor, unsigned int);

CXCursor
clang_ext_DesignatedInitExpr_getArrayIndex(CXCursor, unsigned int);

CXCursor
clang_ext_DesignatedInitExpr_getArrayRangeStart(CXCursor, unsigned int);

CXCursor
clang_ext_DesignatedInitExpr_getArrayRangeEnd(CXCursor, unsigned int);

CXCursor
clang_ext_DesignatedInitExpr_getInit(CXCursor);

CXCursor
clang_ext_ConceptDecl_getConstraintExpr(CXCursor);

struct clang_ext_Requirement {
  const void *data;
  CXTranslationUnit tu;
};

void
clang_ext_Requirement_dispose(struct clang_ext_Requirement);

/* From clang/AST/ExprConcepts.h:Requirement::RequirementKind */
enum clang_ext_RequirementKind {
  clang_ext_RK_Type,
  clang_ext_RK_Simple,
  clang_ext_RK_Compound,
  clang_ext_RK_Nested
};

enum clang_ext_RequirementKind
clang_ext_Requirement_getKind(struct clang_ext_Requirement);

struct clang_ext_TypeLoc
clang_ext_TypeRequirement_getType(struct clang_ext_Requirement);

CXCursor
clang_ext_ExprRequirement_getExpr(struct clang_ext_Requirement);

struct clang_ext_TemplateParameterList
clang_ext_ExprRequirement_ReturnType_getTypeConstraintTemplateParameterList(
  struct clang_ext_Requirement);

CXCursor
clang_ext_ExprRequirement_ReturnType_getTypeConstraint(
  struct clang_ext_Requirement);

CXCursor
clang_ext_NestedRequirement_getConstraintExpr(struct clang_ext_Requirement);

unsigned int
clang_ext_RequiresExpr_getLocalParameterCount(CXCursor);

CXCursor
clang_ext_RequiresExpr_getLocalParameter(CXCursor, unsigned int);

unsigned int
clang_ext_RequiresExpr_getRequirementCount(CXCursor);

struct clang_ext_Requirement
clang_ext_RequiresExpr_getRequirement(CXCursor, unsigned int);

unsigned
clang_ext_DeclContext_visitDecls(
  CXCursor parent, CXCursorVisitor visitor, CXClientData client_data);

unsigned
clang_ext_IndirectFieldDecl_visitChain(
  CXCursor parent, CXCursorVisitor visitor, CXClientData client_data);

enum clang_ext_ElaboratedTypeKeyword
clang_ext_TagDecl_getTagKind(CXCursor);

bool
clang_ext_Decl_hasAttrs(CXCursor);

unsigned
clang_ext_Decl_getAttrCount(CXCursor);

CXCursor
clang_ext_Decl_getAttr(CXCursor, unsigned int);

bool
clang_ext_CursorKind_isAttr(enum CXCursorKind);

bool
clang_ext_FunctionDecl_isInlineSpecified(CXCursor);

bool
clang_ext_FunctionDecl_isInlined(CXCursor);

struct clang_ext_TypeLoc
clang_ext_Cursor_getTypeLoc(CXCursor);

struct clang_ext_VersionTuple {
  unsigned int major;
  unsigned int minor;
  unsigned int subminor;
  unsigned int build;
};

CXCursor
clang_ext_CXXForRangeStmt_getLoopVariable(CXCursor);

CXCursor
clang_ext_CXXForRangeStmt_getRangeInit(CXCursor);

CXCursor
clang_ext_CXXForRangeStmt_getBody(CXCursor);

struct clang_ext_TypeLoc
clang_ext_AttributedTypeLoc_getModifiedLoc(struct clang_ext_TypeLoc);

CXCursor
clang_ext_AttributedTypeLoc_getAttr(struct clang_ext_TypeLoc);

/* clang_Type_getModifiedType has been introduced in Clang 8 */
CXType
clang_ext_AttributedType_getModifiedType(CXType);

enum clang_ext_AttrKind
clang_ext_AttributedType_getAttrKind(CXType);

struct clang_ext_TypeLoc
clang_ext_ElaboratedTypeLoc_getNamedTypeLoc(struct clang_ext_TypeLoc);

struct clang_ext_TypeLoc
clang_ext_PackExpansionTypeLoc_getPatternLoc(struct clang_ext_TypeLoc);

struct clang_ext_TypeLoc
clang_ext_TypedefDecl_getUnderlyingTypeLoc(CXCursor);

CXCursor
clang_ext_CXXMethodDecl_getParent(CXCursor);

CXType
clang_ext_InjectedClassNameType_getInjectedSpecializationType(CXType);

CXType
clang_ext_Type_getUnqualifiedType(CXType);

bool
clang_ext_Type_isSugared(CXType);

CXType
clang_ext_Type_desugar(CXType);

struct clang_ext_OMPInteropInfo {
  const void *data;
  CXTranslationUnit tu;
};

void
clang_ext_OMPInteropInfo_dispose(struct clang_ext_OMPInteropInfo);

struct clang_ext_OMPTraitInfo {
  const void *data;
  CXTranslationUnit tu;
};

void
clang_ext_OMPTraitInfo_dispose(struct clang_ext_OMPTraitInfo);

struct clang_ext_CXXCtorInitializer {
  const void *data;
  CXTranslationUnit tu;
};

typedef enum CXVisitorResult (*CXXCtorInitializerVisitor)(
  struct clang_ext_CXXCtorInitializer, CXClientData client_data);

unsigned
clang_ext_CXXConstructorDecl_visitInitializers(
  CXCursor parent, CXXCtorInitializerVisitor visitor, CXClientData client_data);

void
clang_ext_CXXCtorInitializer_dispose(struct clang_ext_CXXCtorInitializer);

bool
clang_ext_CXXCtorInitializer_isBaseInitializer(struct clang_ext_CXXCtorInitializer);

bool
clang_ext_CXXCtorInitializer_isPackExpansion(struct clang_ext_CXXCtorInitializer);

bool
clang_ext_CXXCtorInitializer_isMemberInitializer(struct clang_ext_CXXCtorInitializer);

bool
clang_ext_CXXCtorInitializer_isIndirectMemberInitializer(struct clang_ext_CXXCtorInitializer);

bool
clang_ext_CXXCtorInitializer_isDelegatingInitializer(struct clang_ext_CXXCtorInitializer);

struct clang_ext_TypeLoc
clang_ext_CXXCtorInitializer_getTypeSourceInfo(struct clang_ext_CXXCtorInitializer);

CXCursor
clang_ext_CXXCtorInitializer_getMember(struct clang_ext_CXXCtorInitializer);

CXCursor
clang_ext_CXXCtorInitializer_getAnyMember(struct clang_ext_CXXCtorInitializer);

CXCursor
clang_ext_CXXCtorInitializer_getInit(struct clang_ext_CXXCtorInitializer);

unsigned int
clang_ext_FunctionDecl_getNumTemplateParameterLists(CXCursor);

struct clang_ext_TemplateParameterList
clang_ext_FunctionDecl_getTemplateParameterList(CXCursor, unsigned int);

CXType
clang_ext_AtomicType_getValueType(CXType);

enum clang_expr_AtomicOp {
  CLANG_EXT_AO_Invalid,
  #define BUILTIN(ID, TYPE, ATTRS)
  #define ATOMIC_BUILTIN(ID, TYPE, ATTRS) CLANG_EXT_AO ## ID,
  #include <clang/Basic/Builtins.def>
};

enum clang_expr_AtomicOp
clang_ext_AtomicExpr_getOp(CXCursor);

CXCursor
clang_ext_TypeOfExprType_getUnderlyingExpr(CXType);

CXType
clang_ext_TypeOfType_getUnderlyingType(CXType);

struct clang_ext_TypeLoc
clang_ext_TypeOfTypeLoc_getUnderlyingType(struct clang_ext_TypeLoc);

enum clang_ext_StorageClass {
  CLANG_EXT_SC_None,
  CLANG_EXT_SC_Extern,
  CLANG_EXT_SC_Static,
  CLANG_EXT_SC_PrivateExtern,
  CLANG_EXT_SC_OpenCLWorkGroupLocal, /* only with Clang 3.{4,5,6,7} */
  CLANG_EXT_SC_Auto,
  CLANG_EXT_SC_Register
};

/* clang_Cursor_getStorageClass is not available with Clang 3.{4,5} */
enum clang_ext_StorageClass
clang_ext_Decl_getStorageClass(CXCursor);

CXCursor
clang_ext_Type_getFieldDecl(CXType, const char *);

#include "libclang_extensions_attrs_headers.inc"
