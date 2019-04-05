#include <clang-c/Index.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Type.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include "clang/Frontend/ASTUnit.h"
#include "clang/Basic/SourceLocation.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>

extern "C" {
  #include "libclang_extensions.h"
}

// Copied from clang source tree: tools/libclang/CXString.cpp
enum CXStringFlag {
  /// CXString contains a 'const char *' that it doesn't own.
  CXS_Unmanaged,

  /// CXString contains a 'const char *' that it allocated with malloc().
  CXS_Malloc,

  /// CXString contains a CXStringBuf that needs to be returned to the
  /// CXStringPool.
  CXS_StringBuf
};

// Copied from clang source tree: tools/libclang/CXString.cpp
static CXString
cxstring_createRef(const char *String)
{
  CXString Str;
  Str.data = String;
  Str.private_flags = CXS_Unmanaged;
  return Str;
}

// Copied from clang source tree: tools/libclang/CXString.cpp
CXString
cxstring_createDup(llvm::StringRef String)
{
  CXString Result;
  char *Spelling = static_cast<char *>(malloc(String.size() + 1));
  if (Spelling == nullptr) {
    return cxstring_createRef("");
  }
  memmove(Spelling, String.data(), String.size());
  Spelling[String.size()] = 0;
  Result.data = Spelling;
  Result.private_flags = (unsigned) CXS_Malloc;
  return Result;
}

/*
static CXString cxstring_createDupFromString(std::string &s) {
  CXString Result;
  char *Spelling = static_cast<char *>(llvm::safe_malloc(s.size() + 1));
  memmove(Spelling, s.data(), s.size());
  Spelling[s.size()] = 0;
  Result.data = Spelling;
  Result.private_flags = CXS_Malloc;
  return Result;
}
*/

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Decl *
GetCursorDecl(CXCursor Cursor)
{
  return static_cast<const clang::Decl *>(Cursor.data[0]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Stmt *
GetCursorStmt(CXCursor Cursor)
{
  if (Cursor.kind == CXCursor_ObjCSuperClassRef ||
      Cursor.kind == CXCursor_ObjCProtocolRef ||
      Cursor.kind == CXCursor_ObjCClassRef)
    return nullptr;

  return static_cast<const clang::Stmt *>(Cursor.data[1]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Expr *
GetCursorExpr(CXCursor Cursor)
{
  return llvm::dyn_cast_or_null<clang::Expr>(GetCursorStmt(Cursor));
}

// From clang source tree: tools/libclang/CXType.cpp
static inline clang::QualType
GetQualType(CXType CT)
{
  return clang::QualType::getFromOpaquePtr(CT.data[0]);
}

// Copied from clang source tree: tools/libclang/CXType.cpp
static inline CXTranslationUnit
GetTU(CXType CT)
{
  return static_cast<CXTranslationUnit>(CT.data[1]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static CXTranslationUnit
getCursorTU(CXCursor Cursor)
{
  return static_cast<CXTranslationUnit>(const_cast<void*>(Cursor.data[2]));
}

// From clang source tree: tools/libclang/CXTranslationUnit.h
struct CXTranslationUnitImpl {
  void *CIdx;
  clang::ASTUnit *TheASTUnit;
  void *StringPool;
  void *Diagnostics;
  void *OverridenCursorsPool;
  void *CommentToXML;
  unsigned ParsingOptions;
  std::vector<std::string> Arguments;
};

CXInt Invalid_CXInt = { nullptr };

static inline CXInt
MakeCXInt(const llvm::APInt &value)
{
  CXInt result = { new llvm::APInt(value) };
  return result;
}

CXFloat Invalid_CXFloat = { nullptr };

static inline CXFloat
MakeCXFloat(const llvm::APFloat &value)
{
  CXFloat result = { new llvm::APFloat(value) };
  return result;
}

/* Copied from clang source tree: tools/libclang/CXCursor.cpp */
static CXCursor
MakeCXCursorInvalid(CXCursorKind K, CXTranslationUnit TU)
{
  assert(K >= CXCursor_FirstInvalid && K <= CXCursor_LastInvalid);
  CXCursor C = { K, 0, { nullptr, nullptr, TU } };
  return C;
}

static CXType
MakeCXTypeInvalid(CXTranslationUnit TU)
{
  CXType CT = { CXType_Invalid, { nullptr, TU }};
  return CT;
}

/* MakeCXCursor is not exported in libclang.
   The following implementation makes a (not well-formed) cursor on an
   OpaqueValueExpr with E as source expression. Visiting the (single) child of
   this cursor calls libclang's MakeCXCursor on E.
*/
enum CXChildVisitResult
MakeCXCursor_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
  *((CXCursor *) client_data) = cursor;
  return CXChildVisit_Break;
}

static CXCursor
MakeCXCursor(clang::Expr *E, CXTranslationUnit TU)
{
  clang::OpaqueValueExpr OV(
    clang::SourceLocation::getFromRawEncoding(0), E->getType(),
    clang::VK_RValue, clang::OK_Ordinary, E);
  CXCursor C = { CXCursor_FirstExpr, 0, { nullptr, &OV, TU }};
  CXCursor Result;
  clang_visitChildren(C, MakeCXCursor_visitor, &Result);
  return Result;
}

/* The following implementation makes a (not well-formed) cursor on a
   default statement with S as substatement. Visiting the (single) child of
   this cursor calls libclang's MakeCXCursor on S.
*/
static CXCursor
MakeCXCursor(const clang::Stmt *S, CXTranslationUnit TU)
{
  clang::DefaultStmt CS(
    clang::SourceLocation::getFromRawEncoding(0),
    clang::SourceLocation::getFromRawEncoding(0), const_cast<clang::Stmt *>(S));
  CXCursor C = { CXCursor_CompoundStmt, 0, { nullptr, &CS, TU }};
  CXCursor Result;
  clang_visitChildren(C, MakeCXCursor_visitor, &Result);
  return Result;
}

static CXCursor
MakeCXCursor(const clang::Decl *T, CXTranslationUnit TU)
{
  clang::DeclGroupRef DGR(const_cast<clang::Decl *>(T));
  clang::DeclStmt DS(DGR,
    clang::SourceLocation::getFromRawEncoding(0),
    clang::SourceLocation::getFromRawEncoding(0));
  CXCursor C = { CXCursor_DeclStmt, 0, { nullptr, &DS, TU }};
  CXCursor Result;
  clang_visitChildren(C, MakeCXCursor_visitor, &Result);
  return Result;
}

/* MakeCXType is not exported in libclang.
   The following implementation makes a (not well-formed) cursor on an
   OpaqueValueExpr of type T. Querying the type of this cursor calls
   libclang's MakeCXType on T.
*/
static CXType
MakeCXType(clang::QualType T, CXTranslationUnit TU)
{
  clang::OpaqueValueExpr OV(
    clang::SourceLocation::getFromRawEncoding(0), T, clang::VK_RValue);
  CXCursor C = { CXCursor_FirstExpr, 0, { nullptr, &OV, TU }};
  return clang_getCursorType(C);
}

static const clang::FunctionDecl *
getFunctionDecl(CXCursor C)
{
  if (auto *D = GetCursorDecl(C)) {
    const clang::FunctionDecl *FD;
    #ifdef LLVM_VERSION_BEFORE_3_5_0
      if (nullptr != (FD = llvm::dyn_cast_or_null<clang::FunctionDecl>(D))) {
      }
      else if (auto FTD =
            llvm::dyn_cast_or_null<clang::FunctionTemplateDecl>(D)) {
        FD = FTD->getTemplatedDecl();
      }
      else {
        FD = nullptr;
      }
    #else
      FD = D->getAsFunction();
    #endif
    return FD;
  }
  return nullptr;
}

[[maybe_unused]]
static const clang::CXXMethodDecl *
getMethodDecl(CXCursor C)
{
  if (auto *FD = getFunctionDecl(C)) {
    if (auto *Method = llvm::dyn_cast_or_null<clang::CXXMethodDecl>(FD)) {
      return Method;
    }
  }
  return nullptr;
}

static const clang::CXXConstructorDecl *
getConstructorDecl(CXCursor C)
{
  if (auto *FD = getFunctionDecl(C)) {
    const clang::CXXConstructorDecl *Constructor =
      llvm::dyn_cast_or_null<clang::CXXConstructorDecl>(FD);
    return Constructor;
  }
  return nullptr;
}

static struct clang_ext_TemplateName
MakeTemplateName(const clang::TemplateName &name, CXTranslationUnit TU)
{
  struct clang_ext_TemplateName TN = {
    new clang::TemplateName(name), TU };
  return TN;
}

static struct clang_ext_TemplateName
MakeTemplateNameInvalid(CXTranslationUnit TU)
{
  struct clang_ext_TemplateName TN = { nullptr, TU };
  return TN;
}

static const clang::TemplateName *
GetTemplateName(struct clang_ext_TemplateName CTN)
{
  return static_cast<const clang::TemplateName *>(CTN.data);
}

static struct clang_ext_TemplateArgument
MakeTemplateArgument(const clang::TemplateArgument &argument, CXTranslationUnit TU)
{
  struct clang_ext_TemplateArgument TA = {
    new clang::TemplateArgument(argument), TU };
  return TA;
}

static struct clang_ext_TemplateArgument
MakeTemplateArgumentInvalid(CXTranslationUnit TU)
{
  struct clang_ext_TemplateArgument TA = { nullptr, TU };
  return TA;
}

static const clang::TemplateArgument *
GetTemplateArgument(struct clang_ext_TemplateArgument CTA)
{
  return static_cast<const clang::TemplateArgument *>(CTA.data);
}

extern "C" {
  bool
  clang_equal_cxint(CXInt a, CXInt b)
  {
    if (auto i = static_cast<llvm::APInt *>(a.data)) {
      if (auto j = static_cast<llvm::APInt *>(b.data)) {
        return *i == *j;
      }
    }
    return false;
  }

  int
  clang_compare_cxint(CXInt a, CXInt b)
  {
    if (auto i = static_cast<llvm::APInt *>(a.data)) {
      if (auto j = static_cast<llvm::APInt *>(b.data)) {
        if (i->ult(*j)) { // sadly, APInt::compare is private
          return -1;
        }
        else if (j->ult(*i)) {
          return 1;
        }
        else {
          return 0;
        }
      }
    }
    return -1;
  }

  CXInt
  clang_ext_IntegerLiteral_getValue(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::IntegerLiteral>(GetCursorStmt(c))) {
      return MakeCXInt(e->getValue());
    }
    return Invalid_CXInt;
  }

  void
  clang_ext_Int_dispose(CXInt c)
  {
    delete(static_cast<llvm::APInt *>(c.data));
    c.data = nullptr;
  }

  bool
  clang_ext_Int_isValid(CXInt c)
  {
    return c.data != nullptr;
  }

  CXString
  clang_ext_Int_toString(CXInt c, unsigned Radix, bool isSigned)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      std::string s = i->toString(Radix, isSigned);
      return cxstring_createDup(s);
    }
    return cxstring_createRef("");
  }

  double
  clang_ext_Int_roundToDouble(CXInt c, bool isSigned)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->roundToDouble(isSigned);
    }
    return 0.;
  }

  float
  clang_ext_Int_bitsToFloat(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->bitsToFloat();
    }
    return 0.f;
  }

  unsigned
  clang_ext_Int_getBitWidth(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getBitWidth();
    }
    return 0;
  }

  unsigned
  clang_ext_Int_getActiveBits(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getActiveBits();
    }
    return 0;
  }

  unsigned
  clang_ext_Int_getMinSignedBits(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getMinSignedBits();
    }
    return 0;
  }

  bool
  clang_ext_Int_getBoolValue(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getBoolValue();
    }
    return false;
  }

  int
  clang_ext_Int_getSExtValue(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getSExtValue();
    }
    return 0;
  }

  int64_t
  clang_ext_Int_getSExtValue64(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getSExtValue();
    }
    return 0;
  }

  CXFloat
  clang_ext_FloatingLiteral_getValue(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::FloatingLiteral>(GetCursorStmt(c))) {
      return MakeCXFloat(e->getValue());
    }
    return Invalid_CXFloat;
  }

  bool
  clang_equal_cxfloat(CXFloat a, CXFloat b)
  {
    if (auto x = static_cast<llvm::APFloat *>(a.data)) {
      if (auto y = static_cast<llvm::APFloat *>(b.data)) {
        return x->compare(*y) == llvm::APFloat::cmpEqual;
      }
    }
    return false;
  }

  int
  clang_compare_cxfloat(CXFloat a, CXFloat b)
  {
    if (auto x = static_cast<llvm::APFloat *>(a.data)) {
      if (auto y = static_cast<llvm::APFloat *>(b.data)) {
        switch (x->compare(*y)) {
        case llvm::APFloat::cmpLessThan : return -1;
        case llvm::APFloat::cmpEqual : return 0;
        case llvm::APFloat::cmpGreaterThan : return 1;
        case llvm::APFloat::cmpUnordered : ;
        }
      }
    }
    return -1;
  }

  void
  clang_ext_Float_dispose(CXFloat c)
  {
    delete(static_cast<llvm::APFloat *>(c.data));
    c.data = nullptr;
  }

  bool
  clang_ext_Float_isValid(CXFloat c)
  {
    return c.data != nullptr;
  }

  CXString
  clang_ext_Float_toString(CXFloat c)
  {
    if (auto f = static_cast<llvm::APFloat *>(c.data)) {
      llvm::SmallString<40> S;
      f->toString(S);
      return cxstring_createDup(S.str());
    }
    return cxstring_createRef("");
  }

  double
  clang_ext_Float_convertToDouble(CXFloat c)
  {
    if (auto f = static_cast<llvm::APFloat *>(c.data)) {
      return f->convertToDouble();
    }
    return 0.;
  }

  CXString
  clang_ext_StringLiteral_GetString(CXCursor c)
  {
    const clang::Expr *e = GetCursorExpr(c);
    if (auto m = llvm::dyn_cast_or_null<clang::StringLiteral>(e)) {
      return cxstring_createDup(m->getString());
    }
    return cxstring_createRef("");
  }

  enum clang_ext_UnaryOperatorKind
  clang_ext_UnaryOperator_getOpcode(CXCursor c)
  {
    if (auto Op =
        llvm::dyn_cast_or_null<clang::UnaryOperator>(GetCursorStmt(c))) {
      return static_cast<clang_ext_UnaryOperatorKind>(Op->getOpcode());
    }
    return static_cast<clang_ext_UnaryOperatorKind>(0);
  }

  CXString
  clang_ext_UnaryOperator_getOpcodeSpelling(
      enum clang_ext_UnaryOperatorKind Kind)
  {
    switch (Kind) {
#define UNARY_OPERATION(Name, Spelling)        \
      case CLANG_EXT_UNARY_OPERATOR_##Name:     \
        return cxstring_createRef(Spelling);
#include "clangml_OperationKinds.def"
    }
    //llvm_unreachable("Unsupported UnaryOperatorKind");
    return cxstring_createRef("");
  }

  enum clang_ext_BinaryOperatorKind
  clang_ext_BinaryOperator_getOpcode(CXCursor c)
  {
    if (auto Op =
        llvm::dyn_cast_or_null<clang::BinaryOperator>(GetCursorStmt(c))) {
      return static_cast<clang_ext_BinaryOperatorKind>(Op->getOpcode());
    }
    return static_cast<clang_ext_BinaryOperatorKind>(0);
  }

  CXString
  clang_ext_BinaryOperator_getOpcodeSpelling(
      enum clang_ext_BinaryOperatorKind Kind)
  {
    switch (Kind) {
#define BINARY_OPERATION(Name, Spelling)        \
      case CLANG_EXT_BINARY_OPERATOR_##Name:     \
        return cxstring_createRef(Spelling);
#include "clangml_OperationKinds.def"
    }
    //llvm_unreachable("Unsupported BinaryOperatorKind");
    return cxstring_createRef("");
  }

  unsigned
  clang_ext_ForStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::ForStmt>(GetCursorStmt(c))) {
      unsigned Result = 0;
      if (S->getInit()) {
        Result |= CLANG_EXT_FOR_STMT_INIT;
      }
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_FOR_STMT_CONDITION_VARIABLE;
      }
      if (S->getCond()) {
        Result |= CLANG_EXT_FOR_STMT_COND;
      }
      if (S->getInc()) {
        Result |= CLANG_EXT_FOR_STMT_INC;
      }
      return Result;
    }
    return 0;
  }

  unsigned
  clang_ext_IfStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::IfStmt>(GetCursorStmt(c))) {
      unsigned Result = 0;
      #ifndef LLVM_VERSION_BEFORE_3_9_0
      if (S->getInit()) {
        Result |= CLANG_EXT_IF_STMT_INIT;
      }
      #endif
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_IF_STMT_CONDITION_VARIABLE;
      }
      if (S->getElse()) {
        Result |= CLANG_EXT_IF_STMT_ELSE;
      }
      return Result;
    }
    return 0;
  }

  CXCursor
  clang_ext_IfStmt_getInit(CXCursor c)
  {
    #ifndef LLVM_VERSION_BEFORE_3_9_0
    if (auto S = llvm::dyn_cast_or_null<clang::IfStmt>(GetCursorStmt(c))) {
      return MakeCXCursor(S->getInit(), getCursorTU(c));
    }
    #endif
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  unsigned
  clang_ext_SwitchStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::SwitchStmt>(GetCursorStmt(c))) {
      unsigned Result = 0;
      #ifndef LLVM_VERSION_BEFORE_3_9_0
      if (S->getInit()) {
        Result |= CLANG_EXT_SWITCH_STMT_INIT;
      }
      #endif
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_SWITCH_STMT_CONDITION_VARIABLE;
      }
      return Result;
    }
    return 0;
  }

  CXCursor
  clang_ext_SwitchStmt_getInit(CXCursor c)
  {
    #ifndef LLVM_VERSION_BEFORE_3_9_0
    if (auto S = llvm::dyn_cast_or_null<clang::SwitchStmt>(GetCursorStmt(c))) {
      return MakeCXCursor(S->getInit(), getCursorTU(c));
    }
    #endif
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  unsigned
  clang_ext_WhileStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::WhileStmt>(GetCursorStmt(c))) {
      unsigned Result = 0;
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_WHILE_STMT_CONDITION_VARIABLE;
      }
      return Result;
    }
    return 0;
  }

  enum clang_ext_ElaboratedTypeKeyword
  clang_ext_ElaboratedType_getKeyword(
      CXType CT)
  {
    clang::QualType T = GetQualType(CT);
    const clang::Type *TP = T.getTypePtrOrNull();
    if (TP && TP->getTypeClass() == clang::Type::Elaborated) {
      return static_cast<enum clang_ext_ElaboratedTypeKeyword>(
        clang::cast<clang::ElaboratedType>(TP)->getKeyword());
    }
    return static_cast<enum clang_ext_ElaboratedTypeKeyword>(0);
  }

  CXString
  clang_ext_ElaboratedType_getKeywordSpelling(
      enum clang_ext_ElaboratedTypeKeyword keyword)
  {
    return cxstring_createDup(clang::TypeWithKeyword::getKeywordName(
      static_cast<clang::ElaboratedTypeKeyword>(keyword)));
  }

  bool
  clang_ext_VarDecl_hasInit(CXCursor c)
  {
    if (auto D = llvm::dyn_cast_or_null<clang::VarDecl>(GetCursorDecl(c))) {
      return D->getInit() != nullptr;
    }
    return false;
  }

  bool
  clang_ext_MemberRefExpr_isArrow(CXCursor c)
  {
    const clang::Expr *e = GetCursorExpr(c);
    if (auto m = llvm::dyn_cast_or_null<clang::MemberExpr>(e)) {
      return m->isArrow();
    }
    return false;
  }

  CXString
  clang_ext_Stmt_GetClassName(CXCursor c)
  {
    const clang::Stmt *s = GetCursorStmt(c);
    return cxstring_createRef(s->getStmtClassName());
  }

  int
  clang_ext_Stmt_GetClassKind(CXCursor c)
  {
    const clang::Stmt *s = GetCursorStmt(c);
    return s->getStmtClass();
  }

  enum clang_ext_CursorKind
  clang_ext_GetCursorKind(CXCursor c)
  {
    switch (c.kind) {
    case CXCursor_UnexposedDecl:
      {
        const clang::Decl *d = GetCursorDecl(c);
        #define CASE(X) case clang::Decl::X: return ECK_##X##Decl
        switch (d->getKind()) {
        CASE(Empty);
        CASE(LinkageSpec);
        default:
          return ECK_Unknown;
        }
        #undef CASE
      }
      return ECK_Unknown;
    default:
      if (const clang::Stmt *s = GetCursorStmt(c)) {
        #define CASE(X) case clang::Stmt::X##Class: return ECK_##X
        switch (s->getStmtClass()) {
        CASE(ImplicitCastExpr);
        CASE(BinaryConditionalOperator);
        CASE(UnaryExprOrTypeTraitExpr);
        default:
          return ECK_Unknown;
        }
        #undef CASE
      }
      return ECK_Unknown;
    }
  }

  enum clang_ext_DeclKind
  clang_ext_Decl_GetKind(CXCursor c)
  {
    if (auto *d = GetCursorDecl(c)) {
      switch (d->getKind()) {
      #define DECL(Derived, Base) \
        case clang::Decl::Derived: return CLANG_EXT_DECL_##Derived;
      #define ABSTRACT_DECL(Decl)
      #include <clang/AST/DeclNodes.inc>
      default:
        return CLANG_EXT_DECL_Unknown;
      }
    }
    return CLANG_EXT_DECL_Invalid;
  }

  enum clang_ext_TypeKind
  clang_ext_GetTypeKind(CXType c)
  {
    clang::QualType T = GetQualType(c);
    if (auto TP = T.getTypePtrOrNull()) {
      switch (TP->getTypeClass()) {
      #define TYPE(Class, Base) case clang::Type::Class: return CLANG_EXT_TYPE_##Class;
      #define ABSTRACT_TYPE(Class, Base)
      #include <clang/AST/TypeNodes.def>
      default:
        return CLANG_EXT_TYPE_Unknown;
      }
    }
    return CLANG_EXT_TYPE_Invalid;
  }

  CXType
  clang_ext_GetInnerType(CXType c)
  {
    clang::QualType T = GetQualType(c);
    if (auto PTT = T->getAs<clang::ParenType>()) {
      return MakeCXType(PTT->getInnerType(), GetTU(c));
    }
    return c;
  }

  CXCursor
  clang_ext_VariableArrayType_GetSizeExpr(CXType c)
  {
    clang::QualType T = GetQualType(c);
    if (auto TP = T.getTypePtrOrNull()) {
      switch (TP->getTypeClass()) {
      case clang::Type::VariableArray:
        return MakeCXCursor(
          clang::cast<clang::VariableArrayType>(TP)->getSizeExpr(), GetTU(c));
      default:
        break;
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, GetTU(c));
  }

  CXString
  clang_ext_AsmStmt_GetAsmString(CXCursor c)
  {
    const clang::Stmt *s = GetCursorStmt(c);
    switch (s->getStmtClass()) {
    case clang::Stmt::GCCAsmStmtClass:;
      return cxstring_createDup(
        clang::cast<clang::GCCAsmStmt>(s)->getAsmString()->getString());
    case clang::Stmt::MSAsmStmtClass:
      return cxstring_createDup(
        clang::cast<clang::MSAsmStmt>(s)->getAsmString());
    default:
      return cxstring_createRef("");
    }
  }

  enum clang_ext_CharacterKind
  clang_ext_CharacterLiteral_GetCharacterKind(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CharacterLiteral>(GetCursorStmt(c))) {
      return static_cast<enum clang_ext_CharacterKind>(e->getKind());
    }
    return ECK_Ascii;
  }

  unsigned
  clang_ext_CharacterLiteral_GetValue(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CharacterLiteral>(GetCursorStmt(c))) {
      return e->getValue();
    }
    return 0;
  }

  enum clang_ext_UnaryExpr
  clang_ext_UnaryExpr_GetKind(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(GetCursorStmt(c))) {
      return static_cast<enum clang_ext_UnaryExpr>(e->getKind());
    }
    return UETT_SizeOf;
  }

  CXType
  clang_ext_UnaryExpr_GetArgumentType(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(GetCursorStmt(c))) {
      return MakeCXType(e->getArgumentType(), getCursorTU(c));
    }
    return MakeCXTypeInvalid(getCursorTU(c));
  }

  CXType
  clang_ext_Type_getNamedType(CXType CT)
  {
    #ifdef LLVM_VERSION_BEFORE_3_9_0
      clang::QualType T = GetQualType(CT);
      const clang::Type *TP = T.getTypePtrOrNull();
      if (TP && TP->getTypeClass() == clang::Type::Elaborated) {
        return MakeCXType(
          llvm::cast<clang::ElaboratedType>(TP)->getNamedType(), GetTU(CT));
      }
      return MakeCXTypeInvalid(GetTU(CT));
    #else
      return clang_Type_getNamedType(CT);
    #endif
  }

  enum clang_ext_AttrKind
  clang_ext_Type_GetAttributeKind(CXType CT)
  {
    clang::QualType T = GetQualType(CT);
    if (auto *ATT = T->getAs<clang::AttributedType>()) {
      return (enum clang_ext_AttrKind) ATT->getAttrKind();
    }
    return CLANG_EXT_ATTR_NoAttr;
  }

  CXString
  clang_ext_AttrKind_GetSpelling(enum clang_ext_AttrKind AttrKind)
  {
    switch (AttrKind) {
#define ATTR(Name)                              \
      case CLANG_EXT_ATTR_##Name:               \
        return cxstring_createRef(#Name);
#include <clang/Basic/AttrList.inc>
    default:
      return cxstring_createRef("");
    }
  }

  unsigned
  clang_ext_CXXMethod_isDefaulted(CXCursor C) {
    #ifdef LLVM_VERSION_BEFORE_3_9_0
      if (auto *Method = getMethodDecl(C)) {
        return Method->isDefaulted();
      }
      return 0;
    #else
      return clang_CXXMethod_isDefaulted(C);
    #endif
  }

  unsigned
  clang_ext_CXXMethod_isConst(CXCursor C) {
    #ifdef LLVM_VERSION_BEFORE_3_5_0
      if (auto *Method = getMethodDecl(C)) {
        return Method->isConst();
      }
      return 0;
    #else
      return clang_CXXMethod_isConst(C);
    #endif
  }

  unsigned
  clang_ext_CXXConstructor_isExplicit(CXCursor C)
  {
    if (auto *Constructor = getConstructorDecl(C)) {
      return Constructor->isExplicit();
    }
    return false;
  }

  unsigned
  clang_ext_FunctionDecl_isDeleted(CXCursor C)
  {
    if (auto *Function = getFunctionDecl(C)) {
      return Function->isDeleted();
    }
    return false;
  }

  unsigned
  clang_ext_FunctionDecl_getNumParams(CXCursor C)
  {
    if (auto *Function = getFunctionDecl(C)) {
      return Function->getNumParams();
    }
    return 0;
  }

  CXCursor
  clang_ext_FunctionDecl_getParamDecl(CXCursor C, unsigned i)
  {
    if (auto *Function = getFunctionDecl(C)) {
      return MakeCXCursor(Function->getParamDecl(i), getCursorTU(C));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(C));
  }

  unsigned
  clang_ext_LinkageSpecDecl_getLanguageIDs(CXCursor C)
  {
    if (auto *D = GetCursorDecl(C)) {
      if (auto *LS = llvm::dyn_cast_or_null<clang::LinkageSpecDecl>(D)) {
        return LS->getLanguage();
      }
    }
    return 0;
  }

  CXType
  clang_ext_TemplateTypeParmDecl_getDefaultArgument(CXCursor C)
  {
    if (auto *D = GetCursorDecl(C)) {
      if (auto TTPD = llvm::dyn_cast_or_null<clang::TemplateTypeParmDecl>(D)) {
        if (TTPD->hasDefaultArgument()) {
          return MakeCXType(TTPD->getDefaultArgument(), getCursorTU(C));
        }
      }
    }
    return MakeCXTypeInvalid(getCursorTU(C));
  }

  void
  clang_ext_TemplateName_dispose(struct clang_ext_TemplateName CTN)
  {
    if (auto *TN = GetTemplateName(CTN)) {
      delete TN;
    }
  }

  enum clang_ext_TemplateName_NameKind
  clang_ext_TemplateName_getKind(struct clang_ext_TemplateName CTN)
  {
    if (auto *TN = GetTemplateName(CTN)) {
      return static_cast<enum clang_ext_TemplateName_NameKind>(TN->getKind());
    }
    return CLANG_EXT_InvalidNameKind;
  }

  CXCursor
  clang_ext_TemplateName_getAsTemplateDecl(struct clang_ext_TemplateName CTN)
  {
    if (auto *TN = GetTemplateName(CTN)) {
      return MakeCXCursor(TN->getAsTemplateDecl(), CTN.TU);
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, CTN.TU);
  }

  enum CXTemplateArgumentKind
  clang_ext_TemplateArgument_getKind(struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return static_cast<enum CXTemplateArgumentKind>(TA->getKind());
    }
    return CXTemplateArgumentKind_Invalid;
  }

  CXType
  clang_ext_TemplateArgument_getAsType(struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXType(TA->getAsType(), CTA.TU);
    }
    return MakeCXTypeInvalid(CTA.TU);
  }

  CXCursor
  clang_ext_TemplateArgument_getAsDecl(struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXCursor(TA->getAsDecl(), CTA.TU);
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, CTA.TU);
  }

  CXType
  clang_ext_TemplateArgument_getNullPtrType(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXType(TA->getNullPtrType(), CTA.TU);
    }
    return MakeCXTypeInvalid(CTA.TU);
  }

  struct clang_ext_TemplateName
  clang_ext_TemplateArgument_getAsTemplateOrTemplatePattern(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeTemplateName(TA->getAsTemplate(), CTA.TU);
    }
    return MakeTemplateNameInvalid(CTA.TU);
  }

  CXInt
  clang_ext_TemplateArgument_getAsIntegral(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXInt(TA->getAsIntegral());
    }
    return Invalid_CXInt;
  }

  CXType
  clang_ext_TemplateArgument_getIntegralType(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXType(TA->getIntegralType(), CTA.TU);
    }
    return MakeCXTypeInvalid(CTA.TU);
  }

  CXType
  clang_ext_TemplateArgument_getNonTypeTemplateArgumentType(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXType(TA->getNonTypeTemplateArgumentType(), CTA.TU);
    }
    return MakeCXTypeInvalid(CTA.TU);
  }

  CXCursor
  clang_ext_TemplateArgument_getAsExpr(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXCursor(TA->getAsExpr(), CTA.TU);
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, CTA.TU);
  }

  void
  clang_ext_TemplateArgument_dispose(struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      delete TA;
    }
  }

  struct clang_ext_TemplateName
  clang_ext_TemplateSpecializationType_getTemplateName(CXType CT)
  {
    clang::QualType T = GetQualType(CT);
    if (auto *TST = T->getAs<clang::TemplateSpecializationType>()) {
      return MakeTemplateName(TST->getTemplateName(), GetTU(CT));
    }
    return MakeTemplateNameInvalid(GetTU(CT));
  }

  unsigned
  clang_ext_TemplateSpecializationType_getNumArgs(CXType CT)
  {
    clang::QualType T = GetQualType(CT);
    if (auto *TST = T->getAs<clang::TemplateSpecializationType>()) {
      return TST->getNumArgs();
    }
    return 0;
  }

  struct clang_ext_TemplateArgument
  clang_ext_TemplateSpecializationType_getArgument(CXType CT, unsigned i)
  {
    clang::QualType T = GetQualType(CT);
    if (auto *TST = T->getAs<clang::TemplateSpecializationType>()) {
      return MakeTemplateArgument(TST->getArg(i), GetTU(CT));
    }
    return MakeTemplateArgumentInvalid(GetTU(CT));
  }
}
