#include <clang-c/Index.h>
#include <clang/AST/Attr.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Type.h>
#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/ExceptionSpecificationType.h>
#include <clang/Frontend/ASTUnit.h>
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

extern "C" void
failwith_fmt(const char* format, ...);

static bool
is_valid_decl(enum CXCursorKind kind)
{
  return
    (kind >= CXCursor_FirstDecl && kind <= CXCursor_LastDecl) ||
    (kind >= CXCursor_FirstExtraDecl && kind <= CXCursor_LastExtraDecl) ||
    (kind == CXCursor_TemplateRef);
}

static bool
is_valid_stmt(enum CXCursorKind kind)
{
  return
    (kind >= CXCursor_FirstExpr && kind <= CXCursor_LastExpr) ||
    (kind >= CXCursor_FirstStmt && kind <= CXCursor_LastStmt);
}

static bool
is_valid_attr(enum CXCursorKind kind)
{
  return (kind >= CXCursor_FirstAttr && kind <= CXCursor_LastAttr);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Decl *
GetCursorDecl(CXCursor cursor)
{
  if (!is_valid_decl(cursor.kind)) {
    failwith_fmt("GetCursorDecl");
  }
  return static_cast<const clang::Decl *>(cursor.data[0]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Stmt *
GetCursorStmt(CXCursor cursor)
{
  assert(is_valid_stmt(cursor.kind));
  return static_cast<const clang::Stmt *>(cursor.data[1]);
}

static const clang::Attr *
GetCursorAttr(CXCursor cursor)
{
  assert(is_valid_attr(cursor.kind));
  return static_cast<const clang::Attr *>(cursor.data[1]);
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
   The following implementation makes a (not well-formed) cursor on a
   default statement with S as substatement. Visiting the (single) child of
   this cursor calls libclang's MakeCXCursor on S.
*/
enum CXChildVisitResult
MakeCXCursor_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
  *((CXCursor *) client_data) = cursor;
  return CXChildVisit_Break;
}

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
  CXCursor C = { CXCursor_UnexposedStmt, 0, { T, nullptr, TU }};
  return clang_getCursorSemanticParent(C);
}

/* MakeCXType is not exported in libclang.
   The following implementation makes a (not well-formed) CXType for a pointer
   to values of type T. Querying the type of the pointee type calls libclang's
   MakeCXType on T.
*/
static CXType
MakeCXType(clang::QualType T, CXTranslationUnit TU)
{
  clang::QualType P = TU->TheASTUnit->getASTContext().getPointerType(T);
  CXType CT = { CXType_Invalid, { P.getAsOpaquePtr(), TU }};
  return clang_getPointeeType(CT);
}

static const clang::FunctionDecl *
getFunctionDecl(CXCursor cursor)
{
  auto *D = GetCursorDecl(cursor);
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
GetTemplateName(struct clang_ext_TemplateName TN)
{
  return static_cast<const clang::TemplateName *>(TN.data);
}

static struct clang_ext_TemplateArgument
MakeTemplateArgument(
  const clang::TemplateArgument &argument, CXTranslationUnit TU)
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
GetTemplateArgument(struct clang_ext_TemplateArgument TA)
{
  return static_cast<const clang::TemplateArgument *>(TA.data);
}

#ifdef LLVM_VERSION_BEFORE_3_5_0
typedef clang::LambdaExpr::Capture LambdaCapture;
#else
typedef clang::LambdaCapture LambdaCapture;
#endif

static struct clang_ext_LambdaCapture
MakeLambdaCapture(
  const LambdaCapture &argument, CXTranslationUnit TU)
{
  struct clang_ext_LambdaCapture LC = {
    new LambdaCapture(argument), TU };
  return LC;
}

static struct clang_ext_LambdaCapture
MakeLambdaCaptureInvalid(CXTranslationUnit TU)
{
  struct clang_ext_LambdaCapture LC = { nullptr, TU };
  return LC;
}

static const LambdaCapture *
GetLambdaCapture(struct clang_ext_LambdaCapture LC)
{
  return static_cast<const LambdaCapture *>(LC.data);
}

static clang::ASTContext &
getCursorContext(CXCursor c)
{
  return getCursorTU(c)->TheASTUnit->getASTContext();
}

static struct clang_ext_DeclarationName
MakeDeclarationName(
  const clang::DeclarationName name, CXTranslationUnit tu)
{
  struct clang_ext_DeclarationName result = {
    new clang::DeclarationName(name), tu };
  return result;
}

static struct clang_ext_DeclarationName
MakeDeclarationNameInvalid(CXTranslationUnit tu)
{
  struct clang_ext_DeclarationName result = { nullptr, tu };
  return result;
}

static const clang::DeclarationName *
GetDeclarationName(struct clang_ext_DeclarationName name)
{
  return static_cast<const clang::DeclarationName *>(name.data);
}

static struct clang_ext_NestedNameSpecifier
MakeNestedNameSpecifier(
  const clang::NestedNameSpecifier *specifier, CXTranslationUnit tu)
{
  struct clang_ext_NestedNameSpecifier result = { specifier, tu };
  return result;
}

static struct clang_ext_NestedNameSpecifier
MakeNestedNameSpecifierInvalid(CXTranslationUnit tu)
{
  struct clang_ext_NestedNameSpecifier result = { nullptr, tu };
  return result;
}

static const clang::NestedNameSpecifier *
GetNestedNameSpecifier(struct clang_ext_NestedNameSpecifier specifier)
{
  return static_cast<const clang::NestedNameSpecifier *>(specifier.data);
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
  clang_ext_IntegerLiteral_getValue(CXCursor cursor)
  {
    auto s = GetCursorStmt(cursor);
    if (auto e = llvm::dyn_cast_or_null<clang::IntegerLiteral>(s)) {
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

  enum clang_ext_fltSemantics
  clang_ext_Float_getSemantics(CXFloat c)
  {
    if (auto f = static_cast<llvm::APFloat *>(c.data)) {
      const llvm::fltSemantics *semantics = &f->getSemantics();
      #ifdef LLVM_VERSION_BEFORE_4_0_0
      #define FLTSEMANTICS_ARGS
      #else
      #define FLTSEMANTICS_ARGS ()
      #endif
      if (semantics == &llvm::APFloat::IEEEhalf FLTSEMANTICS_ARGS) {
        return CLANG_EXT_fltSemantics_IEEEhalf;
      }
      if (semantics == &llvm::APFloat::IEEEsingle FLTSEMANTICS_ARGS) {
        return CLANG_EXT_fltSemantics_IEEEsingle;
      }
      if (semantics == &llvm::APFloat::IEEEdouble FLTSEMANTICS_ARGS) {
        return CLANG_EXT_fltSemantics_IEEEdouble;
      }
      if (semantics == &llvm::APFloat::IEEEquad FLTSEMANTICS_ARGS) {
        return CLANG_EXT_fltSemantics_IEEEquad;
      }
      if (semantics == &llvm::APFloat::PPCDoubleDouble FLTSEMANTICS_ARGS) {
        return CLANG_EXT_fltSemantics_PPCDoubleDouble;
      }
      if (semantics == &llvm::APFloat::x87DoubleExtended FLTSEMANTICS_ARGS) {
        return CLANG_EXT_fltSemantics_x87DoubleExtended;
      }
      if (semantics == &llvm::APFloat::Bogus FLTSEMANTICS_ARGS) {
        return CLANG_EXT_fltSemantics_Bogus;
      }
    }
    return CLANG_EXT_fltSemantics_Invalid;
  }

  float
  clang_ext_Float_convertToFloat(CXFloat c)
  {
    if (auto f = static_cast<llvm::APFloat *>(c.data)) {
      return f->convertToFloat();
    }
    return 0.;
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
  clang_ext_StringLiteral_GetString(CXCursor cursor)
  {
    const clang::Stmt *s = GetCursorStmt(cursor);
    if (auto m = llvm::dyn_cast_or_null<clang::StringLiteral>(s)) {
      return cxstring_createDup(m->getString());
    }
    return cxstring_createRef("");
  }

  CXString
  clang_ext_StringLiteral_getBytes(CXCursor cursor)
  {
    const clang::Stmt *s = GetCursorStmt(cursor);
    if (auto m = llvm::dyn_cast_or_null<clang::StringLiteral>(s)) {
      return cxstring_createDup(m->getBytes());
    }
    return cxstring_createRef("");
  }

  unsigned int
  clang_ext_StringLiteral_getByteLength(CXCursor cursor)
  {
    const clang::Stmt *s = GetCursorStmt(cursor);
    if (auto m = llvm::dyn_cast_or_null<clang::StringLiteral>(s)) {
      return m->getByteLength();
    }
    return 0;
  }

  unsigned int
  clang_ext_StringLiteral_getCharByteWidth(CXCursor cursor)
  {
    const clang::Stmt *s = GetCursorStmt(cursor);
    if (auto m = llvm::dyn_cast_or_null<clang::StringLiteral>(s)) {
      return m->getCharByteWidth();
    }
    return 0;
  }

  enum clang_ext_StringKind
  clang_ext_StringLiteral_getKind(CXCursor cursor)
  {
    const clang::Stmt *s = GetCursorStmt(cursor);
    if (auto m = llvm::dyn_cast_or_null<clang::StringLiteral>(s)) {
      switch (m->getKind()) {
      case clang::StringLiteral::Ascii:
        return clang_ext_StringKind_Ascii;
      case clang::StringLiteral::Wide:
        return clang_ext_StringKind_Wide;
      case clang::StringLiteral::UTF8:
        return clang_ext_StringKind_UTF8;
      case clang::StringLiteral::UTF16:
        return clang_ext_StringKind_UTF16;
      case clang::StringLiteral::UTF32:
        return clang_ext_StringKind_UTF32;
      }
    }
    return clang_ext_StringKind_InvalidStringKind;
  }

  enum clang_ext_UnaryOperatorKind
  clang_ext_UnaryOperator_getOpcode(CXCursor c)
  {
    if (auto Op =
        llvm::dyn_cast_or_null<clang::UnaryOperator>(GetCursorStmt(c))) {
      return static_cast<clang_ext_UnaryOperatorKind>(Op->getOpcode());
    }
    return CLANG_EXT_UNARY_OPERATOR_InvalidUnaryOperator;
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
    default:
      return cxstring_createRef("");
    }
  }

  enum clang_ext_BinaryOperatorKind
  clang_ext_BinaryOperator_getOpcode(CXCursor c)
  {
    if (auto Op =
        llvm::dyn_cast_or_null<clang::BinaryOperator>(GetCursorStmt(c))) {
      return static_cast<clang_ext_BinaryOperatorKind>(Op->getOpcode());
    }
    return CLANG_EXT_BINARY_OPERATOR_InvalidBinaryOperator;
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
    default:
      return cxstring_createRef("");
    }
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
  clang_ext_VarDecl_isConstexpr(CXCursor c)
  {
    if (auto D = llvm::dyn_cast_or_null<clang::VarDecl>(GetCursorDecl(c))) {
      return D->isConstexpr();
    }
    return false;
  }

  bool
  clang_ext_MemberRefExpr_isArrow(CXCursor c)
  {
    auto *d = GetCursorStmt(c);
    switch (d->getStmtClass()) {
    case clang::Stmt::MemberExprClass:
      if (auto m = llvm::dyn_cast_or_null<clang::MemberExpr>(d)) {
        return m->isArrow();
      }
      return false;
    case clang::Stmt::UnresolvedMemberExprClass:
      if (auto m = llvm::dyn_cast_or_null<clang::UnresolvedMemberExpr>(d)) {
        return m->isArrow();
      }
      return false;
    case clang::Stmt::CXXPseudoDestructorExprClass:
      if (auto m = llvm::dyn_cast_or_null<clang::CXXPseudoDestructorExpr>(d)) {
        return m->isArrow();
      }
      return false;
    case clang::Stmt::CXXDependentScopeMemberExprClass:
      if (auto m = llvm::dyn_cast_or_null<clang::CXXDependentScopeMemberExpr>(d)) {
        return m->isArrow();
      }
      return false;
    default:;
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
  clang_ext_Decl_GetKind(CXCursor cursor)
  {
    if (!is_valid_decl(cursor.kind)) {
      return CLANG_EXT_DECL_InvalidDecl;
    }
    auto *d = GetCursorDecl(cursor);
    switch (d->getKind()) {
    #define DECL(ClassName, _Base) \
      case clang::Decl::ClassName: return CLANG_EXT_DECL_##ClassName;
    #define ABSTRACT_DECL(_Decl)
    #include <clang/AST/DeclNodes.inc>
    default:
      return CLANG_EXT_DECL_UnknownDecl;
    }
  }

  enum clang_ext_StmtKind
  clang_ext_Stmt_GetKind(CXCursor cursor)
  {
    if (!is_valid_stmt(cursor.kind)) {
      return CLANG_EXT_STMT_InvalidStmt;
    }
    auto *d = GetCursorStmt(cursor);
    switch (d->getStmtClass()) {
    #define STMT(ClassName, _Base) \
      case clang::Stmt::ClassName##Class: return CLANG_EXT_STMT_##ClassName;
    #define ABSTRACT_STMT(_Stmt)
    #include <clang/AST/StmtNodes.inc>
    default:
      return CLANG_EXT_STMT_UnknownStmt;
    }
  }

  enum clang_ext_TypeKind
  clang_ext_GetTypeKind(CXType c)
  {
    clang::QualType T = GetQualType(c);
    if (auto TP = T.getTypePtrOrNull()) {
      switch (TP->getTypeClass()) {
      #define TYPE(Class, Base) \
        case clang::Type::Class: return CLANG_EXT_TYPE_##Class;
      #define ABSTRACT_TYPE(Class, Base)
      #include <clang/AST/TypeNodes.def>
      default:
        return CLANG_EXT_TYPE_UnknownType;
      }
    }
    return CLANG_EXT_TYPE_InvalidType;
  }

  enum clang_ext_TypeKind
  clang_ext_Type_GetKind(CXType c)
  {
    return clang_ext_GetTypeKind(c);
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
//
  enum clang_ext_StringKind
  clang_ext_CharacterLiteral_GetCharacterKind(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CharacterLiteral>(GetCursorStmt(c))) {
      switch (e->getKind()) {
      case clang::CharacterLiteral::Ascii:
        return clang_ext_StringKind_Ascii;
      case clang::CharacterLiteral::Wide:
        return clang_ext_StringKind_Wide;
      case clang::CharacterLiteral::UTF8:
        return clang_ext_StringKind_UTF8;
      case clang::CharacterLiteral::UTF16:
        return clang_ext_StringKind_UTF16;
      case clang::CharacterLiteral::UTF32:
        return clang_ext_StringKind_UTF32;
      default:;
      }
    }
    return clang_ext_StringKind_InvalidStringKind;
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
    if (auto e = llvm::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(
        GetCursorStmt(c))) {
      return static_cast<enum clang_ext_UnaryExpr>(e->getKind());
    }
    return UETT_SizeOf;
  }

  bool
  clang_ext_UnaryExpr_isArgumentType(CXCursor c)
  {
    if (auto e = llvm::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(
        GetCursorStmt(c))) {
      return e->isArgumentType();
    }
    return false;
  }

  CXType
  clang_ext_UnaryExpr_GetArgumentType(CXCursor c)
  {
    if (auto e = llvm::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(
        GetCursorStmt(c))) {
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

  bool
  clang_ext_FunctionDecl_isConstexpr(CXCursor c)
  {
    if (auto *Function = getFunctionDecl(c)) {
      return Function->isConstexpr();
    }
    return false;
  }

  unsigned
  clang_ext_LinkageSpecDecl_getLanguageIDs(CXCursor C)
  {
    auto *D = GetCursorDecl(C);
    if (auto *LS = llvm::dyn_cast_or_null<clang::LinkageSpecDecl>(D)) {
      return LS->getLanguage();
    }
    return 0;
  }

  CXType
  clang_ext_TemplateTypeParmDecl_getDefaultArgument(CXCursor C)
  {
    auto *D = GetCursorDecl(C);
    if (auto TTPD = llvm::dyn_cast_or_null<clang::TemplateTypeParmDecl>(D)) {
      if (auto *type = TTPD->getDefaultArgumentInfo()) {
        return MakeCXType(type->getType(), getCursorTU(C));
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

  CXCursor
  clang_ext_TemplateArgument_getAsExpr(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeCXCursor(TA->getAsExpr(), CTA.TU);
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, CTA.TU);
  }

  unsigned int
  clang_ext_TemplateArgument_getPackSize(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return TA->pack_size();
    }
    return 0;
  }

  struct clang_ext_TemplateArgument
  clang_ext_TemplateArgument_getPackArgument(
    struct clang_ext_TemplateArgument CTA, unsigned int i)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeTemplateArgument(TA->getPackAsArray()[i], CTA.TU);
    }
    return MakeTemplateArgumentInvalid(CTA.TU);
  }

  struct clang_ext_TemplateArgument
  clang_ext_TemplateArgument_getPackExpansionPattern(
    struct clang_ext_TemplateArgument CTA)
  {
    if (auto *TA = GetTemplateArgument(CTA)) {
      return MakeTemplateArgument(TA->getPackExpansionPattern(), CTA.TU);
    }
    return MakeTemplateArgumentInvalid(CTA.TU);
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

  CXCursor
  clang_ext_FriendDecl_getFriendDecl(CXCursor c)
  {
    if (auto *d = GetCursorDecl(c)) {
      if (auto *fd = llvm::dyn_cast_or_null<clang::FriendDecl>(d)) {
        return MakeCXCursor(fd->getFriendDecl(), getCursorTU(c));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  CXType
  clang_ext_FriendDecl_getFriendType(CXCursor c)
  {
    if (auto *d = GetCursorDecl(c)) {
      if (auto *fd = llvm::dyn_cast_or_null<clang::FriendDecl>(d)) {
        if (auto *type = fd->getFriendType()) {
          return MakeCXType(type->getType(), getCursorTU(c));
        }
      }
    }
    return MakeCXTypeInvalid(getCursorTU(c));
  }

  CXCursor
  clang_ext_FieldDecl_getInClassInitializer(CXCursor c)
  {
    if (auto *d = GetCursorDecl(c)) {
      if (auto *fd = llvm::dyn_cast_or_null<clang::FieldDecl>(d)) {
        if (clang::Expr *e = fd->getInClassInitializer()) {
          return MakeCXCursor(e, getCursorTU(c));
        }
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  CXType
  clang_ext_GenericSelectionExpr_getAssocType(CXCursor c, unsigned i)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::GenericSelectionExpr>(GetCursorStmt(c))) {
      #ifdef LLVM_VERSION_BEFORE_9_0_0
      auto ty = e->getAssocType(i);
      if (ty.getTypePtrOrNull()) {
        return MakeCXType(ty, getCursorTU(c));
      }
      #else
      if (auto type_infos = e->getAssocTypeSourceInfos()[i]) {
        auto ty = type_infos->getType();
        if (ty.getTypePtrOrNull())
          return MakeCXType(ty, getCursorTU(c)); {
        }
      }
      #endif
    }
    return MakeCXTypeInvalid(getCursorTU(c));
  }

  bool
  clang_ext_TemplateParm_isParameterPack(CXCursor c)
  {
    if (auto *d = GetCursorDecl(c)) {
      switch (d->getKind()) {
      case clang::Decl::TemplateTypeParm:
        if (auto *ttpd =
            llvm::dyn_cast_or_null<clang::TemplateTypeParmDecl>(d)) {
          return ttpd->isParameterPack();
        }
        break;
      case clang::Decl::NonTypeTemplateParm:
        if (auto *nttpd =
            llvm::dyn_cast_or_null<clang::NonTypeTemplateParmDecl>(d)) {
          return nttpd->isParameterPack();
        }
        break;
      default:;
      }
    }
    return false;
  }

  CXCursor
  clang_ext_ClassTemplateDecl_getTemplatedDecl(CXCursor c)
  {
    if (auto *d = GetCursorDecl(c)) {
      if (auto *ctd = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(d)) {
        return MakeCXCursor(ctd->getTemplatedDecl(), getCursorTU(c));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  enum clang_ext_PredefinedExpr_IdentKind
  clang_ext_PredefinedExpr_getIdentKind(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::PredefinedExpr>(GetCursorStmt(c))) {
#ifdef LLVM_VERSION_BEFORE_8_0_0
      switch (e->getIdentType()) {
#else
      switch (e->getIdentKind()) {
#endif
      case clang::PredefinedExpr::Func:
        return clang_ext_PredefinedExpr_Func;
      case clang::PredefinedExpr::Function:
        return clang_ext_PredefinedExpr_Function;
      case clang::PredefinedExpr::LFunction:
        return clang_ext_PredefinedExpr_LFunction;
      case clang::PredefinedExpr::FuncDName:
        return clang_ext_PredefinedExpr_FuncDName;
#ifndef LLVM_VERSION_BEFORE_3_5_0
      case clang::PredefinedExpr::FuncSig:
        return clang_ext_PredefinedExpr_FuncSig;
#endif
#ifndef LLVM_VERSION_BEFORE_7_0_0
      case clang::PredefinedExpr::LFuncSig:
        return clang_ext_PredefinedExpr_LFuncSig;
#endif
      case clang::PredefinedExpr::PrettyFunction:
        return clang_ext_PredefinedExpr_PrettyFunction;
      case clang::PredefinedExpr::PrettyFunctionNoVirtual:
        return clang_ext_PredefinedExpr_PrettyFunctionNoVirtual;
      }
    }
    return clang_ext_PredefinedExpr_InvalidPredefinedExpr;
  }

#ifndef LLVM_VERSION_BEFORE_3_6_0
  CXString
  clang_ext_PredefinedExpr_getFunctionName(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::PredefinedExpr>(GetCursorStmt(c))) {
      if (auto s = e->getFunctionName()) {
        return cxstring_createDup(s->getString());
      }
    }
    return cxstring_createRef("");
  }
#endif

  CXString
  clang_ext_PredefinedExpr_ComputeName(
    enum clang_ext_PredefinedExpr_IdentKind kind,
    CXCursor decl)
  {
    #ifdef LLVM_VERSION_BEFORE_8_0_0
      clang::PredefinedExpr::IdentType clang_kind;
    #else
      clang::PredefinedExpr::IdentKind clang_kind;
    #endif
    switch (kind) {
    case clang_ext_PredefinedExpr_Func:
      clang_kind = clang::PredefinedExpr::Func;
      break;
    case clang_ext_PredefinedExpr_Function:
      clang_kind = clang::PredefinedExpr::Function;
      break;
    case clang_ext_PredefinedExpr_LFunction:
      clang_kind = clang::PredefinedExpr::LFunction;
      break;
    case clang_ext_PredefinedExpr_FuncDName:
      clang_kind = clang::PredefinedExpr::FuncDName;
      break;
#ifndef LLVM_VERSION_BEFORE_3_5_0
    case clang_ext_PredefinedExpr_FuncSig:
      clang_kind = clang::PredefinedExpr::FuncSig;
      break;
#endif
#ifndef LLVM_VERSION_BEFORE_7_0_0
    case clang_ext_PredefinedExpr_LFuncSig:
      clang_kind = clang::PredefinedExpr::LFuncSig;
      break;
#endif
    case clang_ext_PredefinedExpr_PrettyFunction:
      clang_kind = clang::PredefinedExpr::PrettyFunction;
      break;
    case clang_ext_PredefinedExpr_PrettyFunctionNoVirtual:
      clang_kind = clang::PredefinedExpr::PrettyFunctionNoVirtual;
      break;
    default:
      return cxstring_createRef("");
    }
    if (auto *d = GetCursorDecl(decl)) {
      auto name = clang::PredefinedExpr::ComputeName(clang_kind, d);
      return cxstring_createRef(name.c_str());
    }
    return cxstring_createRef("");
  }

  enum clang_ext_LambdaCaptureDefault
  clang_ext_LambdaExpr_getCaptureDefault(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::LambdaExpr>(GetCursorStmt(c))) {
      return static_cast<enum clang_ext_LambdaCaptureDefault>(
        e->getCaptureDefault());
    }
    return clang_ext_LCD_CaptureNone;
  }

  unsigned
  clang_ext_LambdaExpr_getCaptureCount(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::LambdaExpr>(GetCursorStmt(c))) {
      return e->capture_end() - e->capture_begin();
    }
    return 0;
  }

  struct clang_ext_LambdaCapture
  clang_ext_LambdaExpr_getCapture(CXCursor c, unsigned index)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::LambdaExpr>(GetCursorStmt(c))) {
      if (index < e->capture_end() - e->capture_begin()) {
        return MakeLambdaCapture(
          e->capture_begin()[index], getCursorTU(c));
      }
    }
    return MakeLambdaCaptureInvalid(getCursorTU(c));
  }

  bool
  clang_ext_LambdaExpr_isMutable(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::LambdaExpr>(GetCursorStmt(c))) {
      return e->isMutable();
    }
    return false;
  }

  bool
  clang_ext_LambdaExpr_hasExplicitParameters(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::LambdaExpr>(GetCursorStmt(c))) {
      return e->hasExplicitParameters();
    }
    return false;
  }

  bool
  clang_ext_LambdaExpr_hasExplicitResultType(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::LambdaExpr>(GetCursorStmt(c))) {
      return e->hasExplicitResultType();
    }
    return false;
  }

  CXCursor
  clang_ext_LambdaExpr_getCallOperator(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::LambdaExpr>(GetCursorStmt(c))) {
      if (auto *m = e->getCallOperator()) {
        return MakeCXCursor(m, getCursorTU(c));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  enum clang_ext_LambdaCaptureKind
  clang_ext_LambdaCapture_getKind(struct clang_ext_LambdaCapture capture)
  {
    if (auto *LC = GetLambdaCapture(capture)) {
      return static_cast<enum clang_ext_LambdaCaptureKind>(
        LC->getCaptureKind());
    }
    return clang_ext_LCK_This;
  }

  CXCursor
  clang_ext_LambdaCapture_getCapturedVar(struct clang_ext_LambdaCapture capture)
  {
    if (auto *LC = GetLambdaCapture(capture)) {
      if (auto *VarDecl = LC->getCapturedVar()) {
        return MakeCXCursor(VarDecl, capture.TU);
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, capture.TU);
  }

  bool
  clang_ext_LambdaCapture_isImplicit(struct clang_ext_LambdaCapture capture)
  {
    if (auto *LC = GetLambdaCapture(capture)) {
      return LC->isImplicit();
    }
    return false;
  }

  bool
  clang_ext_LambdaCapture_isPackExpansion(
    struct clang_ext_LambdaCapture capture)
  {
    if (auto *LC = GetLambdaCapture(capture)) {
      return LC->isPackExpansion();
    }
    return false;
  }

  void
  clang_ext_LambdaCapture_dispose(struct clang_ext_LambdaCapture capture)
  {
    if (auto *LC = GetLambdaCapture(capture)) {
      delete LC;
    }
  }

  CXType
  clang_ext_CXXNewExpr_getAllocatedType(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXNewExpr>(GetCursorStmt(c))) {
        return MakeCXType(e->getAllocatedType(), getCursorTU(c));
    }
    return MakeCXTypeInvalid(getCursorTU(c));
  }

  CXCursor
  clang_ext_CXXNewExpr_getArraySize(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXNewExpr>(GetCursorStmt(c))) {
      if (auto sz = e->getArraySize()) {
        #ifdef LLVM_VERSION_BEFORE_9_0_0
        return MakeCXCursor(sz, getCursorTU(c));
        #else
        return MakeCXCursor(*sz, getCursorTU(c));
        #endif
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  unsigned int
  clang_ext_CXXNewExpr_getNumPlacementArgs(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXNewExpr>(GetCursorStmt(c))) {
      return e->getNumPlacementArgs();
    }
    return 0;
  }

  CXCursor
  clang_ext_CXXNewExpr_getPlacementArg(CXCursor c, unsigned int i)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXNewExpr>(GetCursorStmt(c))) {
      if (auto *s = e->getPlacementArg(i)) {
        return MakeCXCursor(s, getCursorTU(c));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  CXCursor
  clang_ext_CXXNewExpr_getInitializer(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXNewExpr>(GetCursorStmt(c))) {
      if (auto *s = e->getInitializer()) {
        return MakeCXCursor(s, getCursorTU(c));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  bool
  clang_ext_CXXDeleteExpr_isGlobalDelete(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXDeleteExpr>(GetCursorStmt(c))) {
      return e->isGlobalDelete();
    }
    return false;
  }

  bool
  clang_ext_CXXDeleteExpr_isArrayForm(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXDeleteExpr>(GetCursorStmt(c))) {
      return e->isArrayForm();
    }
    return false;
  }

  bool
  clang_ext_CXXTypeidExpr_isTypeOperand(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXTypeidExpr>(GetCursorStmt(c))) {
      return e->isTypeOperand();
    }
    return false;
  }

  CXType
  clang_ext_CXXTypeidExpr_getTypeOperand(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXTypeidExpr>(GetCursorStmt(c))) {
      return MakeCXType(e->getTypeOperand(getCursorContext(c)), getCursorTU(c));
    }
    return MakeCXTypeInvalid(getCursorTU(c));
  }

  CXCursor
  clang_ext_CXXTypeidExpr_getExprOperand(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXTypeidExpr>(GetCursorStmt(c))) {
      return MakeCXCursor(e->getExprOperand(), getCursorTU(c));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  const char *
  clang_ext_LangStandard_getName(enum clang_ext_langstandards s)
  {
    switch (s) {
    #define FOREACH_STANDARD(Ident, Name) \
      case CLANG_EXT_LANGSTANDARDS_##Ident: return Name;
    #ifdef LLVM_VERSION_BEFORE_5_0_0
    #define LANGSTANDARD(Ident, Name, _Desc, _Features) \
      FOREACH_STANDARD(Ident, Name)
    #else
    #define LANGSTANDARD(Ident, Name, _Lang, _Desc, _Features) \
      FOREACH_STANDARD(Ident, Name)
    #endif
    #include <clang/Frontend/LangStandards.def>
    #undef FOREACH_STANDARD
    default:
      return "";
    }
  }

  enum clang_ext_langstandards
  clang_ext_LangStandard_ofName(const char *s)
  {
    #define FOREACH_STANDARD(Ident, Name) \
      if (strcmp(Name, s) == 0) \
        return CLANG_EXT_LANGSTANDARDS_##Ident;
    #ifdef LLVM_VERSION_BEFORE_5_0_0
    #define LANGSTANDARD(Ident, Name, _Desc, _Features) \
      FOREACH_STANDARD(Ident, Name)
    #else
    #define LANGSTANDARD(Ident, Name, _Lang, _Desc, _Features) \
      FOREACH_STANDARD(Ident, Name)
    #endif
    #include <clang/Frontend/LangStandards.def>
    #undef FOREACH_STANDARD
    return CLANG_EXT_LANGSTANDARDS_InvalidLang;
  }

  CXType
  clang_ext_PackExpansion_getPattern(CXType c)
  {
    if (auto PET = GetQualType(c)->getAs<clang::PackExpansionType>()) {
      return MakeCXType(PET->getPattern(), GetTU(c));
    }
    return MakeCXTypeInvalid(GetTU(c));
  }

  bool
  clang_ext_CXXFoldExpr_isRightFold(CXCursor c)
  {
    #ifndef LLVM_VERSION_BEFORE_3_6_0
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXFoldExpr>(GetCursorStmt(c))) {
      return e->isRightFold();
    }
    #endif
    return false;
  }

  enum clang_ext_BinaryOperatorKind
  clang_ext_CXXFoldExpr_getOperator(CXCursor c)
  {
    #ifndef LLVM_VERSION_BEFORE_3_6_0
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXFoldExpr>(GetCursorStmt(c))) {
      return static_cast<clang_ext_BinaryOperatorKind>(e->getOperator());
    }
    #endif
    return CLANG_EXT_BINARY_OPERATOR_InvalidBinaryOperator;
  }

  bool
  clang_ext_CXXBoolLiteralExpr_getValue(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXBoolLiteralExpr>(GetCursorStmt(c))) {
      return e->getValue();
    }
    return false;
  }

  CXCursor
  clang_ext_CallExpr_getCallee(CXCursor c)
  {
    if (auto e = llvm::dyn_cast_or_null<clang::CallExpr>(GetCursorStmt(c))) {
      return MakeCXCursor(e->getCallee(), getCursorTU(c));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  unsigned int
  clang_ext_CallExpr_getNumArgs(CXCursor c)
  {
    if (auto e = llvm::dyn_cast_or_null<clang::CallExpr>(GetCursorStmt(c))) {
      return e->getNumArgs();
    }
    return 0;
  }

  CXCursor
  clang_ext_CallExpr_getArg(CXCursor c, unsigned int i)
  {
    if (auto e = llvm::dyn_cast_or_null<clang::CallExpr>(GetCursorStmt(c))) {
      return MakeCXCursor(e->getArg(i), getCursorTU(c));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  CXCursor
  clang_ext_SizeOfPackExpr_getPack(CXCursor c)
  {
    if (auto e =
        llvm::dyn_cast_or_null<clang::SizeOfPackExpr>(GetCursorStmt(c))) {
      return MakeCXCursor(e->getPack(), getCursorTU(c));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  CXCursor
  clang_ext_DecltypeType_getUnderlyingExpr(CXType t)
  {
    if (auto *dt = GetQualType(t)->getAs<clang::DecltypeType>()) {
      return MakeCXCursor(dt->getUnderlyingExpr(), GetTU(t));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, GetTU(t));
  }

  bool
  clang_ext_NamespaceDecl_isInline(CXCursor c)
  {
    if (auto nd =
        llvm::dyn_cast_or_null<clang::NamespaceDecl>(GetCursorDecl(c))) {
      return nd->isInline();
    }
    return false;
  }

  void
  clang_ext_DeclarationName_dispose(struct clang_ext_DeclarationName name)
  {
    if (auto *d = GetDeclarationName(name)) {
      delete d;
    }
  }

  enum clang_ext_DeclarationNameKind
  clang_ext_DeclarationName_getKind(
    struct clang_ext_DeclarationName name)
  {
    if (auto d = GetDeclarationName(name)) {
      switch (d->getNameKind()) {
      case clang::DeclarationName::NameKind::Identifier:
        return CLANG_EXT_DECLARATION_NAME_Identifier;
      case clang::DeclarationName::NameKind::ObjCZeroArgSelector:
        return CLANG_EXT_DECLARATION_NAME_ObjCZeroArgSelector;
      case clang::DeclarationName::NameKind::ObjCOneArgSelector:
        return CLANG_EXT_DECLARATION_NAME_ObjCOneArgSelector;
      case clang::DeclarationName::NameKind::ObjCMultiArgSelector:
        return CLANG_EXT_DECLARATION_NAME_ObjCMultiArgSelector;
      case clang::DeclarationName::NameKind::CXXConstructorName:
        return CLANG_EXT_DECLARATION_NAME_CXXConstructorName;
      case clang::DeclarationName::NameKind::CXXDestructorName:
        return CLANG_EXT_DECLARATION_NAME_CXXDestructorName;
      case clang::DeclarationName::NameKind::CXXConversionFunctionName:
        return CLANG_EXT_DECLARATION_NAME_CXXConversionFunctionName;
      #ifndef LLVM_VERSION_BEFORE_5_0_0
      case clang::DeclarationName::NameKind::CXXDeductionGuideName:
        return CLANG_EXT_DECLARATION_NAME_CXXDeductionGuideName;
      #endif
      case clang::DeclarationName::NameKind::CXXOperatorName:
        return CLANG_EXT_DECLARATION_NAME_CXXOperatorName;
      case clang::DeclarationName::NameKind::CXXLiteralOperatorName:
        return CLANG_EXT_DECLARATION_NAME_CXXLiteralOperatorName;
      case clang::DeclarationName::NameKind::CXXUsingDirective:
        return CLANG_EXT_DECLARATION_NAME_CXXUsingDirective;
      }
    }
    return CLANG_EXT_DECLARATION_NAME_InvalidDeclarationName;
  }

  const char *
  clang_ext_OverloadedOperator_getSpelling(
    enum clang_ext_OverloadedOperatorKind kind)
  {
    switch (kind) {
    #define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
      case CLANG_EXT_OVERLOADED_OPERATOR_##Name: return Spelling;
    #include <clang/Basic/OperatorKinds.def>
    default:;
    }
    return "";
  }

  enum clang_ext_OverloadedOperatorKind
  clang_ext_DeclarationName_getCXXOverloadedOperator(
    struct clang_ext_DeclarationName name)
  {
    if (auto d = GetDeclarationName(name)) {
      switch (d->getCXXOverloadedOperator()) {
      case clang::OO_None:
        return CLANG_EXT_OVERLOADED_OPERATOR_InvalidOverloadedOperator;
      #define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
        case clang::OO_##Name: return CLANG_EXT_OVERLOADED_OPERATOR_##Name;
      #include <clang/Basic/OperatorKinds.def>
      default:;
      }
    }
    return CLANG_EXT_OVERLOADED_OPERATOR_InvalidOverloadedOperator;
  }

  CXType
  clang_ext_DeclarationName_getCXXNameType(
    struct clang_ext_DeclarationName name)
  {
    if (auto d = GetDeclarationName(name)) {
      return MakeCXType(d->getCXXNameType(), name.tu);
    }
    return MakeCXTypeInvalid(name.tu);
  }

  CXString
  clang_ext_DeclarationName_getAsIdentifier(
    struct clang_ext_DeclarationName name)
  {
    if (auto d = GetDeclarationName(name)) {
      if (auto i = d->getAsIdentifierInfo()) {
        return cxstring_createDup(i->getName());
      }
    }
    return cxstring_createRef("");
  }

  CXCursor
  clang_ext_DeclarationName_getCXXDeductionGuideTemplate(
    struct clang_ext_DeclarationName name)
  {
    #ifndef LLVM_VERSION_BEFORE_5_0_0
    if (auto d = GetDeclarationName(name)) {
      if (auto dg = d->getCXXDeductionGuideTemplate()) {
        return MakeCXCursor(dg, name.tu);
      }
    }
    #endif
    return MakeCXCursorInvalid(CXCursor_InvalidCode, name.tu);
  }

  CXString
  clang_ext_DeclarationName_getCXXLiteralIdentifier(
    struct clang_ext_DeclarationName name)
  {
    if (auto d = GetDeclarationName(name)) {
      if (auto i = d->getCXXLiteralIdentifier()) {
        return cxstring_createDup(i->getName());
      }
    }
    return cxstring_createRef("");
  }

  struct clang_ext_DeclarationName
  clang_ext_Decl_getName(CXCursor cursor)
  {
    switch (cursor.kind) {
    case CXCursor_DeclRefExpr:
    case CXCursor_MemberRefExpr:
      if (auto s = GetCursorStmt(cursor)) {
        switch (s->getStmtClass()) {
        case clang::Stmt::DeclRefExprClass:
          if (auto d = llvm::dyn_cast_or_null<clang::DeclRefExpr>(s)) {
            return MakeDeclarationName(
              d->getNameInfo().getName(), getCursorTU(cursor));
          }
          return MakeDeclarationNameInvalid(getCursorTU(cursor));
        case clang::Stmt::CXXDependentScopeMemberExprClass:
          if (auto d =
              llvm::dyn_cast_or_null<clang::CXXDependentScopeMemberExpr>(s)) {
            return MakeDeclarationName(
              d->getMember(), getCursorTU(cursor));
          }
          return MakeDeclarationNameInvalid(getCursorTU(cursor));
        case clang::Stmt::DependentScopeDeclRefExprClass:
          if (auto d =
              llvm::dyn_cast_or_null<clang::DependentScopeDeclRefExpr>(s)) {
            return MakeDeclarationName(
              d->getDeclName(), getCursorTU(cursor));
          }
          return MakeDeclarationNameInvalid(getCursorTU(cursor));
        default:
          if (auto d = llvm::dyn_cast_or_null<clang::OverloadExpr>(s)) {
            return MakeDeclarationName(
              d->getNameInfo().getName(), getCursorTU(cursor));
          }
          return MakeDeclarationNameInvalid(getCursorTU(cursor));
        }
      }
      return MakeDeclarationNameInvalid(getCursorTU(cursor));
    default:
      if (is_valid_decl(cursor.kind)) {
        auto d = GetCursorDecl(cursor);
        if (auto nd = llvm::dyn_cast_or_null<clang::NamedDecl>(d)) {
          return MakeDeclarationName(nd->getDeclName(), getCursorTU(cursor));
        }
      }
    }
    return MakeDeclarationNameInvalid(getCursorTU(cursor));
  }

  CXCursor
  clang_ext_UsingDirectiveDecl_getNominatedNamespace(CXCursor c)
  {
    if (auto d = GetCursorDecl(c)) {
      if (auto ud = llvm::dyn_cast_or_null<clang::UsingDirectiveDecl>(d)) {
        return MakeCXCursor(ud->getNominatedNamespace(), getCursorTU(c));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  enum clang_ext_NestedNameSpecifierKind
  clang_ext_NestedNameSpecifier_getKind(
    struct clang_ext_NestedNameSpecifier specifier)
  {
    if (auto s = GetNestedNameSpecifier(specifier)) {
      switch (s->getKind()) {
      case clang::NestedNameSpecifier::SpecifierKind::Identifier:
        return CLANG_EXT_NESTED_NAME_SPECIFIER_Identifier;
      case clang::NestedNameSpecifier::SpecifierKind::Namespace:
        return CLANG_EXT_NESTED_NAME_SPECIFIER_Namespace;
      case clang::NestedNameSpecifier::SpecifierKind::NamespaceAlias:
        return CLANG_EXT_NESTED_NAME_SPECIFIER_NamespaceAlias;
      case clang::NestedNameSpecifier::SpecifierKind::TypeSpec:
        return CLANG_EXT_NESTED_NAME_SPECIFIER_TypeSpec;
      case clang::NestedNameSpecifier::SpecifierKind::TypeSpecWithTemplate:
        return CLANG_EXT_NESTED_NAME_SPECIFIER_TypeSpecWithTemplate;
      case clang::NestedNameSpecifier::SpecifierKind::Global:
        return CLANG_EXT_NESTED_NAME_SPECIFIER_Global;
      #ifndef LLVM_VERSION_BEFORE_3_6_0
      case clang::NestedNameSpecifier::SpecifierKind::Super:
        return CLANG_EXT_NESTED_NAME_SPECIFIER_Super;
      #endif
      default:;
      }
    }
    return CLANG_EXT_NESTED_NAME_SPECIFIER_InvalidNestedNameSpecifier;
  }

  struct clang_ext_NestedNameSpecifier
  clang_ext_NestedNameSpecifier_getPrefix(
    struct clang_ext_NestedNameSpecifier specifier)
  {
    if (auto s = GetNestedNameSpecifier(specifier)) {
      return MakeNestedNameSpecifier(s->getPrefix(), specifier.tu);
    }
    return MakeNestedNameSpecifierInvalid(specifier.tu);
  }

  CXString
  clang_ext_NestedNameSpecifier_getAsIdentifier(
    struct clang_ext_NestedNameSpecifier specifier)
  {
    if (auto s = GetNestedNameSpecifier(specifier)) {
      if (auto i = s->getAsIdentifier()) {
        return cxstring_createDup(i->getName());
    }
    }
    return cxstring_createRef("");
  }

  CXCursor
  clang_ext_NestedNameSpecifier_getAsNamespace(
    struct clang_ext_NestedNameSpecifier specifier)
  {
    if (auto s = GetNestedNameSpecifier(specifier)) {
      if (auto n = s->getAsNamespace()) {
        return MakeCXCursor(n, specifier.tu);
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, specifier.tu);
  }

  CXType
  clang_ext_NestedNameSpecifier_getAsType(
    struct clang_ext_NestedNameSpecifier specifier)
  {
    if (auto s = GetNestedNameSpecifier(specifier)) {
      if (auto t = s->getAsType()) {
        return MakeCXType(clang::QualType(t, 0), specifier.tu);
      }
    }
    return MakeCXTypeInvalid(specifier.tu);
  }

  struct clang_ext_NestedNameSpecifier
  clang_ext_Decl_getNestedNameSpecifier(CXCursor cursor)
  {
    switch (cursor.kind) {
    case CXCursor_DeclRefExpr:
      if (auto *d = GetCursorStmt(cursor)) {
        switch (d->getStmtClass()) {
        case clang::Stmt::DeclRefExprClass:
          if (auto dd = llvm::dyn_cast_or_null<clang::DeclRefExpr>(d)) {
            return MakeNestedNameSpecifier(dd->getQualifier(), getCursorTU(cursor));
          }
          return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
        case clang::Stmt::DependentScopeDeclRefExprClass:
          if (auto dd = llvm::dyn_cast_or_null<clang::DependentScopeDeclRefExpr>(d)) {
            return MakeNestedNameSpecifier(dd->getQualifier(), getCursorTU(cursor));
          }
          return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
        default:
          if (auto dd = llvm::dyn_cast_or_null<clang::OverloadExpr>(d)) {
            return MakeNestedNameSpecifier(dd->getQualifier(), getCursorTU(cursor));
          };
        }
      }
      return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
    case CXCursor_MemberRefExpr:
      if (auto *d = GetCursorStmt(cursor)) {
        switch (d->getStmtClass()) {
        case clang::Stmt::CXXPseudoDestructorExprClass:
          if (auto m = llvm::dyn_cast_or_null<clang::CXXPseudoDestructorExpr>(d)) {
            return MakeNestedNameSpecifier(m->getQualifier(), getCursorTU(cursor));
          }
          return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
        case clang::Stmt::CXXDependentScopeMemberExprClass:
          if (auto m = llvm::dyn_cast_or_null<clang::CXXDependentScopeMemberExpr>(d)) {
            return MakeNestedNameSpecifier(m->getQualifier(), getCursorTU(cursor));
          }
          return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
        default:;
        }
      }
      return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
    case CXCursor_UsingDirective:
      if (auto d =
          llvm::dyn_cast_or_null<clang::UsingDirectiveDecl>(
            GetCursorDecl(cursor))) {
        return MakeNestedNameSpecifier(d->getQualifier(), getCursorTU(cursor));
      }
      return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
    case CXCursor_NamespaceAlias:
      if (auto d =
          llvm::dyn_cast_or_null<clang::NamespaceAliasDecl>(
            GetCursorDecl(cursor))) {
        return MakeNestedNameSpecifier(d->getQualifier(), getCursorTU(cursor));
      }
      return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
    case CXCursor_UsingDeclaration:
      if (auto d =
          llvm::dyn_cast_or_null<clang::UsingDecl>(GetCursorDecl(cursor))) {
        return MakeNestedNameSpecifier(d->getQualifier(), getCursorTU(cursor));
      }
      return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
    default:
      if (is_valid_decl(cursor.kind)) {
        auto d = GetCursorDecl(cursor);
        if (auto td = llvm::dyn_cast_or_null<clang::TagDecl>(d)) {
          return MakeNestedNameSpecifier(
            td->getQualifier(), getCursorTU(cursor));
        }
        else if (auto dd = llvm::dyn_cast_or_null<clang::DeclaratorDecl>(d)) {
          return MakeNestedNameSpecifier(
            dd->getQualifier(), getCursorTU(cursor));
        }
      }
      return MakeNestedNameSpecifierInvalid(getCursorTU(cursor));
    }
  }

  struct clang_ext_NestedNameSpecifier
  clang_ext_Type_getQualifier(CXType t)
  {
    if (auto tp = GetQualType(t).getTypePtrOrNull()) {
      switch (tp->getTypeClass()) {
      case clang::Type::Elaborated:
        if (auto et = tp->getAs<clang::ElaboratedType>()) {
          return MakeNestedNameSpecifier(et->getQualifier(), GetTU(t));
        }
        return MakeNestedNameSpecifierInvalid(GetTU(t));
      case clang::Type::DependentName:
        if (auto dt = tp->getAs<clang::DependentNameType>()) {
          return MakeNestedNameSpecifier(dt->getQualifier(), GetTU(t));
        }
        return MakeNestedNameSpecifierInvalid(GetTU(t));
      default:;
      }
    }
    return MakeNestedNameSpecifierInvalid(GetTU(t));
  }

  bool
  clang_ext_TagDecl_isCompleteDefinition(CXCursor cursor)
  {
    if (auto d = GetCursorDecl(cursor)) {
      if (auto td = llvm::dyn_cast_or_null<clang::TagDecl>(d)) {
        return td->isCompleteDefinition();
      }
    }
    return false;
  }

  CXType
  clang_ext_CXXPseudoDestructorExpr_getDestroyedType(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CXXPseudoDestructorExpr>(
          GetCursorStmt(c))) {
        return MakeCXType(e->getDestroyedType(), getCursorTU(c));
    }
    return MakeCXTypeInvalid(getCursorTU(c));
  }

  unsigned int
  clang_ext_Cursor_getNumTemplateArgs(CXCursor cursor)
  {
    if (is_valid_decl(cursor.kind)) {
      auto d = GetCursorDecl(cursor);
      if (auto cts = llvm::dyn_cast_or_null<
          clang::ClassTemplateSpecializationDecl>(d)) {
        return cts->getTemplateArgs().size();
      }
      else if (auto cts = llvm::dyn_cast_or_null<
          clang::VarTemplateSpecializationDecl>(d)) {
        return cts->getTemplateArgs().size();
      }
    }
    else if (is_valid_stmt(cursor.kind)) {
      auto s = GetCursorStmt(cursor);
      if (auto e = llvm::dyn_cast_or_null<
          clang::CXXDependentScopeMemberExpr>(s)) {
        return e->getNumTemplateArgs();
      }
    }
    return 0;
  }

  struct clang_ext_TemplateArgument
  clang_ext_Cursor_getTemplateArg(
    CXCursor cursor, unsigned int i)
  {
    if (is_valid_decl(cursor.kind)) {
      auto d = GetCursorDecl(cursor);
      if (auto cts = llvm::dyn_cast_or_null<
          clang::ClassTemplateSpecializationDecl>(d)) {
        return MakeTemplateArgument(cts->getTemplateArgs().get(i),
          getCursorTU(cursor));
      }
      else if (auto cts = llvm::dyn_cast_or_null<
          clang::VarTemplateSpecializationDecl>(d)) {
        return MakeTemplateArgument(cts->getTemplateArgs().get(i),
          getCursorTU(cursor));
      }
    }
    else if (is_valid_stmt(cursor.kind)) {
      auto s = GetCursorStmt(cursor);
      if (auto e = llvm::dyn_cast_or_null<
          clang::CXXDependentScopeMemberExpr>(s)) {
        return MakeTemplateArgument(e->getTemplateArgs()[i].getArgument(),
          getCursorTU(cursor));
      }
    }
    return MakeTemplateArgumentInvalid(getCursorTU(cursor));
  }

  CXCursor
  clang_ext_TypeAliasTemplateDecl_getTemplatedDecl(CXCursor cursor)
  {
    if (auto d = GetCursorDecl(cursor)) {
      if (auto td = llvm::dyn_cast_or_null<clang::TypeAliasTemplateDecl>(d)) {
        return MakeCXCursor(td->getTemplatedDecl(), getCursorTU(cursor));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(cursor));
  }

  unsigned int
  clang_ext_TemplateDecl_getParameterCount(CXCursor cursor)
  {
    if (auto d = GetCursorDecl(cursor)) {
      if (auto td = llvm::dyn_cast_or_null<clang::TemplateDecl>(d)) {
        return td->getTemplateParameters()->size();
      }
      else if (auto tp =
     llvm::dyn_cast_or_null<clang::ClassTemplatePartialSpecializationDecl>(d)) {
        return tp->getTemplateParameters()->size();
      }
    }
    return 0;
  }

  CXCursor
  clang_ext_TemplateDecl_getParameter(CXCursor cursor, unsigned int i)
  {
    if (auto d = GetCursorDecl(cursor)) {
      if (auto td = llvm::dyn_cast_or_null<clang::TemplateDecl>(d)) {
        return MakeCXCursor(
          td->getTemplateParameters()->getParam(i), getCursorTU(cursor));
      }
      else if (auto tp =
     llvm::dyn_cast_or_null<clang::ClassTemplatePartialSpecializationDecl>(d)) {
        return MakeCXCursor(
          tp->getTemplateParameters()->getParam(i), getCursorTU(cursor));
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(cursor));
  }

  CXCursor
  clang_ext_SubstNonTypeTemplateParmExpr_getReplacement(CXCursor cursor)
  {
    auto s = GetCursorStmt(cursor);
    if (auto e =
        llvm::dyn_cast_or_null<clang::SubstNonTypeTemplateParmExpr>(s)) {
      return MakeCXCursor(e->getReplacement(), getCursorTU(cursor));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(cursor));
  }

  unsigned int
  clang_ext_AttributedStmt_GetAttributeCount(CXCursor cursor)
  {
    auto s = GetCursorStmt(cursor);
    if (auto e = llvm::dyn_cast_or_null<clang::AttributedStmt>(s)) {
      return e->getAttrs().size();
    }
    return 0;
  }

  enum clang_ext_AttrKind
  clang_ext_AttributedStmt_GetAttributeKind(CXCursor cursor, unsigned int i)
  {
    auto s = GetCursorStmt(cursor);
    if (auto e = llvm::dyn_cast_or_null<clang::AttributedStmt>(s)) {
      return static_cast<enum clang_ext_AttrKind>(e->getAttrs()[i]->getKind());
    }
    return CLANG_EXT_ATTR_NoAttr;
  }

  unsigned int
  clang_ext_DecompositionDecl_GetBindingsCount(CXCursor cursor)
  {
    #ifndef LLVM_VERSION_BEFORE_4_0_0
    auto s = GetCursorDecl(cursor);
    if (auto e = llvm::dyn_cast_or_null<clang::DecompositionDecl>(s)) {
      return e->bindings().size();
    }
    #endif
    return 0;
  }

  CXCursor
  clang_ext_DecompositionDecl_GetBindings(CXCursor cursor, unsigned int i)
  {
    #ifndef LLVM_VERSION_BEFORE_4_0_0
    auto s = GetCursorDecl(cursor);
    if (auto e = llvm::dyn_cast_or_null<clang::DecompositionDecl>(s)) {
      return MakeCXCursor(e->bindings()[i], getCursorTU(cursor));
    }
    #endif
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(cursor));
  }

  enum clang_ext_AttrKind
  clang_ext_Attr_GetKind(CXCursor cursor)
  {
    auto a = GetCursorAttr(cursor);
    return static_cast<enum clang_ext_AttrKind>(a->getKind());
  }

  CXCursor
  clang_ext_VarTemplateDecl_getTemplatedDecl(CXCursor cursor)
  {
    auto s = GetCursorDecl(cursor);
    if (auto e = llvm::dyn_cast_or_null<clang::VarTemplateDecl>(s)) {
      return MakeCXCursor(e->getTemplatedDecl(), getCursorTU(cursor));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(cursor));
  }

  enum clang_ext_ExceptionSpecificationType
  clang_ext_FunctionProtoType_getExceptionSpecType(CXType t)
  {
    if (auto *fpt = GetQualType(t)->getAs<clang::FunctionProtoType>()) {
      switch (fpt->getExceptionSpecType()) {
      case clang::EST_None: return CLANG_EXT_EST_NoExceptionSpecification;
      case clang::EST_DynamicNone: return CLANG_EXT_EST_DynamicNone;
      case clang::EST_Dynamic: return CLANG_EXT_EST_Dynamic;
      case clang::EST_MSAny: return CLANG_EXT_EST_MSAny;
      case clang::EST_BasicNoexcept: return CLANG_EXT_EST_BasicNoexcept;
    #ifdef LLVM_VERSION_BEFORE_7_0_0
      case clang::EST_ComputedNoexcept: return CLANG_EXT_EST_DependentNoexcept;
    #else
      case clang::EST_DependentNoexcept: return CLANG_EXT_EST_DependentNoexcept;
      case clang::EST_NoexceptFalse: return CLANG_EXT_EST_NoexceptFalse;
      case clang::EST_NoexceptTrue: return CLANG_EXT_EST_NoexceptTrue;
    #endif
      case clang::EST_Unevaluated: return CLANG_EXT_EST_Unevaluated;
      case clang::EST_Uninstantiated: return CLANG_EXT_EST_Uninstantiated;
    #ifndef LLVM_VERSION_BEFORE_3_6_0
      case clang::EST_Unparsed: return CLANG_EXT_EST_Unparsed;
    #endif
      }
    }
    return CLANG_EXT_EST_NoExceptionSpecification;
  }

  unsigned int
  clang_ext_FunctionProtoType_getNumExceptions(CXType t)
  {
    if (auto *fpt = GetQualType(t)->getAs<clang::FunctionProtoType>()) {
      return fpt->getNumExceptions();
    }
    return 0;
  }

  CXType
  clang_ext_FunctionProtoType_getExceptionType(CXType t, unsigned int i)
  {
    if (auto *fpt = GetQualType(t)->getAs<clang::FunctionProtoType>()) {
      return MakeCXType(fpt->getExceptionType(i), GetTU(t));
    }
    return MakeCXTypeInvalid(GetTU(t));
  }

  CXCursor
  clang_ext_FunctionProtoType_getNoexceptExpr(CXType t)
  {
    if (auto *fpt = GetQualType(t)->getAs<clang::FunctionProtoType>()) {
      return MakeCXCursor(fpt->getNoexceptExpr(), GetTU(t));
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, GetTU(t));
  }
}
