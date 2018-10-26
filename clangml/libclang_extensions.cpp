#include <clang-c/Index.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>
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
static CXString cxstring_createRef(const char *String) {
  CXString Str;
  Str.data = String;
  Str.private_flags = CXS_Unmanaged;
  return Str;
}

// Copied from clang source tree: tools/libclang/CXString.cpp
CXString cxstring_createDup(llvm::StringRef String) {
  CXString Result;
  char *Spelling = static_cast<char *>(malloc(String.size() + 1));
  if (Spelling == NULL) {
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
static const clang::Decl *getCursorDecl(CXCursor Cursor) {
  return static_cast<const clang::Decl *>(Cursor.data[0]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Stmt *getCursorStmt(CXCursor Cursor) {
  if (Cursor.kind == CXCursor_ObjCSuperClassRef ||
      Cursor.kind == CXCursor_ObjCProtocolRef ||
      Cursor.kind == CXCursor_ObjCClassRef)
    return nullptr;

  return static_cast<const clang::Stmt *>(Cursor.data[1]);
}

// From tools/libclang/CXType.cpp
static inline clang::QualType GetQualType(CXType CT) {
  return clang::QualType::getFromOpaquePtr(CT.data[0]);
}

extern "C" {
  CXString clang_ext_IntegerLiteral_getValueAsString(
      CXCursor c, unsigned Radix, bool isSigned) {
    const clang::IntegerLiteral *e =
      llvm::dyn_cast_or_null<clang::IntegerLiteral>(getCursorStmt(c));
    if (e == NULL) {
      return cxstring_createRef("");
    }
    std::string s = e->getValue().toString(Radix, isSigned);
    return cxstring_createDup(s);
  }

  enum clang_ext_UnaryOperatorKind clang_ext_UnaryOperator_getOpcode(
      CXCursor c) {
    const clang::UnaryOperator *e =
      llvm::dyn_cast_or_null<clang::UnaryOperator>(getCursorStmt(c));
    if (e == NULL) {
      return static_cast<clang_ext_UnaryOperatorKind>(0);
    }
    return static_cast<clang_ext_UnaryOperatorKind>(e->getOpcode());
  }

  CXString clang_ext_UnaryOperator_getOpcodeSpelling(
      enum clang_ext_UnaryOperatorKind Kind) {
    switch (Kind) {
#define UNARY_OPERATION(Name, Spelling)        \
      case CLANG_EXT_UNARY_OPERATOR_##Name:     \
        return cxstring_createRef(Spelling);
#include <clang/AST/OperationKinds.def>
    }
    //llvm_unreachable("Unsupported BinaryOperatorKind");
  }

  enum clang_ext_BinaryOperatorKind clang_ext_BinaryOperator_getOpcode(
      CXCursor c) {
    const clang::BinaryOperator *e =
      llvm::dyn_cast_or_null<clang::BinaryOperator>(getCursorStmt(c));
    if (e == NULL) {
      return static_cast<clang_ext_BinaryOperatorKind>(0);
    }
    return static_cast<clang_ext_BinaryOperatorKind>(e->getOpcode());
  }

  CXString clang_ext_BinaryOperator_getOpcodeSpelling(
      enum clang_ext_BinaryOperatorKind Kind) {
    switch (Kind) {
#define BINARY_OPERATION(Name, Spelling)        \
      case CLANG_EXT_BINARY_OPERATOR_##Name:     \
        return cxstring_createRef(Spelling);
#include <clang/AST/OperationKinds.def>
    }
    //llvm_unreachable("Unsupported BinaryOperatorKind");
  }

  unsigned clang_ext_ForStmt_getChildrenSet(CXCursor c) {
    const clang::ForStmt *e =
      llvm::dyn_cast_or_null<clang::ForStmt>(getCursorStmt(c));
    if (e == NULL) {
      return 0;
    }
    unsigned Result = 0;
    if (e->getInit()) {
      Result |= CLANG_EXT_FOR_STMT_INIT;
    }
    if (e->getConditionVariable()) {
      Result |= CLANG_EXT_FOR_STMT_CONDITION_VARIABLE;
    }
    if (e->getCond()) {
      Result |= CLANG_EXT_FOR_STMT_COND;
    }
    if (e->getInc()) {
      Result |= CLANG_EXT_FOR_STMT_INC;
    }
    return Result;
  }

  unsigned clang_ext_IfStmt_getChildrenSet(CXCursor c) {
    const clang::IfStmt *e =
      llvm::dyn_cast_or_null<clang::IfStmt>(getCursorStmt(c));
    if (e == NULL) {
      return 0;
    }
    unsigned Result = 0;
    if (e->getInit()) {
      Result |= CLANG_EXT_IF_STMT_INIT;
    }
    if (e->getConditionVariable()) {
      Result |= CLANG_EXT_IF_STMT_CONDITION_VARIABLE;
    }
    if (e->getElse()) {
      Result |= CLANG_EXT_IF_STMT_ELSE;
    }
    return Result;
  }

  unsigned clang_ext_SwitchStmt_getChildrenSet(CXCursor c) {
    const clang::SwitchStmt *e =
      llvm::dyn_cast_or_null<clang::SwitchStmt>(getCursorStmt(c));
    if (e == NULL) {
      return 0;
    }
    unsigned Result = 0;
    if (e->getInit()) {
      Result |= CLANG_EXT_SWITCH_STMT_INIT;
    }
    if (e->getConditionVariable()) {
      Result |= CLANG_EXT_SWITCH_STMT_CONDITION_VARIABLE;
    }
    return Result;
  }

  unsigned clang_ext_WhileStmt_getChildrenSet(CXCursor c) {
    const clang::WhileStmt *e =
      llvm::dyn_cast_or_null<clang::SwitchStmt>(getCursorStmt(c));
    if (e == NULL) {
      return 0;
    }
    unsigned Result = 0;
    if (e->getConditionVariable()) {
      Result |= CLANG_EXT_WHILE_STMT_CONDITION_VARIABLE;
    }
    return Result;
  }

  enum clang_ext_ElaboratedTypeKeyword clang_ext_ElaboratedType_getKeyword(
      CXType CT) {
    clang::QualType T = GetQualType(CT);
    const clang::Type *TP = T.getTypePtrOrNull();
    if (TP && TP->getTypeClass() == clang::Type::Elaborated) {
      return static_cast<enum clang_ext_ElaboratedTypeKeyword>(
        clang::cast<clang::ElaboratedType>(TP)->getKeyword());
    }
    return static_cast<enum clang_ext_ElaboratedTypeKeyword>(0);
  }

  CXString clang_ext_ElaboratedType_getKeywordSpelling(
      enum clang_ext_ElaboratedTypeKeyword keyword) {
    return cxstring_createDup(clang::TypeWithKeyword::getKeywordName(
      static_cast<clang::ElaboratedTypeKeyword>(keyword)));
  }

  bool clang_ext_VarDecl_hasInit(CXCursor c) {
    const clang::VarDecl *d =
      llvm::dyn_cast_or_null<clang::VarDecl>(getCursorDecl(c));
    if (d == NULL) {
      return false;
    }
    return d->getInit() != NULL;
  }
}
