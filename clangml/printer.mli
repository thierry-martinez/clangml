module Make (Node : Clang__ast.NodeS) :
  Ast_sig.PrinterS with module Node := Node
