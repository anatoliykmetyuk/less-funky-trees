package funkycompiler

object stage4:
  sealed trait Tree
  case class VarDefs(defs: List[VarDef]) extends Tree
  case class VarDef(name: String, expr: Expr, condition: Expr | Null) extends Tree

  sealed trait Expr extends Tree
  case class Const(x: Double | Boolean) extends Expr
  case class VarRef(name: String) extends Expr
  case class BinaryOp(lhs: Expr, rhs: Expr, sign: String) extends Expr
  case class UnaryOp(rhs: Expr, sign: String) extends Expr
  case class If(condition: Expr, lhs: Expr, rhs: Expr | Null) extends Expr
  case class Call(name: String, args: List[Expr]) extends Expr
