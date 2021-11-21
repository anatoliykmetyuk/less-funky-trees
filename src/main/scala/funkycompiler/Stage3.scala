package funkycompiler

object stage3:
  sealed trait Tree
  case class Number(value: Double) extends Tree
  case class Variable(name: String) extends Tree
  case class BinaryOp(lhs: Tree, rhs: Tree, sign: String) extends Tree
  case class UnaryOp(rhs: Tree, sign: String) extends Tree
  case class Assignment(lhs: Tree, rhs: Tree) extends Tree
  case class If(condition: Tree, lhs: Tree, rhs: Tree) extends Tree
  case class Block(stats: List[Tree]) extends Tree
