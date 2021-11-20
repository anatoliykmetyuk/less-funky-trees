package funkycompiler

object stage3:
  sealed trait Tree
  case class Stats(ss: List[Stat]) extends Tree

  sealed trait Expr extends Tree
  case class LNumber(value: Double) extends Expr
  case class Variable(name: String) extends Expr
  case class BinaryOp(lhs: Expr, rhs: Expr, sign: String) extends Expr
  case class Block(stats: Stats, result: Expr) extends Expr
  case class IfExpr(p: Expr, lhs: Expr, rhs: Expr) extends Expr

  sealed trait Stat extends Tree
  case class Assignment(lhs: Variable, rhs: Expr) extends Stat
  case class IfStat(p: Expr, lhs: Stats, rhs: Stats) extends Stat

// def compile(ft: stage2.Tree): String = ???
//   case Variable(v) => v
//   case Call(f, args) =>
//     s"$f(${args.map(compile).mkString(",")})"
//   case Assignment(v, value) =>
//     s"<Setter variable=\"${compile(v)}\" function=\"${compile(value)}\" priority=\"0\" />"
//   case Literal(v) => v.toString
//   case BinaryOperator(t1, t2, sign) => s"(${compile(t1)})$sign(${compile(t2)})"
//   case IfOp(cond, pos, neg) => s"(${compile(cond)})?(${compile(pos)}):(${compile(neg)})"

// def compile(vars: Variables): String =
//   s"<Variables>${vars.vars.map(compile).mkString("")}</Variables>"
