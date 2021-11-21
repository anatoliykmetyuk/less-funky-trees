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

  def mkXml(t: Tree): String = t match
    case VarDefs(defs) =>
      s"<Variables>\n${defs.map(mkXml).mkString("\n")}\n</Variables>"

    case VarDef(name, expr, condition) =>
      val maybeActivator = if condition != null then s"""activator="${mkXml(condition)}" """ else ""
      s"""  <Setter name="$name" value="${mkXml(expr)}" $maybeActivator/>"""

    case Const(x) => x.toString
    case VarRef(name) => name
    case BinaryOp(lhs, rhs, sign) => s"(${mkXml(lhs)})$sign(${mkXml(rhs)})"
    case UnaryOp(rhs, sign) => s"${sign}(${mkXml(rhs)})"
    case If(cnd, lhs, rhs) =>
      if rhs == null then
        throw RuntimeException("If you are using `if` to compute a value, " +
          "it must have both `then` and `else` branches")
      s"(${mkXml(cnd)})?(${mkXml(lhs)}):(${mkXml(rhs)})"

    case Call(name, args) => s"$name(${args.map(mkXml).mkString(",")})"
