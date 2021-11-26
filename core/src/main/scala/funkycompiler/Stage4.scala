package funkycompiler

object stage4:
  sealed trait Tree
  case class VarDefs(defs: List[VarDef]) extends Tree
  case class VarDef(name: String, expr: Expr, condition: Expr) extends Tree

  sealed trait Expr extends Tree
  case class Const(x: Double | Boolean) extends Expr
  case class VarRef(name: String) extends Expr
  case class BinaryOp(lhs: Expr, rhs: Expr, sign: String) extends Expr
  case class UnaryOp(rhs: Expr, sign: String) extends Expr
  case class If(condition: Expr, lhs: Expr, rhs: Expr | Null) extends Expr
  case class Call(name: String, args: List[Expr]) extends Expr

  extension (t: Tree) def toXml: String = t match
    case VarDefs(defs) =>
      s"<Variables>\n${defs.map(_.toXml).mkString("\n")}\n</Variables>"

    case VarDef(name, expr, condition) =>
      val maybeActivator = condition match
        case Const(true) => ""
        case cnd => s"""activator="${cnd.toXml}""""
      s"""  <Setter variable="$name" function="${expr.toXml}" $maybeActivator/>"""

    case Const(x) => x.toString
    case VarRef(name) => name
    case BinaryOp(lhs, rhs, sign) => s"(${lhs.toXml} ${sign} ${rhs.toXml})"
    case UnaryOp(rhs, sign) => s"(${sign}${rhs.toXml})"
    case If(cnd, lhs, rhs) =>
      if rhs == null then
        throw RuntimeException("If you are using `if` to compute a value, " +
          "it must have both `then` and `else` branches")
      s"(${cnd.toXml}?${lhs.toXml}:${rhs.toXml})"

    case Call(name, args) => s"$name(${args.map(_.toXml).mkString(",")})"
  end extension

  extension (e: Expr)
    def unary_! = UnaryOp(e, "!")
    def & (e2: Expr) = BinaryOp(e, e2, "&")
    def | (e2: Expr) = BinaryOp(e, e2, "|")

  extension (vd: VarDef) def simplify: VarDef = vd match
    case VarDef(name, expr, condition) => VarDef(name, expr.simplify, condition.simplify)

  extension (vds: VarDefs) def simplify: VarDefs =
    vds.copy(defs = vds.defs.map(_.simplify))

  extension (expr: Expr) def simplify: Expr =
    def simplificationPass(e: Expr) = e match
      case _: (Const | VarRef) => e

      case BinaryOp(Const(true), x, "&") => x
      case BinaryOp(x, Const(true), "&") => x
      case BinaryOp(Const(false), _, "&") => Const(false)
      case BinaryOp(_, Const(false), "&") => Const(false)
      case BinaryOp(Const(false), x, "|") => x
      case BinaryOp(x, Const(false), "|") => x
      case BinaryOp(Const(true), _, "|") => Const(true)
      case BinaryOp(_, Const(true), "|") => Const(true)

      case BinaryOp(lhs, rhs, sign) => BinaryOp(lhs.simplify, rhs.simplify, sign)
      case UnaryOp(rhs, sign) => UnaryOp(rhs.simplify, sign)
      case If(condition, lhs, rhs) => If(condition.simplify, lhs.simplify, rhs.simplify)
      case Call(name, args) => Call(name, args.map(_.simplify))
    end simplificationPass
    @annotation.tailrec def loop(e: Expr): Expr =
      val simplified = simplificationPass(e)
      if simplified != e then loop(simplified) else e
    end loop
    loop(expr)
