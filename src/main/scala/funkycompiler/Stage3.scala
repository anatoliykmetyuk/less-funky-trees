package funkycompiler

import funkycompiler.{ stage4 => s4 }


object stage3:
  case class Function(name: String)

  sealed trait Tree
  case class Const(value: Double | Boolean) extends Tree
  case class Variable(name: String) extends Tree
  case class Call(function: String, args: List[Tree]) extends Tree
  case class BinaryOp(lhs: Tree, rhs: Tree, sign: String) extends Tree
  case class UnaryOp(rhs: Tree, sign: String) extends Tree
  case class Assignment(lhs: Variable, rhs: Tree) extends Tree
  case class If(condition: Tree, lhs: Tree, rhs: Tree | Null) extends Tree
  case class Block(stats: List[Tree]) extends Tree

  private def interpret(t: Tree): (List[s4.VarDef], s4.Expr) = t match
    case Const(n) => (Nil, s4.Const(n))
    case Variable(n) => (Nil, s4.VarRef(n))
    case Call(name, args) =>
      val compiledArgs = args.map(interpret)
      val argsDefs = compiledArgs.flatMap(_._1)
      val argsExprs = compiledArgs.map(_._2)
      (argsDefs, s4.Call(name, argsExprs))

    case BinaryOp(rhs, lhs, sign) =>
      val (lhsDefs, lhsExpr) = interpret(lhs)
      val (rhsDefs, rhsExpr) = interpret(rhs)
      (lhsDefs ++ rhsDefs, s4.BinaryOp(rhsExpr, lhsExpr, sign))

    case UnaryOp(rhs, sign) =>
      val (rhsDefs, rhsExpr) = interpret(rhs)
      (rhsDefs, s4.UnaryOp(rhsExpr, sign))

    case Assignment(Variable(name), rhs) =>
      val (rhsDefs, rhsExpr) = interpret(rhs)
      (rhsDefs :+ s4.VarDef(name, rhsExpr, null), s4.VarRef(name))

    case If(cnd, lhs, rhs) =>
      val (cndDefs, cndExpr) = interpret(cnd)
      val (lhsDefs, lhsExpr) = interpret(lhs)
      val (rhsDefs, rhsExpr) =
          if rhs != null then interpret(rhs) else (Nil, null)
      (cndDefs ++ lhsDefs.withCondition(cndExpr) ++ rhsDefs.withCondition(s4.UnaryOp(cndExpr, "!")),
        s4.If(cndExpr, lhsExpr, rhsExpr))

    case Block(stats) =>
      val statsDefs = stats.flatMap(interpret(_)._1)
      val lastExpr = interpret(stats.last)._2
      (statsDefs, lastExpr)
  end interpret

  extension (ds: List[s4.VarDef]) def withCondition(expr: s4.Expr) =
    ds.map(d =>
      val newCond =
        if d.condition != null then s4.BinaryOp(expr, d.condition, "&amp;")
        else expr
      d.copy(condition = newCond))

  def mkVarDefs(t: Tree) = s4.VarDefs(interpret(t)._1)
