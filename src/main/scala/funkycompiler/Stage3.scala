package funkycompiler

import funkycompiler.{ stage4 => s4 }


object stage3:
  case class Function(name: String)

  sealed trait Tree:
    lazy val guard = s4.VarRef(freshVarName("guard"))
    lazy val setGuard = s4.VarDef(guard.name, s4.Const(true), null)
    lazy val resetGuard = s4.VarDef(guard.name, s4.Const(false), null)

  case class Const(value: Double | Boolean) extends Tree
  case class Variable(name: String) extends Tree
  case class Call(function: String, args: List[Tree]) extends Tree
  case class BinaryOp(lhs: Tree, rhs: Tree, sign: String) extends Tree
  case class UnaryOp(rhs: Tree, sign: String) extends Tree
  case class Assignment(lhs: Variable, rhs: Tree) extends Tree
  case class If(condition: Tree, lhs: Tree, rhs: Tree | Null) extends Tree
  case class Block(stats: List[Tree]) extends Tree

  /**
   * 1. `defs` must return a sequence of variable definitions that
   * is evaluated only once.
   * 2. `defs` returns a `guard` of the tree. A guard is true if and
   * only if (1) no variable in the tree is evaluated anymore, and
   * (2) the result of the tree is fully computed.
   */
  extension (t: Tree) def defs: List[s4.VarDef] = t match
    case Const(n) => t.setGuard
    case Variable(n) => t.setGuard
    case Call(f, ts) => ts.foldLeft(_.defs).guardedBy(t) :+ t.setGuard.afterAll(ts)
    case BinaryOp(lhs, rhs, sign) =>
      (lhs.defs ++ rhs.defs.after(lhs)).guardedBy(t) :+ t.setGuard.after(rhs)
    case UnaryOp(rhs, sign) => rhs.defs.guardedBy(t) :+ t.setGuard.after(rhs)
    case Assignment(Variable(name), rhs) =>
      rhs.defs :+ VarDef(name, rhs, !t.guard).after(rhs) :+ t.setGuard.after(rhs)
    case If(cnd, lhs, rhs) =>
      (cnd.defs ++ lhs.defs.after(cnd).ifTrue(cnd) ++ rhs.defs.after(cnd).ifTrue(!cnd))
        .guardedBy(t) :+ t.setGuard.afterOneOf(lhs, rhs)
    case Block(ts) =>
      defsOneAfterAnother(ts).guardedBy(t) :+ t.setGuard.after(ts.last)
    case Parallel(ts) =>
      ts.flatMap(_.defs).guardedBy(t) :+ t.setGuard.afterAll(ts)
    case While(cnd, ts) =>
      (cnd.defs ++ defsOneAfterAnother(ts)).guardedBy(t) ++
      t.resetAllGuards.after(ts.last).ifTrue(cnd) ++
      t.setGuard.after(ts.last).ifTrue(!cnd)

  extension (t: Tree) def resetAllGuards: List[s4.VarDef] = t.resetGuard :: (t match
    case Const(_) => Nil
    case Variable(_) => Nil
    case Call(_, ts) => ts.flatMap(_.resetAllGuards)
    case BinaryOp(lhs, rhs, _) => lhs.resetAllGuards ++ rhs.resetAllGuards
    case UnaryOp(rhs, _) => rhs.resetAllGuards
    case Assignment(_, rhs) => rhs.resetAllGuards
    case If(cnd, lhs, rhs) => cnd.resetAllGuards ++ lhs.resetAllGuards ++ rhs.resetAllGuards
    case Block(ts) => ts.flatMap(_.resetAllGuards)
    case Parallel(ts) => ts.flatMap(_.resetAllGuards)
    case While(cnd, ts) => cnd.resetGuard ++ ts.flatMap(_.resetAllGuards)



  def defsOneAfterAnother(ts: List[Tree]) =
    ts.head.defs ++ ts.sliding(2).flatMap { case (prev, next) => next.defs.after(prev) }

  def after = ???
  def ifTrue = ???
  def guardedBy = ???
  def afterAll = ???


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
