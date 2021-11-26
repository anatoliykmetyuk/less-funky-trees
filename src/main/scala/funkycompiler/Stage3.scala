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
  case class If(condition: Tree, lhs: Tree, rhs: Tree | Null) extends Tree
  case class Block(stats: List[Tree]) extends Tree
  case class While(condition: Tree, stats: List[Tree]) extends Tree
  case class Parallel(stats: List[Tree]) extends Tree

  case class Assignment(lhs: Variable, rhs: Tree) extends Tree:
    val evaluationFlag = s4.VarRef(freshVarName(s"${lhs.name}_evaluationFlag"))

  extension (t: Tree)
    /**
     * A sequence of variable definitions that represent `t` on
     * the Funky Trees level. It is guaranteed that these variable
     * definitions are evaluated only once across all frame updates.
     * For loops, 'only once' means 'as many times as the
     * loop parameters permit the loop to run, but not more'.
     * It is possible to reevaluate these defs explicitly via `t.allowReevaluation`.
     */
    def defs: List[s4.VarDef] = t match
      case Const(n) => Nil
      case Variable(n) => Nil
      case Call(f, ts) => ts.flatMap(_.defs)
      case BinaryOp(lhs, rhs, sign) => lhs.defs ++ rhs.defs.after(lhs)
      case UnaryOp(rhs, sign) => rhs.defs
      case t@Assignment(Variable(name), rhs) =>
        rhs.defs ++ List(
          s4.VarDef(name, rhs.result, !t.evaluationFlag),
          s4.VarDef(t.evaluationFlag.name, s4.Const(true), s4.Const(true))).after(rhs)
      case If(cnd, lhs, rhs) =>
        cnd.defs ++ lhs.defs.ifTrue(cnd) ++
          (if rhs != null then rhs.defs.ifTrue(!cnd) else Nil)
      case Block(ts) => defsOneAfterAnother(ts)
      case Parallel(ts) => ts.flatMap(_.defs)
      case While(cnd, ts) =>
        cnd.defs ++ defsOneAfterAnother(ts).ifTrue(cnd) ++
          t.allowReevaluation.after(ts.last).ifTrue(cnd)

    /**
     * Defines what it means for a tree to be fully evaluated.
     * Returns an expression that is true if and only if `t` has been
     * fully evaluated and its `result` is fully computed.
     */
    def evaluated: s4.Expr = t match
      case _: (Const | Variable) => s4.Const(true)
      case Call(_, ts) => ts.allEvaluated
      case UnaryOp(rhs, _) => rhs.evaluated
      case BinaryOp(lhs, rhs, _) => lhs.evaluated & rhs.evaluated
      case t@Assignment(_, rhs) => rhs.evaluated & t.evaluationFlag
      case If(cnd, lhs, rhs) => lhs.evaluated |
        (if rhs != null then rhs.evaluated else s4.Const(true))
      case Block(ts) => ts.last.evaluated
      case Parallel(ts) => ts.allEvaluated
      case While(cnd, ts) => cnd.evaluated & !cnd.result & ts.last.evaluated

    /**
     * Reset all the variable evaluation flags, thus allowing them
     * to be computed one more time.
     */
    def allowReevaluation: List[s4.VarDef] = t match
      case _: (Const | Variable) => Nil
      case Call(_, ts) => ts.flatMap(_.allowReevaluation)
      case BinaryOp(lhs, rhs, _) => lhs.allowReevaluation ++ rhs.allowReevaluation
      case UnaryOp(rhs, _) => rhs.allowReevaluation
      case t@Assignment(_, rhs) => rhs.allowReevaluation :+
        s4.VarDef(t.evaluationFlag.name, s4.Const(false), s4.Const(true))
      case If(cnd, lhs, rhs) => cnd.allowReevaluation ++ lhs.allowReevaluation ++
        (if rhs != null then rhs.allowReevaluation else Nil)
      case Block(ts) => ts.flatMap(_.allowReevaluation)
      case Parallel(ts) => ts.flatMap(_.allowReevaluation)
      case While(cnd, ts) => cnd.allowReevaluation ++ ts.flatMap(_.allowReevaluation)

    def result: s4.Expr = t match
      case Const(x) => s4.Const(x)
      case Variable(n) => s4.VarRef(n)
      case Call(n, ts) => s4.Call(n, ts.map(_.result))
      case BinaryOp(lhs, rhs, sign) => s4.BinaryOp(lhs.result, rhs.result, sign)
      case UnaryOp(rhs, sign) => s4.UnaryOp(rhs.result, sign)
      case Assignment(_, rhs) => rhs.result
      case If(cnd, lhs, rhs) => s4.If(cnd.result, lhs.result,
        if rhs != null then rhs.result else null)
      case Block(ts) => ts.last.result
      case Parallel(ts) => ts.last.result
      case While(cnd, ts) => ts.last.result

    def compile = s4.VarDefs(t.defs).toXml
  end extension

  extension (vd: s4.VarDef)
    /** Evaluate `vd` only if `t` is evaluated. */
    def after(t: Tree): s4.VarDef = vd.copy(condition = vd.condition & t.evaluated)

    /** Evaluate `vd` only when `cnd`'s result is `true`. */
    def ifTrue(cnd: Tree): s4.VarDef =
      vd.copy(condition = vd.condition & cnd.result).after(cnd)

    /** Evaluate `vd` only after all of the `ts` are evaluated. */
    def afterAll(ts: List[Tree]): s4.VarDef =
      ts.foldLeft(vd) { case (vdTransformed, t) => vdTransformed.after(t) }
  end extension

  def defsOneAfterAnother(ts: List[Tree]) =
    ts.head.defs ++ (
      if ts.length > 1 then ts.sliding(2).flatMap {
        case prev :: next :: Nil => next.defs.after(prev) }
      else Nil)


  extension (vds: List[s4.VarDef])
    def after(t: Tree): List[s4.VarDef] = vds.map(_.after(t))
    def ifTrue(cnd: Tree): List[s4.VarDef] = vds.map(_.ifTrue(cnd))
    def afterAll(ts: List[Tree]): List[s4.VarDef] = vds.map(_.afterAll(ts))
  extension (ts: List[Tree])
    def allEvaluated: s4.Expr = ts.foldLeft(s4.Const(true): s4.Expr) {
      (accum, t) => accum & t.evaluated }
