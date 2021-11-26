package funkycompiler

import funkycompiler.{ stage4 => s4 }


object stage3:
  case class Function(name: String)

  sealed trait Tree:
  case class Const(value: Double | Boolean) extends Tree
  case class Variable(name: String) extends Tree
  case class Call(function: String, args: List[Tree]) extends Tree
  case class BinaryOp(lhs: Tree, rhs: Tree, sign: String) extends Tree
  case class UnaryOp(rhs: Tree, sign: String) extends Tree
  case class If(condition: Tree, lhs: Tree, rhs: Tree | Null) extends Tree
  case class Block(stats: List[Tree]) extends Tree

  case class Assignment(lhs: Variable, rhs: Tree) extends Tree:
    val evaluationFlag = s4.VarRef(freshVarName("evaluationFlag"))

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
      case Call(f, ts) => ts.foldLeft(_.defs)
      case BinaryOp(lhs, rhs, sign) => lhs.defs ++ rhs.defs.after(lhs)
      case UnaryOp(rhs, sign) => rhs.defs
      case t@Assignment(Variable(name), rhs) =>
        rhs.defs :: List(
          s4.VarDef(name, rhs, !t.evaluationFlag),
          s4.VarDef(t.evaluationFlag.name, s4.Const(true), null)).after(rhs)
      case If(cnd, lhs, rhs) =>
        cnd.defs ++ lhs.defs.ifTrue(cnd) ++ rhs.defs.ifTrue(!cnd)
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
      case _: Const | Variable => true
      case Call(_, ts) => ts.allEvaluated
      case BinaryOp(lhs, rhs, _) => lhs.evaluated & rhs.evaluated
      case t@Assignment(_, rhs) => rhs.evaluated & evaluationFlag
      case If(cnd, lhs, rhs) => lhs.evaluated | rhs.evaluated
      case Block(ts) => ts.last.evaluated
      case Parallel(ts) => ts.allEvaluated
      case While(cnd, ts) => cnd.evaluated & !cnd.result & ts.last.evaluated

    /**
     * Reset all the variable evaluation flags, thus allowing them
     * to be computed one more time.
     */
    def allowReevaluation: List[s4.VarDef] = t match
      case _: Const | Variable => Nil
      case Call(_, ts) => ts.flatMap(_.allowReevaluation)
      case BinaryOp(lhs, rhs, _) => lhs.allowReevaluation ++ rhs.allowReevaluation
      case UnaryOp(rhs, _) => rhs.allowReevaluation
      case t@Assignment(_, rhs) => rhs.allowReevaluation :+ s4.VarDef(t.evaluationFlag.name, s4.Const(false), null)
      case If(cnd, lhs, rhs) => cnd.allowReevaluation ++ lhs.allowReevaluation ++ rhs.allowReevaluation
      case Block(ts) => ts.flatMap(_.allowReevaluation)
      case Parallel(ts) => ts.flatMap(_.allowReevaluation)
      case While(cnd, ts) => cnd.allowReevaluation ++ ts.flatMap(_.allowReevaluation)

    def varDefs = s4.VarDefs(t.defs)
    def compile = t.varDefs.mkXml
  end extension

  extension (vd: s4.VarDef)
    /** Evaluate `vd` only if `t` is evaluated. */
    def after(t: Tree): s4.VarDef = vd.copy(condition = vd.condition & t.evaluated)

    /** Evaluate `vd` only when `cnd`'s result is `true`. */
    def ifTrue(cnd: Tree): s4.VarDef =
      vd.after(cnd).copy(condition = vd.condition & cnd.result)

    /** Evaluate `vd` only after all of the `ts` are evaluated. */
    def afterAll(ts: List[Tree]): s4.VarDef =
      ts.foldLeft(vd) { case (vdTransformed, t) => vdTransformed.after(t) }
  end extension

  def defsOneAfterAnother(ts: List[Tree]) =
    ts.head.defs ++ ts.sliding(2).flatMap { case (prev, next) => next.defs.after(prev) }


  extension (vds: List[s4.VarDef])
    def after(t: Tree): List[s4.VarDef] = vds.map(_.after(t))
    def ifTrue(cnd: Tree): List[s4.VarDef] = vds.map(_.ifTrue(cnd))
    def afterAll(ts: List[Tree]): List[s4.VarDef] = vds.map(_.afterAll(ts))
  extension (ts: List[Tree])
    def allEvaluated: s4.Expr = ts.foldLeft(s4.Const(true)) { (accum, t) => accum & t }
