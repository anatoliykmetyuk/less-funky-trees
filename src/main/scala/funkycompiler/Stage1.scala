package funkycompiler

import scala.quoted.*
import stage2.{ Tree => S2Tree, Expr => S2Expr, Block => S2Block, * }

private class Stage2StatsBuilder():
  private val stats = collection.mutable.ListBuffer.empty[Stat]
  export stats.{ append, nonEmpty }
  def mkBlockOrStats: S2Block | Stats =
    stats.last match
      case x: stage2.Expr => S2Block(Stats(stats.toList.dropRight(1)), x)
      case _ => Stats(stats.toList)
end Stage2StatsBuilder

object SupportedOp:
  def unapply(str: String): Boolean =
    Set(">", "<", "<=", ">=", "!=", "==",
        "+", "-", "*", "/",
        "&", "|").contains(str)
end SupportedOp

class TypeTest[T]:
  def unapply(using Quotes, Type[T])(t: quotes.reflect.Tree): Boolean =
    apply(t)
  def apply(using Quotes, Type[T])(t: quotes.reflect.Tree): Boolean =
    t match
      case t: quotes.reflect.Term => t.tpe <:< quotes.reflect.TypeRepr.of[T]
      case _ => false
end TypeTest

object IsStage2Tree  extends TypeTest[S2Tree]
object IsStage2Expr  extends TypeTest[S2Expr]
object IsStage2Stats extends TypeTest[Stats]


inline def stage(inline expr: Any): Any = ${stageImpl('expr)}
def stageImpl(expr: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*
  inline def asFTExpr(t: Term) = t.asExprOf[S2Expr]
  inline def asFTStats(t: Term) =
    if IsStage2Stats(t) then t.asExprOf[Stats]
    else '{Stats(${t.asExprOf[Stat]} :: Nil)}

  /** This map converts terms to Stage 2 blocks or stats */
  object blockOrStatsMap extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term =
      def withStatsBuildingMap(transform: StatsBuildingMap => List[Statement])(fallback: () => Term) =
        '{ val stage2StatsBuilder = new Stage2StatsBuilder }.asTerm match
          case Inlined(_, _, Block(valDef :: Nil, _)) =>
            val statsBuilderDefTree: Statement = valDef.asInstanceOf[Statement]
            val statsBuilderHandle: Term = Ref(statsBuilderDefTree.symbol)
            val statsResult: Expr[S2Block | Stats] = '{${statsBuilderHandle.asExprOf[Stage2StatsBuilder]}
              .mkBlockOrStats}.asTerm
            val map = StatsBuildingMap(statsBuilderHandle)

            val transformed = transform(map)
            if map.statsBuilder.nonEmpty then
              Block(statsBuilderDefTree :: stats, statsResult)
            else super.transformTerm(t)(owner)
      end withStatsBuilder

      t match
        case Block(stats, result) =>
          withStatsBuildingMap(_.transformStats(stats :+ result)(owner))

        case t =>
          withStatsBuildingMap(_.transformTerm(t)(owner) :: Nil)
  end blockMap

  class StatsBuildingMap(val statsBuilder: Term) extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term = t match
      case Block(stats, res) =>
        blockMap.transformTerm(t)(owner)

      // a > b => Operator
      case Apply(Apply(Ident(op@SupportedOp()), arg1 :: Nil), arg2 :: Nil) =>
        val tArg1 = transformTerm(arg1)(owner)
        val tArg2 = transformTerm(arg2)(owner)

        if IsStage2Tree(tArg1) || IsStage2Tree(tArg2) then
          val op1 = if IsStage2Tree(tArg1) then asFTExpr(tArg1) else '{LNumber(${tArg1.asExprOf[Double]})}
          val op2 = if IsStage2Tree(tArg2) then asFTExpr(tArg2) else '{LNumber(${tArg2.asExprOf[Double]})}
          '{BinaryOp($op1, $op2, ${Expr(op)})}.asTerm
        else super.transformTerm(t)(owner)

      // a := b => Assignment
      case Apply(Apply(Ident(":="), (arg1@IsStage2Tree()) :: Nil), arg2 :: Nil) =>
        val tArg2 = transformTerm(arg2)(owner)
        '{
          ${statsBuilder.asExprOf[Stage2StatsBuilder]}.
            append(Assignment(${arg1.asExprOf[Variable]}, ${asFTExpr(tArg2)}))
        }.asTerm

      // if a then ... else ... => If(a, ..., ...)
      case If(p, pos, neg) =>
        val tP   = transformTerm(p)(owner)
        val tPos = scopedMap.transformTerm(pos)(owner)
        val tNeg = scopedMap.transformTerm(neg)(owner)

        if IsStage2Tree(tP) then
          if IsStage2Expr(tPos) && IsStage2Expr(tNeg) then
            '{IfExpr(${asFTExpr(tP)}, ${asFTExpr(tPos)}, ${asFTExpr(tNeg)})}.asTerm
          else
            '{IfStat(${asFTExpr(tP)}, ${asFTStats(tPos)}, ${asFTStats(tNeg)})}.asTerm
        else super.transformTerm(t)(owner)

      // TODO funkylib.square(...)
      case _ => super.transformTerm(t)(owner)
  end termMap

  // println(s"Input:\n${expr.asTerm}\n\n")
  val out = scopedMap.transformTerm(expr.asTerm)(Symbol.spliceOwner)
  println(s"Output:\n${out.asExpr.show}")
  '{println(${out.asExprOf[Stage2AST]})}
end stageImpl
