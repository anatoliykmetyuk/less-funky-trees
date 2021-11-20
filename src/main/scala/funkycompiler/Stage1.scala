package funkycompiler

import scala.quoted.*
import stage3.{ Tree => S3Tree, Expr => S3Expr, Block => S3Block, * }

private class Stage3StatsBuilder():
  private val stats = collection.mutable.ListBuffer.empty[Stat]
  def append(stat: Stat): Unit = stats.append(stat)
  def mkBlockOrStats: S3Block | Stats =
    stats.last match
      case x: stage3.Expr => S3Block(Stats(stats.toList.dropRight(1)), x)
      case _ => Stats(stats.toList)
end Stage3StatsBuilder

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

object IsStage3Tree  extends TypeTest[S3Tree]
object IsStage3Expr  extends TypeTest[S3Expr]
object IsStage3Stats extends TypeTest[Stats]


inline def stage(inline expr: Any): Any = ${stageImpl('expr)}
def stageImpl(expr: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*
  def asFTExpr(t: Term): Expr[S3Expr] =
    if t.tpe <:< TypeRepr.of[S3Expr] then t.asExprOf[S3Expr]
    else if t.tpe <:< TypeRepr.of[Double] then '{LNumber(${t.asExprOf[Double]})}
    else throw RuntimeException("Only numbers can be automatically converted to funky trees")
    // '{
    //   if $tExpr.isInstanceOf[S3Expr] then $tExpr.asInstanceOf[S3Expr]
    //   else if $tExpr.isInstanceOf[Double] then LNumber($tExpr.asInstanceOf[Double])
    // }
      // '{ ${t.asExpr} match
      //   case t: S3Expr => t
      //   case n: Double => LNumber(n)
      // }
  def asFTStats(t: Term) = t.asExprOf[Stats]

  /** Interprets Scala blocks or terms to Stage 2 blocks or stats. */
  object blockInterpreter extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term =
      def withExprInterpreter(transform: ExprInterpreter => List[Statement]) =
        '{ val stage2StatsBuilder = new Stage3StatsBuilder }.asTerm match
          case Inlined(_, _, Block(valDef :: Nil, _)) =>
            val statsBuilderDefTree: Statement = valDef.asInstanceOf[Statement]
            val statsBuilderHandle: Term = Ref(statsBuilderDefTree.symbol)
            val statsResult: Expr[S3Block | Stats] =
              '{${statsBuilderHandle.asExprOf[Stage3StatsBuilder]}
                .mkBlockOrStats}
            val exprInterp = ExprInterpreter(statsBuilderHandle.asExprOf[Stage3StatsBuilder])

            val transformed = transform(exprInterp)
            if exprInterp.transformationDone then
              Block(statsBuilderDefTree :: transformed, statsResult.asTerm)
            else super.transformTerm(t)(owner)
      end withExprInterpreter

      t match
        case Block(stats, result) =>
          withExprInterpreter(_.transformStats(stats :+ result)(owner))

        case t =>
          withExprInterpreter(_.transformTerm(t)(owner) :: Nil)
  end blockInterpreter

  /** Interprets Scala expressions to Stage 2 expressions. */
  class ExprInterpreter(statsBuilderExpr: Expr[Stage3StatsBuilder]) extends TreeMap:
    private var _transformationDone = false
    def transformationDone = _transformationDone
    def flagTransformation(): Unit = _transformationDone = true

    private def appendStat(stat: Expr[Stat]) =
      _transformationDone = true
      '{ $statsBuilderExpr.append($stat) }

    override def transformTerm(t: Term)(owner: Symbol): Term = t match
      case Block(stats, res) =>
        blockInterpreter.transformTerm(t)(owner)

      // a > b => Operator
      case Apply(Apply(Ident(op@SupportedOp()), arg1 :: Nil), arg2 :: Nil) =>
        val tArg1 = transformTerm(arg1)(owner)
        val tArg2 = transformTerm(arg2)(owner)

        if IsStage3Tree(tArg1) || IsStage3Tree(tArg2) then
          flagTransformation()
          val op1 = if IsStage3Tree(tArg1) then asFTExpr(tArg1) else '{LNumber(${tArg1.asExprOf[Double]})}
          val op2 = if IsStage3Tree(tArg2) then asFTExpr(tArg2) else '{LNumber(${tArg2.asExprOf[Double]})}
          '{BinaryOp($op1, $op2, ${Expr(op)})}.asTerm
        else super.transformTerm(t)(owner)

      // a := b => Assignment
      case Apply(Apply(Ident(":="), (arg1@IsStage3Tree()) :: Nil), arg2 :: Nil) =>
        val tArg2 = transformTerm(arg2)(owner)
        appendStat('{Assignment(${arg1.asExprOf[Variable]}, ${asFTExpr(tArg2)})}).asTerm

      // if a then ... else ... => If(a, ..., ...)
      case If(p, pos, neg) =>
        val tP   = transformTerm(p)(owner)
        val tPos = blockInterpreter.transformTerm(pos)(owner).asExprOf[S3Block | Stats]
        val tNeg = blockInterpreter.transformTerm(neg)(owner).asExprOf[S3Block | Stats]

        if IsStage3Tree(tP) then
          flagTransformation()
          val tPExpr: Expr[S3Expr] = asFTExpr(tP)
          '{
            ($tPos, $tNeg) match
              case (pos: S3Expr, neg: S3Expr) => IfExpr($tPExpr, pos, neg)
              case (pos: Stats, neg: Stats) => IfStat($tPExpr, pos, neg)
              case _ => throw RuntimeException("Both branches of if statement must either be expressions or statements")
          }.asTerm
        else super.transformTerm(t)(owner)

      // TODO funkylib.square(...)
      case _ => super.transformTerm(t)(owner)
  end ExprInterpreter

  // println(s"Input:\n${expr.asTerm}\n\n")
  val out = blockInterpreter.transformTerm(expr.asTerm)(Symbol.spliceOwner)
    .asExpr
  println(s"Output:\n${out.show}")
  out
end stageImpl
