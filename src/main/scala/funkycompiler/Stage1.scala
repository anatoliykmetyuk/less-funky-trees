package funkycompiler

import scala.quoted.*
import funkycompiler.{ stage3 => s3 }


inline def stage(inline expr: Any): s3.Tree = ${stageImpl('expr)}
def stageImpl(expr: Expr[Any])(using Quotes): Expr[s3.Tree] =
  import quotes.reflect.*

  extension (t: Tree)
    def is[T: Type] = t match
      case t: Term => t.tpe <:< TypeRepr.of[T]
      case _ => false

    def isS3: Boolean = t.is[s3.Tree]

    def maybeS3: Expr[s3.Tree] | Null =
      if t.isS3 then t.asS3 else null

    def asS3: Expr[s3.Tree] = t match
      case t: Term if t.is[Boolean] => '{booleanToConst(${t.asExprOf[Boolean]})}
      case t: Term if t.is[Int] => '{intToConst(${t.asExprOf[Int]})}
      case t: Term if t.is[Double] => '{doubleToConst(${t.asExprOf[Double]})}
      case t => t.asExprOf[s3.Tree]

  end extension

  object stage3Interpreter extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term = t match
      case Apply(Select(Ident("given_Conversion_Tree_Boolean"), "apply")
        , s3tree :: Nil) => s3tree

      case Block(stats, res) =>
        def treeTermsToBlockTerm(t: Term): Term = t match
          case t: Term if t.is[Seq[s3.Tree]] =>
            '{treesToBlock(${t.asExprOf[Seq[s3.Tree]]})}.asTerm
          case t => t

        val tStats = transformStats(stats)(owner).map {
          case t: Term => treeTermsToBlockTerm(t)
          case s => s
        }
        val tRes = treeTermsToBlockTerm(transformTerm(res)(owner)) match
          case t: Term if t.is[Boolean] | t.is[Int] | t.is[Double] => t.asS3.asTerm
          case t => t

        if tStats.exists(_.isS3) then
          val tStatsRecorded = Expr.ofList((tStats :+ tRes).collect {
            case s: Term if s.isS3 => s.asS3 })
          Block.copy(t)(tStats :+ tRes, '{s3.Block($tStatsRecorded)}.asTerm)
        else Block.copy(t)(tStats, tRes)

      case If(p, pos, neg) => transformTerm(p)(owner).maybeS3 match
        case null => super.transformTerm(t)(owner)
        case tPExpr: Expr[s3.Tree] =>
          val tPos = transformTerm(pos)(owner).asS3
          val tNeg = transformTerm(neg)(owner) match
            case Literal(UnitConstant()) => '{null}
            case x => x.asS3
          '{s3.If($tPExpr, $tPos, $tNeg)}.asTerm

      case _ => super.transformTerm(t)(owner)
  end stage3Interpreter

  val out = stage3Interpreter.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[s3.Tree]
  // println(s"Output:\n${out.show}")
  out
end stageImpl
