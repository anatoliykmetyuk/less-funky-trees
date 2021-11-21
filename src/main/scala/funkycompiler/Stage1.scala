package funkycompiler

import scala.quoted.*
import stage3.{ Tree => S3Tree, Block => S3Block, Variable => S3Variable, Number => S3Number, If => S3If, * }

private class S3BlockBuilder:
  private val stats = collection.mutable.ListBuffer.empty[S3Tree]
  def append(stat: S3Tree): Unit = stats.append(stat)
  def mkBlock: S3Block = S3Block(stats.toList)
end S3BlockBuilder

def mkS3BlockBuilder(using Quotes): (quotes.reflect.Statement, Expr[S3BlockBuilder]) =
  import quotes.reflect.*
  '{ val s3BlockBuilder = new S3BlockBuilder }.asTerm match
    case Inlined(_, _, Block(valDef :: Nil, _)) =>
      val expr = Ref(valDef.symbol).asExprOf[S3BlockBuilder]
      (valDef, expr)
end mkS3BlockBuilder


inline def stage(inline expr: Any): S3Tree = ${stageImpl('expr)}
def stageImpl(expr: Expr[Any])(using Quotes): Expr[S3Tree] =
  import quotes.reflect.*

  extension (t: Tree)
    def isS3: Boolean = t match
      case t: Term => t.tpe <:< TypeRepr.of[S3Tree]
      case _ => false

    def maybeS3: Expr[S3Tree] | Null =
      if t.isS3 then t.asS3 else null

    def asS3: Expr[S3Tree] =
      t.asExprOf[S3Tree]
  end extension

  object stage3Interpreter extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term = t match
      case Apply(Select(Ident("given_Conversion_Tree_Boolean"), "apply")
        , s3tree :: Nil) => s3tree

      case Block(stats, res) =>
        val tStats = transformStats(stats)(owner)
        val tRes = transformTerm(res)(owner)

        if tStats.exists(_.isS3) then
          val (builderDef, builderExpr) = mkS3BlockBuilder
          val tStatsRecorded = for stat <- (tStats :+ tRes) yield
            stat.maybeS3 match
              case null => stat
              case stat => '{ $builderExpr.append($stat) }.asTerm
          Block.copy(t)(builderDef :: tStatsRecorded, '{$builderExpr.mkBlock}.asTerm)
        else Block.copy(t)(tStats, tRes)

      case If(p, pos, neg) => transformTerm(p)(owner).maybeS3 match
        case null => super.transformTerm(t)(owner)
        case tPExpr =>
          val tPos = transformTerm(pos)(owner).asS3
          val tNeg = transformTerm(neg)(owner) match
            case Literal(UnitConstant()) => '{null}
            case x => x.asS3
          '{S3If($tPExpr, $tPos, $tNeg)}.asTerm

      case _ => super.transformTerm(t)(owner)
  end stage3Interpreter

  val out = stage3Interpreter.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[S3Tree]
  // println(s"Output:\n${out.show}")
  out
end stageImpl
