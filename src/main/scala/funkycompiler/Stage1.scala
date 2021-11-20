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


val supportedBinaryOps = Set(
  ">", "<", "<=", ">=", "!=", "==",
  "+", "-", "*", "/",
  "&", "|")


inline def stage(inline expr: Any): Any = ${stageImpl('expr)}
def stageImpl(expr: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*

  extension (t: Tree)
    def isS3: Boolean = t match
      case t: Term => t.tpe <:< TypeRepr.of[S3Tree]
      case _ => false

    def maybeS3: Expr[S3Tree] | Null = t match
      case t: Term =>
        if t.isS3 then t.asExprOf[S3Tree]
        else if t.tpe <:< TypeRepr.of[Double] then '{S3Number(${t.asExprOf[Double]})}
        else null
      case _ => null

    def asS3: Expr[S3Tree] = t.maybeS3 match
      case null => throw RuntimeException("Only numbers can be automatically converted to funky trees")
      case x => x
  end extension

  object stage3Interpreter extends TreeMap:
    override def transformTerm(t: Term)(owner: Symbol): Term = t match
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

      case Apply(Apply(Ident(op), lhs :: Nil), rhs :: Nil)
      if supportedBinaryOps(op) =>
        val tLhs = transformTerm(lhs)(owner)
        val tRhs = transformTerm(rhs)(owner)

        if tLhs.isS3 || tRhs.isS3 then
          '{BinaryOp(${tLhs.asS3}, ${tRhs.asS3}, ${Expr(op)})}.asTerm
        else super.transformTerm(t)(owner)

      case Apply(Apply(Ident(":="), lhs :: Nil), rhs :: Nil)
        if lhs.tpe <:< TypeRepr.of[S3Variable] =>
        val tRhs = transformTerm(rhs)(owner)
        '{Assignment(${lhs.asExprOf[S3Variable]}, ${tRhs.asS3})}.asTerm

      case If(p, pos, neg) => transformTerm(p)(owner).maybeS3 match
        case null => super.transformTerm(t)(owner)
        case tPExpr =>
          val tPos = transformTerm(pos)(owner).asS3
          val tNeg = transformTerm(neg)(owner).asS3
          '{S3If($tPExpr, $tPos, $tNeg)}.asTerm

      case _ => super.transformTerm(t)(owner)
  end stage3Interpreter

  val out = stage3Interpreter.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExpr
  println(s"Output:\n${out.show}")
  out
end stageImpl
