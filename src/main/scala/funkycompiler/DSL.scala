package funkycompiler

import java.io.File

import stage3.*

def toBeExpanded = throw RuntimeException("Method wasn't expanded by the compiler")

given doubleToConst: Conversion[Double, Const] = Const(_)
given intToConst: Conversion[Int, Const] = Const(_)
given booleanToConst: Conversion[Boolean, Const] = Const(_)
given Conversion[Tree, Boolean] = toBeExpanded
given treesToBlock: Conversion[Seq[Tree], Block] = ts => Block(ts.toList)

extension (n: Tree)
  def +(t: Tree) = BinaryOp(n, t, "+")
  def -(t: Tree) = BinaryOp(n, t, "-")
  def *(t: Tree) = BinaryOp(n, t, "*")
  def /(t: Tree) = BinaryOp(n, t, "/")

  def <(t: Tree) = BinaryOp(n, t, "&lt;")
  def >(t: Tree) = BinaryOp(n, t, "&gt;")
  def <=(t: Tree) = BinaryOp(n, t, "&lt;")
  def >=(t: Tree) = BinaryOp(n, t, "&gt;")
  def !==(t: Tree) = BinaryOp(n, t, "!=")
  def ===(t: Tree) = BinaryOp(n, t, "=")

  def &(t: Tree) = BinaryOp(n, t, "&amp;")
  def |(t: Tree) = BinaryOp(n, t, "|")

  def unary_! = UnaryOp(n, "!")
end extension

extension (v: Variable)
  def :=(value: Tree) = Block(List(Assignment(v, value),v))
  def :+=(value: Tree) = v := v + value
  def :-=(value: Tree) = v := v - value
  def :/=(value: Tree) = v := v / value
  def :*=(value: Tree) = v := v * value

extension (f: Function) def apply(values: Tree*) =
  Call(f.name, values.toList)

extension (ts: Seq[Tree]) def sumTrees =
  ts.foldLeft(0: Tree) { case (accum, t) => accum + t }

inline def program(plane: File)(inline expr: Any): Unit =
  val xml = stage4.mkXml(stage3.mkVarDefs(stage(expr)))
  writeVariables(plane, xml)

private var freshVarCounter = 0
def freshVar(prefix: String = "syntheticVar") =
  freshVarCounter += 1
  Variable(s"${prefix}${freshVarCounter}")

def defineVariable(initialValue: Tree) = ??? // todo
