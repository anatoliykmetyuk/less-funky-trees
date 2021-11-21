package funkycompiler

import stage3.*

def toBeExpanded = throw RuntimeException("Method wasn't expanded by the compiler")

given Conversion[Double, Const] = Const(_)
given Conversion[Int, Const] = Const(_)
given Conversion[Boolean, Const] = Const(_)
given Conversion[Tree, Boolean] = toBeExpanded
given treesToBlock: Conversion[Seq[Tree], Block] = ts => Block(ts.toList)

extension (n: Tree)
  def +(t: Tree) = BinaryOp(n, t, "+")
  def -(t: Tree) = BinaryOp(n, t, "-")
  def *(t: Tree) = BinaryOp(n, t, "*")
  def /(t: Tree) = BinaryOp(n, t, "/")

  def <(t: Tree) = BinaryOp(n, t, "&lt;")
  def >(t: Tree) = BinaryOp(n, t, "&gt;")
  def <=(t: Tree) = BinaryOp(n, t, "&lt;<")
  def >=(t: Tree) = BinaryOp(n, t, "&gt;>=")
  def !==(t: Tree) = BinaryOp(n, t, "!=")
  def ===(t: Tree) = BinaryOp(n, t, "=")

  def &(t: Tree) = BinaryOp(n, t, "&amp;")
  def |(t: Tree) = BinaryOp(n, t, "|")

  def unary_! = UnaryOp(n, "!")
end extension

extension (v: Variable) def :=(value: Tree) =
  Assignment(v, value)

extension (f: Function) def apply(values: Tree*) =
  Call(f.name, values.toList)
