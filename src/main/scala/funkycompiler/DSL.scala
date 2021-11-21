package funkycompiler

import stage3.*

def toBeExpanded = throw RuntimeException("Method wasn't expanded by the compiler")

given Conversion[Double, Number] = Number(_)
given Conversion[Int, Number] = Number(_)
given Conversion[Tree, Boolean] = toBeExpanded

extension (n: Tree)
  def +(t: Tree) = BinaryOp(n, t, "+")
  def -(t: Tree) = BinaryOp(n, t, "-")
  def *(t: Tree) = BinaryOp(n, t, "*")
  def /(t: Tree) = BinaryOp(n, t, "/")

  def <(t: Tree) = BinaryOp(n, t, "<")
  def >(t: Tree) = BinaryOp(n, t, ">")
  def <=(t: Tree) = BinaryOp(n, t, "<=")
  def >=(t: Tree) = BinaryOp(n, t, ">=")
  def !==(t: Tree) = BinaryOp(n, t, "!=")
  def ===(t: Tree) = BinaryOp(n, t, "==")

  def &(t: Tree) = BinaryOp(n, t, "&")
  def |(t: Tree) = BinaryOp(n, t, "|")

  def unary_! = UnaryOp(n, "!")
end extension

extension (v: Variable) def :=(value: Tree) =
  Assignment(v, value)

extension (f: Function) def apply(values: Tree*) =
  Call(f, values.toList)
