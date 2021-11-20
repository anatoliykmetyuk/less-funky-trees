package funkycompiler

import stage3.*

def compilerError = throw RuntimeException("Compiler error: method wasn't interpreted")

extension (n: Double)
  def +(t: Tree) = compilerError
  def -(t: Tree) = compilerError
  def *(t: Tree) = compilerError
  def /(t: Tree) = compilerError

extension (n: Tree)
  def +(t: Tree) = compilerError
  def +(t: Double)    = compilerError
  def -(t: Tree) = compilerError
  def -(t: Double)    = compilerError
  def *(t: Tree) = compilerError
  def *(t: Double)    = compilerError
  def /(t: Tree) = compilerError
  def /(t: Double)    = compilerError

  def <(t: Tree) = compilerError
  def <(t: Double)    = compilerError
  def >(t: Tree) = compilerError
  def >(t: Double)    = compilerError

extension (v: Variable)
  def :=(value: Tree | Double): Tree = compilerError
