package funkycompiler

import stage3.*
import stdlib.*


@main def main = println(
  funky {
    def f(x: Tree): Tree =
      if x === 0 then
        Pitch := 10
        false
      else
        Pitch := 20
        true
    if f(10) then
      Roll := 1
  }.compile
)