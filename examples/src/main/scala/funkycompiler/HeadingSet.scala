package funkycompiler
package examples

import stage3.{ Tree, Variable }
import stdlib.*


@main def HeadingSet = program(testPlane) {
  levelPitch & setHeading(-60)
}
