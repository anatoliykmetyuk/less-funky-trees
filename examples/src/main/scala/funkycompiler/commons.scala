package funkycompiler
package examples

import stage3.*
import stdlib.*

def plane(planeName: String) =
  val basedir = java.io.File("/Users/kmetiuk/Library/Application Support/unity.Jundroo.SimplePlanes/AircraftDesigns/")
  java.io.File(basedir, s"$planeName.xml")

val elevators = Variable("elevators")
val ailerons = Variable("ailerons")
val rudder = Variable("rudder")
val thrust = Variable("thrust")
val rateOfDescent = rate(AltitudeAgl)

def waitFor(time: Tree): Tree = funky {
  val start = freshVar()
  start := Time
  while Time - start < time do ()
}
