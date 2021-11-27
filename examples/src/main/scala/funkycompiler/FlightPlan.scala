package funkycompiler
package examples

import stage3.*
import stdlib.*


@main def FlightPlan = program(testPlane) {
  val headingSet = Variable("headingSet")
  val hdgVar = Variable("hdg")
  headingSet := false
  levelPitch & (for hdg <- 0 to 360 by 90 yield {
    hdgVar := hdg
    (setHeading(hdg, headingSet) | { while !headingSet do () ; waitFor(5) }): Tree
  })
  setHeading(0)
}
