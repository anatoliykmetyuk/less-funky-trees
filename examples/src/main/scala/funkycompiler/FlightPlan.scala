package funkycompiler
package examples

import stage3.*
import stdlib.*


@main def FlightPlan = program(testPlane) {
  val headingSet = Variable("headingSet")
  headingSet := false
  levelPitch & {
    setHeading(60, headingSet) | { while !headingSet do () ; waitFor(10) }
    setHeading(-60, headingSet) | { while !headingSet do () ;  waitFor(10) }
    setHeading(0, headingSet)
  }
}
