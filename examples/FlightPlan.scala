package funkycompiler
package examples

import stage3.{ Tree, Variable, mkVarDefs, Assignment }
import stage4.mkXml
import stdlib.*

/** Returns a tree that follows the value of `t` and freezes at a value
 * that matches a given predicate. */
def captureValue(t: Tree, predicate: Tree => Tree): Tree = funky {
  val result = freshVar
  once {
    if predicate(t) then result := t
    predicate(t)
  }
  result
}

/** Evaluates the assignments in a given tree only until the tree evaluates to true. */
def once(t: Tree): Tree = funky {
  val guard = freshVar
  if !guard then
    if t then guard := guard | true
  guard
}

def wait(time: Tree): Tree = funky { once {
  val start = freshVar
  if start === 0 then
    start := Time
    false
  else if Time - start > time then true else false
}}

/** Evaluates trees one after another while making sure that
 * all previously evaluated trees stay true */
def checklist(ts: Tree*): Tree = funky {
  ts.foldLeft(true: Tree) { case (accum, t) =>
    if accum then t else false }
}

@main def FlightPlan = program(testPlane) {
  thrust := 1
  elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
  if checklist { headingSet(-60); wait(5) } then
    headingSet(60)
}
