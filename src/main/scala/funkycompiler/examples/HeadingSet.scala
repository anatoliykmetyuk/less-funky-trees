package funkycompiler
package examples

import stage3.{ Tree, Variable, mkVarDefs, Assignment }
import stage4.mkXml
import stdlib.*


@main def HeadingSet = program(testPlane) {
  thrust := 1

  def checklist(ts: Tree*): Tree =
    ts.foldLeft(true: Tree) {
      case (check, ass: Assignment) =>
        if check then
          ass
          check
        else check
      case (check, next) => if check then next else check
    }

  def bankSet(tgt: Tree, maxDeviation: Tree = 5, deflection: Tree = 0.3): Tree =
    if abs(deltaangle(RollAngle, tgt)) > maxDeviation then
      ailerons := sign(RollAngle - tgt) * deflection
      false
    else
      ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
      true

  def headingSet(tgt: Tree, maxDeviation: Tree = 0.5): Tree =
    val delta = freshVar := deltaangle(tgt, Heading)
    val correctionAngle = freshVar
    val aileronDeflection = freshVar
    val maxBankDeviation = freshVar

    if abs(delta) > maxDeviation then
      if abs(delta) < 10 then
        correctionAngle := lerp(5, 10, 0.5)
        aileronDeflection := 0.1
        maxBankDeviation := 1
      else
        correctionAngle := 60
        aileronDeflection := 0.3
        maxBankDeviation := 5
      bankSet(sign(delta) * correctionAngle, maxBankDeviation, aileronDeflection)
      false
    else
      bankSet(0)

  elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
  headingSet(-60)

  // if abs(deltaangle(RollAngle, targetAngle)) > maxDeviation then
  //   ailerons := sign(RollAngle - targetAngle) * 0.3
  // else
  //   ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
}
