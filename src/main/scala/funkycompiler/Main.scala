package funkycompiler

import stage3.Variable

val Pitch = Variable("Pitch")
val VTOL = Variable("VTOL")
val PitchRate = Variable("PitchRate")

@main def small = println(stage {
  val a = Variable("a")
  if VTOL > 10 then a := 20 else a := 10
})

// @main def main =
//   stage {
//     val Flaps = Variable("Flaps")
//     val Autotrim = Variable("Autotrim")

//     val maxPitchRate = 0.01
//     object flaps:
//       val extensionRate = 0.02
//       val maxExtension = 30

//     if PitchRate > maxPitchRate then
//       if Flaps < flaps.maxExtension then
//         Flaps := Flaps + flaps.extensionRate
//       else
//         Autotrim := Autotrim + flaps.extensionRate
//     else if PitchRate < -maxPitchRate then
//       Flaps := Flaps - flaps.extensionRate
//     else Flaps := Flaps
//   }
