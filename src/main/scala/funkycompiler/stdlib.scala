package funkycompiler

import stage3.*

object stdlib extends StdLibVariables with StdLibFunctions

trait StdLibVariables:
  val Pitch = Variable("Pitch")
  val VTOL = Variable("VTOL")
  val PitchRate = Variable("PitchRate")
end StdLibVariables

trait StdLibFunctions:
  val abs = Function("abs")
  val ceil = Function("ceil")
  val exp = Function("exp")
end StdLibFunctions
