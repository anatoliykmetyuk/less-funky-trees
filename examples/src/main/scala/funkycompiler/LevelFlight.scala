package funkycompiler
package examples

import stage3.{ Tree, Variable }
import stdlib.*
import planes.Pidgin.{ file => pidgin, * }


@main def LevelFlight = program(pidgin) {
  setLevelPitch
}
