package funkycompiler
package examples

import stage3.*
import stdlib.*
import planes.Pidgin.{ file => pidgin, * }

@main def KrakabloaRace = program(pidgin) {
  thrust := 1
  setLevelPitch & funky {
    waitFor(5)
    setHeading(90)
  }
}
