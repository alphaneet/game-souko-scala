package neet.game.puzzle

package object souko {
//{{{

import processing.core.PConstants

type Point = java.awt.Point

val CrossKeyMap = Map(
  PConstants.UP    -> new Point(0, -1), 
  PConstants.LEFT  -> new Point(-1, 0),
  PConstants.DOWN  -> new Point(0,  1), 
  PConstants.RIGHT -> new Point( 1, 0)
)

val SoukoKindList = List(
  SoukoKind.Space,
  SoukoKind.Wall,
  SoukoKind.Box,
  SoukoKind.Goal,
  SoukoKind.GoalBox,
  SoukoKind.Player
)

private val _SoukoKindMap = Map(
  "Space"   -> SoukoKind.Space,
  "Wall"    -> SoukoKind.Wall,
  "Box"     -> SoukoKind.Box,
  "Goal"    -> SoukoKind.Goal,
  "GoalBox" -> SoukoKind.GoalBox,
  "Player"  -> SoukoKind.Player
)

def SoukoKindMap(key:String) = {
  _SoukoKindMap(key.substring(0, 1).toUpperCase +
                key.substring(1))
}

//}}}
}

// vim: set fdm=marker:
