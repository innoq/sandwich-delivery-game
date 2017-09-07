module App.Player exposing (..)

import App.Map exposing (Id, Location, Road, computeNewPoint)
import Time exposing (Time, millisecond)


type alias Player =
  { location : Location
  , lastLocation : Location
  , radius : Int
  , speed : Float
  , bearing : Float
  , stearingAngle : Float
  , outOfStreet : Bool
  , distanceToRoads : Float
  }


type alias PlayerMovement =
  { speed : Float
  , stearingAngle : Float
  }


createPlayer : Location -> Float -> Player
createPlayer location bearing =
  { location = location
  , lastLocation = location
  , radius = 10
  , speed = 0
  , bearing = bearing
  , stearingAngle = 0
  , outOfStreet = False
  , distanceToRoads = 0
  }


updatePlayerMovement : Player -> PlayerMovement -> Player
updatePlayerMovement p m =
  { p | speed = max 0 m.speed
      , stearingAngle = m.stearingAngle
  }


computePlayerSpeed : Player -> Float -> (Float, Bool)
computePlayerSpeed player d =
  let
    dm = d-- * 1000
    dt = max (dm - 0.06) 0
    r  = max 0.25 (1 - 1.8 * (sqrt(dt)))
    outOfStreet = r == 0.25
    s  = 2 * player.speed * r
  in
    (s, outOfStreet)


updatePlayerPosition : Player -> Bool -> Time -> Time -> Player
updatePlayerPosition p canRotateWithoutSpeed old new =
  let
    timeDiff = new - old

    d = p.distanceToRoads
    (s, outOfStreet) = computePlayerSpeed p d

    b = if canRotateWithoutSpeed || s > 0 then
          p.bearing + p.stearingAngle * 0.2
        else
          p.bearing
    md = movedDistance s timeDiff
    l = computeNewPoint p.location md b
  in
  { p | location = l
      , lastLocation = p.location
      , bearing = b
      , outOfStreet = outOfStreet
  }


updatePlayerDistanceToRoads : Player -> Float -> Player
updatePlayerDistanceToRoads p d =
  { p | distanceToRoads = d }


movedDistance : Float -> Time -> Float
movedDistance speed ms =
  (ms * speed) / (1 * 60 * 60 * 1000)

