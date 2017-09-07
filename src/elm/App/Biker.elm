module App.Biker exposing (..)

import Cons
import Cons exposing (Cons)
import App.Map exposing (Location, Route, Speed, distanceTo, computeNewPoint, computePointOnLine)


type alias Biker =
  { id: Int
  , route: Cons Location
  , speed: Speed
  , location: Location
  , lastLocation: Location
  }


createBiker : Int -> Location -> Route -> Speed -> Biker
createBiker id start route speed =
  { id = id
  , route = route
  , speed = speed
  , location = start
  , lastLocation = start
  }


updateBiker: Biker -> Maybe Biker
updateBiker biker =
  let
    nextPoint = Cons.head biker.route
    step = distanceTo biker.location nextPoint
    nextRoute = biker.speed >= step
    newLocation =
      if not nextRoute then
        computePointOnLine biker.location nextPoint biker.speed
      else
        nextPoint
    newBiker =
      { biker | location = newLocation
              , lastLocation = biker.location
      }
  in
    if nextRoute then
      case Cons.tail biker.route of
        h :: t ->
          Just { newBiker | route = Cons.cons h t }
        [] ->
          Nothing
    else
      Just newBiker
