module App.Map exposing (..)

import Cons exposing (Cons)
import Native.Geofunctions


type alias Id = String


type alias Location =
  { lat: Float
  , lng: Float
  }


type alias Road =
  { location1: Location
  , location2: Location
  }

type alias Speed = Float


type alias Route = Cons Location


type alias Area =
  { edges : List Location
  }


createRoad : Location -> Location -> Road
createRoad location1 location2 =
  { location1 = location1
  , location2 = location2
  }


createLocation : Float -> Float -> Location
createLocation lat lng =
  { lat = lat
  , lng = lng
  }


createArea : List Location -> Area
createArea edges =
  { edges = edges
  }


distanceTo : Location -> Location -> Float
distanceTo location1 location2 =
  Native.Geofunctions.distanceTo location1.lat location1.lng location2.lat location2.lng


distanceInKm : Float -> Float
distanceInKm distance =
  (toFloat (round (distance / 100.0))) / 10.0


isNearby : Location -> Location -> Float -> Bool
isNearby location1 location2 radius =
  isInRange (distanceTo location1 location2) radius


isInRange : Float -> Float -> Bool
isInRange distance radius =
  distance <= radius


computeNewPoint : Location -> Float -> Float -> Location
computeNewPoint location distanceKm bearing =
  Native.Geofunctions.computeNewPoint location.lat location.lng distanceKm bearing


computePointOnLine: Location -> Location -> Float -> Location
computePointOnLine loc1 loc2 speed =
  Native.Geofunctions.computePointOnLine loc1.lat loc1.lng loc2.lat loc2.lng speed


-- https://en.wikipedia.org/wiki/Even%E2%80%93odd_rule
isInArea : Location -> Area -> Bool
isInArea location area =
  let
    last = List.head
            <| List.drop
            ((List.length area.edges) - 1)
            (area.edges)
    (x, y) = (location.lat, location.lng)
    i = 0
    j = List.length area.edges
    c = False
    f i (j, c) =
      if ((i.lng > y) /= (j.lng > y)) &&
         (x < (j.lat - i.lat) * (y - i.lng) /
         (j.lng - i.lng) + i.lat) then
         (i, not c)
       else
         (i, c)

    takeC (_, c) = c
  in
    case last of
      Just l ->
        takeC
          <| List.foldl f (l, c) area.edges
      Nothing ->
        False


centerOfArea : Area -> Location
centerOfArea area =
  Location (centerX area) (centerY area)


centerX : Area -> Float
centerX area =
  let
    sA = signedArea area
    sum a = case a of
      a :: b :: t ->
        (a.lat + b.lat) * (a.lat * b.lng - b.lat * a.lng) + (sum (b :: t))
      _ ->
        0
  in
    (1.0 / (6.0 * sA)) * (sum area.edges)


centerY : Area -> Float
centerY area =
  let
    sA = signedArea area
    sum a = case a of
      a :: b :: t ->
        (a.lng + b.lng) * (a.lat * b.lng - b.lat * a.lng) + (sum (b :: t))
      _ ->
        0
  in
    (1.0 / (6.0 * sA)) * (sum area.edges)


signedArea : Area -> Float
signedArea area =
  let
    sum a = case a of
      a :: b :: t ->
        (a.lat * b.lng - b.lat * a.lng) + (sum (b :: t))
      _ ->
        0
  in
    0.5 * (sum area.edges)
