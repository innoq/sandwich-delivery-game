module App.TrafficLight exposing (..)

import App.Biker exposing (Biker)
import App.Map exposing (Area, Id, Location, Road, Speed, centerOfArea, createArea, createLocation, distanceTo, isNearby, isInArea)
import App.Player exposing (Player)
import Dict exposing (Dict, empty)
import GeoJson exposing (GeoJson, GeoJsonObject(..))
import Json.Decode exposing (Value, decodeValue, dict, field, string)
import List.Extra exposing (..)
import Maybe.Extra exposing (..)
import Time exposing (Time)



type alias TrafficLight =
  { id : Id
  , intersection : Area
  , trafficFlows : List TrafficFlow
  , stateChangeTime : Time
  , nextStateChangeTime : Time
  , nextStateKeepTime : Time
  , nextFlow : Maybe TrafficFlow
  , nextFlowWeight : Maybe Int
  }


type alias TrafficFlow =
  { id : Id
  , on : Bool
  , streets : List Street
  , lights : List Location
  , lightStates :
    { red : Light
    , yellow : Light
    , green : Light
    }
  }


type alias Street =
  { waitingArea : Area
  }


type alias Light =
  Bool


type FlowState = Red | Yellow | Green



createTrafficLight : Id -> Area -> List TrafficFlow -> TrafficLight
createTrafficLight id intersection trafficFlows =
  let
    nF = case trafficFlows of
      a :: _ -> Just a
      _ -> Nothing
  in
    { id = id
    , intersection = intersection
    , trafficFlows = trafficFlows
    , stateChangeTime = 0
    , nextStateChangeTime = 0
    , nextStateKeepTime = 0
    , nextFlow = nF
    , nextFlowWeight = Nothing
    }


createTrafficFlow : Id -> List Street -> List Location -> TrafficFlow
createTrafficFlow id streets lights =
  { id = id
  , on = False
  , streets = streets
  , lights = lights
  , lightStates =
    { red = createLight
    , yellow = createLight
    , green = createLight
    }
  }


createStreet : Area -> Street
createStreet area =
  { waitingArea = area
  }


createLight : Light
createLight =
  False


type alias Vehicle =
  { id : Id
  , location : Location
  , lastLocation : Location
  , speed : Speed
  }


type alias TrafficLightConfig =
  { maxFlowTime : Time
  , minFlowTime : Time
  , yellowRedChangeTime : Time
  , defaultGreenPhaseTime : Time
  }


updateTrafficLight : TrafficLightConfig -> Time -> TrafficLight -> Player -> List Biker -> TrafficLight
updateTrafficLight config time trafficLight player bikers =
  let
    center = centerOfArea trafficLight.intersection

    weight v = List.length v

    timeForPassing : Vehicle -> Time
    timeForPassing b =
      let
        d = distanceTo b.location center
      in
        d / b.speed * 2

    times v =
      List.map timeForPassing v

    vehicles : List Vehicle
    vehicles =
      (Vehicle "player" player.location player.lastLocation player.speed) ::
      (List.map (\n -> Vehicle (toString n.id) n.location n.lastLocation n.speed) bikers)

    bf : Dict Id (List Vehicle)
    bf = vehiclesPerFlow vehicles trafficLight

    preferredFlowIdAndWeight : Maybe (String, Int)
    preferredFlowIdAndWeight =
      Dict.foldl
        ( \k v max ->
          case max of
            Nothing ->
              Just (k, weight v)

            Just (maxW, maxV) ->
              let
                wV = weight v
              in
                if wV > maxV
                  then Just (k, wV)
                else max
        )
        Nothing
        bf

    preferredFlow : Maybe TrafficFlow
    preferredFlow =
      case preferredFlowIdAndWeight of
        Just (f, _) ->
          find (\n -> n.id == f) trafficLight.trafficFlows
        Nothing ->
          Nothing

    preferredFlowBikers =
      case preferredFlowIdAndWeight of
        Just (f, _) ->
          Dict.get f bf
        Nothing ->
          Nothing

    currentFlow : Maybe TrafficFlow
    currentFlow = find (\n -> n.on) trafficLight.trafficFlows

    (nextStateChangeTime, nextStateKeepTime) =
      case (preferredFlow, preferredFlowBikers) of
        (Just f, Just b) ->
          let
            t = times b
            min = Maybe.withDefault 0 (List.minimum t)
            max = Maybe.withDefault 0 (List.maximum t)
          in
          (time + config.yellowRedChangeTime, config.defaultGreenPhaseTime)
        _ ->
          (time + config.defaultGreenPhaseTime, config.defaultGreenPhaseTime)

    isBetter = case (preferredFlowIdAndWeight, trafficLight.nextFlowWeight) of
      (Just (_, w), Just nw) ->
        if w > nw then True else False
      (Just _, _) ->
        True
      _ -> False

    maxGreenPhaseReached = trafficLight.stateChangeTime + config.maxFlowTime < time
    inYellowRedPhase = trafficLight.nextStateChangeTime - config.yellowRedChangeTime < time

    updatedTrafficLight =
      if not maxGreenPhaseReached && isJust preferredFlow && isBetter then
        if preferredFlow == currentFlow then
          if not inYellowRedPhase then
            { trafficLight
            | nextStateChangeTime = time + config.defaultGreenPhaseTime
            , nextStateKeepTime = config.defaultGreenPhaseTime
            }
          else
            trafficLight
        else
          { trafficLight
          | nextStateChangeTime = max (trafficLight.stateChangeTime + config.yellowRedChangeTime + config.minFlowTime) nextStateChangeTime
          , nextStateKeepTime = nextStateKeepTime
          , nextFlow = preferredFlow
          , nextFlowWeight = Maybe.map (\(_, w) -> w) preferredFlowIdAndWeight
          }
      else
        trafficLight


  in
    if updatedTrafficLight.nextStateChangeTime < time then
      let
        nF = activateFlow updatedTrafficLight.nextFlow updatedTrafficLight.trafficFlows
      in
        { updatedTrafficLight
        | trafficFlows = nF
        , stateChangeTime = time
        , nextStateChangeTime = time + updatedTrafficLight.nextStateKeepTime
        , nextStateKeepTime = config.defaultGreenPhaseTime
        , nextFlow = nextFlow nF
        , nextFlowWeight = Nothing
        }
    else if updatedTrafficLight.nextStateChangeTime - config.yellowRedChangeTime < time then
      let
        turnOff f =
          if f.on then
            changeFlowState Yellow f
          else
            f
      in
        { updatedTrafficLight
        | trafficFlows = List.map turnOff updatedTrafficLight.trafficFlows
        }
    else
      updatedTrafficLight


currentFlow : List TrafficFlow -> Maybe TrafficFlow
currentFlow f =
  find (\n -> n.on) f


nextFlow : List TrafficFlow -> Maybe TrafficFlow
nextFlow l =
  case l of
    a :: b :: t ->
      if b.on then
         Just a
       else
         nextFlow (b :: t)
    a :: [] ->
      Just a
    [] ->
      Nothing


activateFlow : Maybe TrafficFlow -> List TrafficFlow -> List TrafficFlow
activateFlow f l =
  case f of
    Just f ->
      List.map (\n -> if n == f then (changeFlowState Green n) else (changeFlowState Red n)) l
    Nothing ->
      List.map (changeFlowState Red) l


vehiclesPerFlow : List Vehicle -> TrafficLight -> Dict Id (List Vehicle)
vehiclesPerFlow vehicles trafficLight =
  let
    onFlow : Vehicle -> TrafficFlow -> Bool
    onFlow b f = onTrafficFlow b.location f

    getFlow : Vehicle -> TrafficLight -> Maybe TrafficFlow
    getFlow b t =
      if movingTowardsTrafficLight b.lastLocation b.location t then
        List.head
          <| List.filter (onFlow b) t.trafficFlows
      else
        Nothing

    addToList b v =
      case v of
        Just l ->
          Just (b::l)
        Nothing ->
          Just [b]

    addToFlow : Vehicle -> Maybe TrafficFlow -> Dict Id (List Vehicle) -> Dict Id (List Vehicle)
    addToFlow b f d =
      case f of
        Just f ->
          Dict.update f.id (addToList b) d
        Nothing ->
          d

  in
    List.foldl
      (\b d -> addToFlow b (getFlow b trafficLight) d)
      Dict.empty
      vehicles


changeFlowState : FlowState -> TrafficFlow -> TrafficFlow
changeFlowState state trafficFlow =
  case state of
    Red ->
      { trafficFlow | on = False, lightStates = { red = True, yellow = False, green = False } }
    Yellow ->
      { trafficFlow | on = True, lightStates = { red = False, yellow = True, green = False } }
    Green ->
      { trafficFlow | on = True, lightStates = { red = False, yellow = False, green = True } }


onRedLight : Location -> Location -> TrafficLight -> Bool
onRedLight lastLocation location trafficLight =
  if movingTowardsTrafficLight lastLocation location trafficLight && onIntersection location trafficLight then
    List.any (\n -> (not n.on) && onTrafficFlow location n) trafficLight.trafficFlows
  else
    False


movingTowardsRedLight : Location -> Location -> TrafficLight -> Bool
movingTowardsRedLight lastLocation location trafficLight =
  if movingTowardsTrafficLight lastLocation location trafficLight then
    List.any (\n -> (not n.on) && onTrafficFlow location n) trafficLight.trafficFlows
  else
    False


movingTowardsTrafficLight : Location -> Location -> TrafficLight -> Bool
movingTowardsTrafficLight lastLocation location trafficLight =
  let
    center = centerOfArea trafficLight.intersection
  in
    movingTowards lastLocation location center


movingTowards : Location -> Location -> Location -> Bool
movingTowards lastLocation location target =
  distanceTo location target < distanceTo lastLocation target


onIntersection : Location -> TrafficLight -> Bool
onIntersection location trafficLight =
  isInArea location trafficLight.intersection


onTrafficFlow : Location -> TrafficFlow -> Bool
onTrafficFlow location trafficFlow =
  List.any (onStreet location) trafficFlow.streets


onStreet : Location -> Street -> Bool
onStreet location street =
  isInArea location street.waitingArea


-- parsing

type TrafficLightFeature = A Area | L Location


readTrafficLights : GeoJson -> Maybe (List TrafficLight)
readTrafficLights geoJson =
  let
    updateDict : String -> String -> TrafficLightFeature -> Dict String (Dict String (List TrafficLightFeature)) -> Dict String (Dict String (List TrafficLightFeature))
    updateDict t f a d =
      Dict.update t
        (\n -> case n of
          Just v ->
            Just <| updateFlowDict f a v
          Nothing ->
            Just <| updateFlowDict f a Dict.empty
        )
        d

    updateFlowDict : String -> TrafficLightFeature -> Dict String (List TrafficLightFeature) -> Dict String (List TrafficLightFeature)
    updateFlowDict f a d =
      Dict.update f
        (\n -> case n of
          Just v -> Just (a :: v)
          Nothing -> Just [a]
        )
        d

    toStreetsAndLights l =
      List.foldl (\n (s, l) -> case n of
        A a ->
          ((createStreet a) :: s, l)
        L a ->
          (s, a :: l))
        ([], [])
        l

    toTrafficLights : Dict String (Dict String (List TrafficLightFeature)) -> List TrafficLight
    toTrafficLights d =
      List.map
        (\(k, n) -> toTrafficLight k n)
        (Dict.toList d)
      |> List.filterMap (\n -> n)


    toTrafficLight : Id -> Dict String (List TrafficLightFeature) -> Maybe TrafficLight
    toTrafficLight id dict =
      let
        i = Dict.get "intersection" dict
        intersection = case i of
          Just [A x] ->
            Just x
          _ ->
            Nothing

        d = Dict.remove "intersection" dict

        createFlow k v =
          let
            (s, l) = toStreetsAndLights v
          in
            createTrafficFlow k s l

        flows : List TrafficFlow
        flows = Dict.values <| Dict.map
              (\k v -> createFlow k v)
              d

      in
        case intersection of
          Just x ->
            Just <| createTrafficLight id x flows
          _ ->
            Nothing
  in
  case geoJson of
    (FeatureCollection features, _) ->
      Just (toTrafficLights
        ( List.foldl
              (\f d ->
                List.foldl
                  (\(t, f, a) d -> updateDict t f a d)
                  d
                  (readTrafficLightFeature f)
              )
              Dict.empty
              features
        )
      )
    _ ->
      Nothing


readTrafficLightFeature : GeoJson.FeatureObject -> List (String, String, TrafficLightFeature)
readTrafficLightFeature feature =
  let
    decode p =
      ( decodeValue trafficLightDecoder p
      , Result.map (\n ->
          List.map String.trim
            <| String.split "," n
        ) (decodeValue trafficFlowDecoder p)
      )
    toFeature geometry =
      case geometry of
        Just (GeoJson.Polygon [l]) ->
          Just
            <| A
            <| createArea
            <| List.map (\(lng,lat, _) -> createLocation lat lng) l
        Just (GeoJson.Point (lng, lat, _)) ->
          Just
            <| L
            <| createLocation lat lng
        _ ->
          Nothing
  in
    case (decode feature.properties, toFeature feature.geometry) of
      ((Ok t, Ok fs), Just g) ->
        List.map (\f -> (t, f, g)) fs
      _ ->
        []


trafficLightDecoder : Json.Decode.Decoder String
trafficLightDecoder =
  field "trafficLight" string


trafficFlowDecoder : Json.Decode.Decoder String
trafficFlowDecoder =
  field "trafficFlow" string
