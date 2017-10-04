port module Main exposing (..)

import App.Biker exposing (Biker, createBiker, updateBiker)
import App.GroupOrder exposing (GroupOrder, GroupOrderLocation, readGroupOrderLocations, generateGroupOrder, updateGroupOrders, updateSelectedGroupOrder, up, down, select)
import App.Map exposing (Id, Location, Route, Speed, createLocation, createRoad, distanceInKm, distanceTo, isNearby, isInRange)
import App.Player exposing (Player, PlayerMovement, createPlayer, updatePlayerMovement, updatePlayerPosition, updatePlayerDistanceToRoads)
import App.TrafficLight exposing (TrafficLight, TrafficLightConfig, movingTowardsRedLight, onRedLight, readTrafficLights, updateTrafficLight)
import Color exposing (Color, green, red, orange, rgb, rgba)
import Cons
import Ease
import GeoJson exposing (GeoJson, GeoJsonObject(..), decoder)
import Graphics.Render exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error)
import Maybe.Extra exposing (isJust)
import Random exposing (generate, int)
import Time exposing (Time, millisecond, now)
import Json.Encode
import Json.Decode
import List.Extra



-- MAIN


main : Program Config Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Config =
  { control : String
  , interval : Time
  , rideZoomLevel : Int
  , focusedSidebarZoomLevel : Int
  , playerStartLocation : Location
  , playerStartBearing : Float
  , maxGroupOrders : Int
  , minGroupOrderCount : Int
  , maxGroupOrderCount : Int
  , roadsGeoJson : String
  , trafficLightsGeoJson : String
  , groupOrderLocationsGeoJson : String
  , bikersCount : Int
  , scorePointsPerOrder : Int
  , scorePointsPerRedLight : Int
  , scorePointsPerOffRoad : Int
  , canRotateWithoutSpeed : Bool
  , outOfStreetWarningDistance : Float
  , trafficLightConfig : TrafficLightConfig
  , tellWarnings : Bool
  , timerStart : Int
  , timerNotifications : List Int
  , showControls : Bool
  , scoreServerUrl : String
  }



-- MODEL


type State = InitializingMap | LoadingData | Ready | Running | Paused | Finished


type alias Model =
  { time : Time
  , state : State
  , mapInitialized : Bool
  , roads : Maybe (Result Error GeoJson)
  , trafficLights : Maybe (List TrafficLight)
  , bikers : List Biker
  , bikerCounter: Int
  , groupOrderLocations : Maybe (List GroupOrderLocation)
  , groupOrders : List GroupOrder
  , groupOrderCounter : Int
  , player : Player
  , onRedLight : Bool
  , movingTowardsRedLight : Bool
  , score : Int
  , notifications : List Notification
  , focusSidebar : Bool
  , timer : Maybe Int
  , errors : List String
  , config : Config
  , timerNotificationColor : Color
  , positiveScoreColor : Color
  , negativeScoreColor : Color
  , warningNotificationColor : Color
  , playerId : Maybe Int
  , scores : Maybe (List ScoreEntry)
  }


type alias Notification =
  { text : NotificationType
  , group : String
  , color : Color
  , started : Time
  , duration : Time
  }

type NotificationType = S String | M (Model -> String)


type alias ScoreEntry =
  { position: Int
  , playerId : Int
  , score : Int
  }


init : Config -> ( Model, Cmd Msg )
init config =
  { time = 0
  , state = InitializingMap
  , mapInitialized = False
  , roads = Nothing
  , trafficLights = Nothing
  , bikers = []
  , bikerCounter = 0
  , groupOrderLocations = Nothing
  , groupOrders = []
  , groupOrderCounter = 0
  , player = initPlayer config.playerStartLocation config.playerStartBearing
  , onRedLight = False
  , movingTowardsRedLight = False
  , score = 0
  , notifications = []
  , focusSidebar = True
  , timer = Nothing
  , errors = []
  , config = config
  , timerNotificationColor = rgb 153 204 255
  , positiveScoreColor = rgb 144 238 144
  , negativeScoreColor = red
  , warningNotificationColor = rgb 229 103 23
  , playerId = Nothing
  , scores = Nothing
  } ! [ initializeMap (), getScores config ]


initPlayer : Location -> Float -> Player
initPlayer location bearing =
  createPlayer location bearing




-- UPDATE


type Msg
  = Restart
  | OnMapInitialized
  | Tick Time
  | RoadsResult (Result Http.Error GeoJson)
  | TrafficLightsResult (Result Http.Error GeoJson)
  | GroupOrderLocationsResult (Result Http.Error GeoJson)
  | GenerateGroupOrder (Int, Int)
  | UpdatePlayerMovement PlayerMovement
  | ControllerInput String
  | UpdatePlayerDistanceToRoads Float
  | UpdateScore Int
  | SelectGroupOrder Id
  | CreateBiker (List Location, Speed)
  | SendPlayerScoreResult (Result Http.Error Int)
  | ScoreScreenResults (Result Http.Error (List ScoreEntry))



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Restart ->
      model ! [ restart () ]

    OnMapInitialized ->
      { model | mapInitialized = True } ! [ Cmd.none ]

    Tick newTime ->
      handleTick model newTime

    RoadsResult result ->
      handleRoadsResult model result

    TrafficLightsResult result ->
      handleTrafficLightsResult model result

    GroupOrderLocationsResult result ->
      handleGroupOrderLocationsResult model result

    GenerateGroupOrder generationData ->
      handleGenerateGroupOrder model generationData

    UpdatePlayerMovement playerMovement ->
      handleUpdatePlayerMovement model playerMovement

    ControllerInput input ->
      handleControllerInput model input

    UpdatePlayerDistanceToRoads input ->
      handleUpdatePlayerDistanceToRoads model input

    UpdateScore score ->
      handleUpdateScore model score

    SelectGroupOrder id ->
      handleSelectGroupOrder model id

    CreateBiker (locations, speed) ->
      handleCreateBiker model locations speed

    SendPlayerScoreResult result ->
      case result of
        Ok playerId -> { model | playerId = Just playerId } ! [ getScores model.config ]
        Err error -> { model | errors = (toString error) :: model.errors } ! [ getScores model.config ]

    ScoreScreenResults result ->
      case result of
        Ok scores -> { model |  scores = Just scores } ! [ Cmd.none ]
        Err error -> { model | errors = (toString error) :: model.errors } ! [ Cmd.none ]


handleTick : Model -> Time -> (Model, Cmd Msg)
handleTick model newTime =
  let
    (m, cmds) = case model.state of
      Ready ->
        model ! [ Cmd.none ]
      Running ->
        run model newTime
      Paused ->
        model ! [ Cmd.none ]
      Finished ->
        model ! [ Cmd.none ]
      _ ->
        checkState model
  in
    ({ m | time = newTime }, cmds)


checkState : Model -> (Model, Cmd Msg)
checkState model =
  case model.state of
    InitializingMap ->
      if model.mapInitialized then
        let
          bikersCount = model.config.bikersCount
          trafficLightsGeoJson = model.config.trafficLightsGeoJson
          groupOrderLocationsGeoJson = model.config.groupOrderLocationsGeoJson
        in
          { model | state = LoadingData } !
            [ loadBikers bikersCount
            , loadTrafficLights trafficLightsGeoJson
            , loadGroupOrderLocations groupOrderLocationsGeoJson
            ]
      else
        model ! [ Cmd.none ]

    LoadingData ->
      if isJust model.trafficLights && isJust model.groupOrderLocations && model.config.bikersCount <= List.length model.bikers then
        { model | state = Ready } ! [ Cmd.none ]
      else
        model ! [ Cmd.none ]

    _ ->
      model ! [ Cmd.none ]


run : Model -> Time -> (Model, Cmd Msg)
run model newTime =
  let
    (timer, timerNotification) =
      let
        inSeconds t = Basics.round <| Time.inSeconds t
        newTimeInSeconds = inSeconds newTime

      in
      case model.timer of
        Nothing ->
          ( Just <| model.config.timerStart
          , [ createNotification (M (\n -> "Start: " ++ (timerToStr n.timer))) "time" model.timerNotificationColor newTime 6000 ]
          )
        Just t ->
          let
            nt = t - (newTimeInSeconds - (inSeconds model.time))
            notification =
              if nt /= t && List.any (\n -> n == nt) model.config.timerNotifications then
                [ createNotification (M (\n -> timerToStr n.timer)) "time" model.timerNotificationColor newTime 6000 ]
              else
                []
          in
            ( Just nt
            , notification
            )

    (newTrafficLights, trafficLightsUpdateCmds) = updateTrafficLights model newTime

    (newGroupOrders, groupOrderUpdateCmds, groupOrderScore) = updateGroupOrders model.groupOrders model.player newTime 1000 updateGroupOrder
    groupOrderScorePoints = groupOrderScore * model.config.scorePointsPerOrder

    generateGroupOrderCmd =
      case model.groupOrderLocations of
        Just l ->
          if List.length newGroupOrders < model.config.maxGroupOrders then
            generateNewGroupOrder (List.length l) (model.config.minGroupOrderCount) (model.config.maxGroupOrderCount)
          else
            Cmd.none
        Nothing ->
          Cmd.none

    player = updatePlayerPosition model.player model.config.canRotateWithoutSpeed model.time newTime
    updatePlayerCmd =
      if player /= model.player then
        updatePlayer player
      else
        Cmd.none

    updatePlayerGroupOrderDirectionsCmd =
      if player /= model.player || newGroupOrders /= model.groupOrders then
        updatePlayerGroupOrderDirections (player, newGroupOrders)
      else Cmd.none

    isOnRedLight = List.any (onRedLight player.lastLocation player.location) (Maybe.withDefault [] newTrafficLights)
    isMovingTowardsRedLight = List.any (movingTowardsRedLight player.lastLocation player.location) (Maybe.withDefault [] newTrafficLights)

    redLightScorePoints = if isOnRedLight && not model.onRedLight then model.config.scorePointsPerRedLight else 0

    newBikers = List.filterMap (\biker ->
          let
            updated = updateBiker biker
          in
            Maybe.map (\n ->
              if List.any (onRedLight n.lastLocation n.location) (Maybe.withDefault [] newTrafficLights) then
                biker
              else
                n
            ) updated
        ) model.bikers
    newBikersCmds = List.repeat (model.config.bikersCount - List.length newBikers) (createBikerRoute ())

    scoredNotification : List Notification
    scoredNotification =
      if groupOrderScorePoints /= 0 then [createNotification (S ("+ " ++ (toString groupOrderScorePoints) ++ " CHF")) "score" model.positiveScoreColor newTime 4000] else []

    notifyMovingTowardsRedLight =
      model.movingTowardsRedLight == False && isMovingTowardsRedLight

    redLightNotification : List Notification
    redLightNotification =
      if notifyMovingTowardsRedLight then [createNotification (S "Red Light Ahead !") "red light warning" model.warningNotificationColor newTime 4000] else []

    redLightPenaltyNotification : List Notification
    redLightPenaltyNotification =
      if redLightScorePoints /= 0 then [createNotification (S ("- " ++ (toString (abs redLightScorePoints)) ++ " CHF")) "red light penalty" model.negativeScoreColor newTime 4000] else []

    outOfSteetScorePoints =
      if player.outOfStreet && not model.player.outOfStreet then
         model.config.scorePointsPerOffRoad
      else 0

    notifyOutOfStreet =
      player.distanceToRoads > model.config.outOfStreetWarningDistance &&
      (not (List.any (\n -> n.group == "out of street warning") model.notifications))


    outOfStreetNotification : List Notification
    outOfStreetNotification =
      if notifyOutOfStreet then [createNotification (S "Off Road !") "out of street warning" model.warningNotificationColor newTime 4000] else []

    outOfStreetPenaltyNotification : List Notification
    outOfStreetPenaltyNotification =
      if outOfSteetScorePoints /= 0 then [createNotification (S ("- " ++ (toString (abs outOfSteetScorePoints)) ++ " CHF")) "out of street penalty" model.negativeScoreColor newTime 4000] else []

    notifications =
      timerNotification ++
      scoredNotification ++
      redLightNotification ++
      redLightPenaltyNotification ++
      outOfStreetNotification ++
      outOfStreetPenaltyNotification ++
      (List.filter (\n -> n.started + n.duration > newTime) model.notifications)

    tellWarnings =
      if model.config.tellWarnings then
        if notifyMovingTowardsRedLight then
          tell "Red light ahead"
        else if notifyOutOfStreet then
          tell "Off Road"
        else
          Cmd.none
      else
        Cmd.none

    score = Basics.max 0 <| model.score +
      groupOrderScorePoints +
      redLightScorePoints +
      outOfSteetScorePoints

  in
    if isJust timer && Maybe.withDefault 0 timer > 0 then
            { model
            | time = newTime
            , timer = timer
            , trafficLights = newTrafficLights
            , groupOrders = newGroupOrders
            , player = player
            , onRedLight = isOnRedLight
            , movingTowardsRedLight = isMovingTowardsRedLight
            , score = score
            , notifications = notifications
            , bikers = newBikers
            } ! ( [ updatePlayerCmd
                               , updatePlayerGroupOrderDirectionsCmd
                               , generateGroupOrderCmd
                               , updateBikersView (bikersToJs newBikers)
                               , tellWarnings
                               ]
                             ++ trafficLightsUpdateCmds
                             ++ groupOrderUpdateCmds
                             ++ newBikersCmds
                             )
          else
            { model
            | state = Finished
            } ! [ sendPlayerScore model.config model.score ]


timerToStr : Maybe Int -> String
timerToStr timer =
  case timer of
    Nothing -> ""
    Just t ->
      let
        s = t % 60
        m = Basics.floor <| (toFloat t) / 60
      in
        (String.padLeft 2 '0' (toString m)) ++ ":" ++ (String.padLeft 2 '0' (toString s))


createNotification : NotificationType -> String -> Color -> Time -> Time -> Notification
createNotification text group color time duration =
  { text = text
  , group = group
  , color = color
  , started = time
  , duration = duration
  }


updateTrafficLights : Model -> Time -> (Maybe (List TrafficLight), List (Cmd msg))
updateTrafficLights model newTime =
  let
    check time trafficLight =
      let
        bikers = model.bikers
        c = updateTrafficLight model.config.trafficLightConfig time trafficLight model.player bikers
      in
        if c == trafficLight then (c, Cmd.none)
        else (c, updateTrafficLightView (c.id, Just c))
  in
    case model.trafficLights of
      Just t ->
        -- FIXME better way to add Just ?
        case List.unzip (List.map (\t -> check newTime t) t) of
          (a, b) -> (Just a, b)
      Nothing ->
        (Nothing, [])


handleRoadsResult : Model -> (Result Http.Error GeoJson) -> (Model, Cmd Msg)
handleRoadsResult model result =
  { model | roads = Just result } ! [ Cmd.none ]


handleTrafficLightsResult : Model -> (Result Http.Error GeoJson) -> (Model, Cmd Msg)
handleTrafficLightsResult model result =
  case result of
    Ok geoJson ->
      let
        parsed = readTrafficLights geoJson
      in
        case parsed of
          Just v ->
            { model | trafficLights = Just v
            } ! (List.map (\n -> updateTrafficLightView (n.id, Just n)) v)
          Nothing ->
            { model | errors = "Parsing group order GeoJson failed" :: model.errors } ! [ Cmd.none ]

    Err error ->
      { model | errors = (toString error) :: model.errors
      } ! [ Cmd.none ]


handleGroupOrderLocationsResult : Model -> (Result Http.Error GeoJson) -> (Model, Cmd Msg)
handleGroupOrderLocationsResult model result =
  case result of
    Ok geoJson ->
      let
        parsed = readGroupOrderLocations geoJson
      in
        case parsed of
          Just v ->
            { model | groupOrderLocations = Just v
            } ! [ (generateNewGroupOrder (List.length v) (model.config.minGroupOrderCount) (model.config.maxGroupOrderCount)) ]
          Nothing ->
            { model | errors = "Parsing group order GeoJson failed" :: model.errors } ! [ Cmd.none ]

    Err error ->
      { model | groupOrderLocations = Nothing
              , errors = (toString error) :: model.errors
      } ! [ Cmd.none ]


handleGenerateGroupOrder : Model -> (Int, Int) -> (Model, Cmd Msg)
handleGenerateGroupOrder model (locationIndex, count) =
  case model.groupOrderLocations of
    Just v ->
      let
        maxOrders = model.config.maxGroupOrders
        min = model.config.minGroupOrderCount
        max = model.config.maxGroupOrderCount
        groupOrderCounter = model.groupOrderCounter + 1
        id = toString groupOrderCounter
        groupOrder = generateGroupOrder id v locationIndex count

      in
        case groupOrder of
          Just g ->
            let
              invalid =
                distanceTo g.location model.player.location <= g.radius ||
                List.any
                  (\n -> n.location == g.location || distanceTo n.location model.player.location <= n.radius) model.groupOrders

              newModel = { model | groupOrders = g :: model.groupOrders
                    , groupOrderCounter = groupOrderCounter
                    }
              generateMoreGroupOrders =
                if invalid || (List.length model.groupOrders) + 1 < maxOrders then
                  (generateNewGroupOrder (List.length v) min max)
                else
                  Cmd.none
            in
               if not invalid then
               newModel ! [ (updateGroupOrder (id, Just g))
                          , updatePlayerGroupOrderDirections (newModel.player, newModel.groupOrders)
                          , generateMoreGroupOrders
                          ]
               else
                model ! [ generateNewGroupOrder (List.length v) min max]
          Nothing -> ( model, Cmd.none )

    Nothing ->
      { model | errors = "Failed to generate group orders with out group order locations" :: model.errors
      } ! [ Cmd.none ]


generateNewGroupOrder : Int -> Int -> Int -> Cmd Msg
generateNewGroupOrder maxId minCount maxCount =
  Random.generate GenerateGroupOrder (Random.pair (Random.int 0 (maxId - 1)) (Random.int minCount maxCount))


handleUpdatePlayerMovement : Model -> PlayerMovement -> (Model, Cmd Msg)
handleUpdatePlayerMovement model playerMovement =
  { model | player = updatePlayerMovement model.player playerMovement
  } ! [ Cmd.none ]


handleControllerInput : Model -> String -> (Model, Cmd Msg)
handleControllerInput model input =
  case model.state of
    Ready ->
      case input of
        "select" -> { model | state = Running } ! [ initializeControls ()
                                                  , updatePlayer model.player ]
        _ -> model ! [ Cmd.none ]
    Running ->
      handleControllerInputRunning model input
    Paused ->
      case input of
        "select" -> model ! [ restart () ]
        "right" -> { model | state = Running } ! [ Cmd.none ]
        _ -> model ! [ Cmd.none ]
    Finished ->
      case input of
        "select" -> model ! [ restart () ]
        _ -> model ! [ Cmd.none ]
    _ ->
      model ! [ Cmd.none ]


handleUpdatePlayerDistanceToRoads : Model -> Float -> (Model, Cmd Msg)
handleUpdatePlayerDistanceToRoads model input =
  { model | player = updatePlayerDistanceToRoads model.player input
  } ! [ Cmd.none ]


handleControllerInputRunning : Model -> String -> (Model, Cmd Msg)
handleControllerInputRunning model input =
  let
    groupOrderCmd c =
      updateSelectedGroupOrder model.groupOrders c updateGroupOrder

    (groupOrders, groupOrderCmds) = case input of
      "up" ->
        groupOrderCmd up
      "down" ->
        groupOrderCmd down
      "select" ->
        groupOrderCmd App.GroupOrder.select
      _ ->
        (model.groupOrders, [])

    getZoomLevel focusSidebar =
      if focusSidebar then
        model.config.focusedSidebarZoomLevel
      else
        model.config.rideZoomLevel

    updateSidebarFocus focusSidebar =
      if focusSidebar /= model.focusSidebar then
        (focusSidebar, [ updateZoomLevel (getZoomLevel focusSidebar) ])
      else
        (focusSidebar, [ Cmd.none ])

    (focusSidebar, focusSidebarCmds) =
      case input of
        "left" ->
          updateSidebarFocus True
        "right" ->
          updateSidebarFocus False
        "up" ->
          updateSidebarFocus True
        "down" ->
          updateSidebarFocus True
        "select" ->
          updateSidebarFocus False
        _ ->
          updateSidebarFocus model.focusSidebar

  in
    case (input, model.focusSidebar) of
      ("left", True) ->
        { model | state = Paused } ! [ Cmd.none ]
      _ ->
        { model | groupOrders = groupOrders
                , focusSidebar = focusSidebar
        } ! (groupOrderCmds ++ focusSidebarCmds)


handleUpdateScore : Model -> Int -> (Model, Cmd Msg)
handleUpdateScore model score =
  { model | score = model.score + score } ! [ Cmd.none ]


handleSelectGroupOrder : Model -> Id -> (Model, Cmd Msg)
handleSelectGroupOrder model id =
  let
    (newGroupOrders, cmds) = List.foldr
      (\n (l, c) ->
        case (n.id == id, n.selected) of
          (True, False) ->
            let
              new = { n | selected = True }
            in
              (new :: l, (updateGroupOrder (n.id, Just new)) :: c)

          (True, True) ->
            (n :: l, c)

          (False, True) ->
            let
              new = { n | selected = False }
            in
              (new :: l, (updateGroupOrder (n.id, Just new)) :: c)

          (False, False) ->
            (n :: l, c)
      )
      ([], [])
      model.groupOrders
  in
  { model | groupOrders = newGroupOrders } ! cmds


handleCreateBiker: Model -> List Location -> Speed -> (Model, Cmd Msg)
handleCreateBiker model locations speed =
  case locations of
    h :: t ->
      let
        bikerCounter = (model.bikerCounter + 1)
        route = Cons.cons h t
        biker = createBiker bikerCounter h route speed
        moreBikerCmd =
          if bikerCounter < model.config.bikersCount then
            createBikerRoute ()
          else
            Cmd.none
      in
        { model |
            bikerCounter = bikerCounter
            , bikers = biker :: model.bikers
        } ! [ moreBikerCmd ]
    _ ->
      model ! [ createBikerRoute () ]


loadRoads : String -> Cmd Msg
loadRoads url =
  Http.send RoadsResult
    <| Http.get url decoder


loadBikers : Int -> Cmd Msg
loadBikers count =
  createBikerRoute ()


loadTrafficLights : String -> Cmd Msg
loadTrafficLights url =
  Http.send TrafficLightsResult
    <| Http.get url decoder


loadGroupOrderLocations : String -> Cmd Msg
loadGroupOrderLocations url =
  Http.send GroupOrderLocationsResult
    <| Http.get url decoder


sendPlayerScore : Config -> Int -> Cmd Msg
sendPlayerScore config score =
  let
    url = "https://" ++ config.scoreServerUrl ++ "/score"
    responseDecoder = Json.Decode.field "playerId" Json.Decode.int
    body = Json.Encode.object [ ("score", Json.Encode.int score) ] |> Http.jsonBody
    request = Http.post url body responseDecoder
  in
    Http.send SendPlayerScoreResult request

getScores : Config -> Cmd Msg
getScores config =
  let
    url = "https://" ++ config.scoreServerUrl ++ "/score"
    responseDecoder =
        Json.Decode.list (
          Json.Decode.map3 ScoreEntry
            (Json.Decode.field "position" Json.Decode.int)
            (Json.Decode.field "playerId" Json.Decode.int)
            (Json.Decode.field "score" Json.Decode.int)
        )
    decoder =  Json.Decode.list
  in
    Http.get url responseDecoder
      |> Http.send ScoreScreenResults




-- VIEW


view : Model -> Html Msg
view model =
  let
    map =
      div [ (Html.Attributes.id "mapid") ] []

    gameState =
      viewGameState model

    sidebar =
      viewSidebar model

    mapOverlay =
      div [ (Html.Attributes.id "map-overlay") ]
        <| ( gameState ++ sidebar ++ ( viewNotifications model ) )

  in
    div [ (Html.Attributes.id "elm-main") ]
      ( [ map
        , mapOverlay
        ]
      )


viewGameState : Model -> List (Html Msg)
viewGameState model =
  let
    h1Text = "The Sandwich Delivery Game"
    introText1 = """Welcome to our Sandwich Delivery Game.
                 In this game, it is your job to deliver sandwiches to small groups of people in the city of Basel."""
    rulesText1 = "You earn "
    earnAmount = "10 CHF"
    rulesText2 = " for each sandwich delivered but ride carefully: Running a "
    redLight = "Red Light"
    rulesText3 = " will cost you "
    redLightAmount = "60 CHF"
    rulesText4 = ". Driving "
    offRoads = "Off Roads"
    rulesText5 = " or not using bicycle lanes when they are present will cost you "
    offRoadAmount = "30 CHF"
    rulesText6 = "."
    timeLimitText1 = "You can take up to "
    roundDuration = "2 minutes"
    timeLimitText2 = """ for a delivery tour."""
    introText = p [ (Html.Attributes.id "game-intro-text") ] <|
      [ p [] [ Html.text introText1 ]
      , p [] [ Html.text rulesText1
             , Html.span [ (Html.Attributes.class "amount-earned") ] [Html.text earnAmount]
             , Html.text rulesText2
             , Html.span [ (Html.Attributes.class "warning") ] [Html.text redLight]
             , Html.text rulesText3
             , Html.span [ (Html.Attributes.class "amount-deducted") ] [Html.text redLightAmount]
             , Html.text rulesText4
             , Html.span [ (Html.Attributes.class "warning") ] [Html.text offRoads]
             , Html.text rulesText5
             , Html.span [ (Html.Attributes.class "amount-deducted") ] [Html.text offRoadAmount]
             , Html.text rulesText6
             ]
      , p [] [ Html.text timeLimitText1
             , Html.span [ (Html.Attributes.class "round-duration") ] [Html.text roundDuration]
             , Html.text timeLimitText2
             ]
      , p [] ( List.indexedMap (\i t -> if i % 2 == 0 then
                                          Html.text t
                                        else
                                          Html.span [ (Html.Attributes.class "navigation-input") ] [Html.text t]
                               )
                               (case model.config.control of
                                 "keyboard" ->
                                   ["Keys: "
                                   , "W" , ", " , "A" , ", " , "S" , ", ", "D"
                                   , " for Forward, Left, Break and Right. "
                                   , "Space"
                                   , " for Select. "
                                   , "H", " / ", "L"
                                   , " for showing/hiding sidebar. "
                                   , "J", " / ", "K"
                                   , " to move up down orders in sidebar."]
                                 _ ->
                                   ["Navigation: "
                                   , "Tilt mobile phone"
                                   , " for Forward, Left, Break and Right. "
                                   , "Tap"
                                   , " for Select. "
                                   , "Swipe left", " / ", "right"
                                   , " for showing/hiding orders sidebar. "
                                   , "Tap on order"
                                   , " to select an order in sidebar."]
                               )
             )
      ]
  in
    case model.state of
      Ready ->
        [ div [ (Html.Attributes.id "game-state") ]
          [ h2 [] <| [ Html.text h1Text ]
          , introText
          , p [] <| [ Html.text ("Press the 'select' button on your right to start.") ]
          ]
        ]
      Running ->
        []
      Paused ->
        [ div [ (Html.Attributes.id "game-state") ]
          [ h2 [] <| [ Html.text "Paused" ]
          , viewTopScores model
          , p [] <| [ Html.text ("Press 'right' to continue") ]
          , p [] <| [ Html.text ("Press 'select' to restart") ]
          ]
        ]
      Finished ->
        [ div [ (Html.Attributes.id "game-state") ]
          [ h2 [] <| [ Html.text "Finished !" ]
            , viewTopScores model
            , p [] <| [ Html.text ("Press 'select' to restart") ]
          ]
        ]
      _ ->
        [ div [ (Html.Attributes.id "game-state") ]
          [ h2 [] <| [ Html.text h1Text ]
          , introText
          , p [] <| [ Html.text ("Loading ...") ]
          ]
        ]


viewTopScores : Model -> Html Msg
viewTopScores model =
  let
    top = Maybe.map (List.take 5) model.scores
    playerId = Maybe.withDefault 0 model.playerId
    outOfTopScore = case model.scores of
        Nothing -> Nothing
        Just list -> List.drop 5 list |> List.Extra.find (\e -> e.playerId == playerId)

    toTableRow : ScoreEntry -> Html Msg
    toTableRow e = tr [Html.Attributes.class (if e.playerId == playerId then "highlighted" else "")] [
                       td [] [Html.text <| (toString e.position) ++ "."]
                     , td [] [Html.text <| "Player " ++ (toString e.playerId)]
                     , td [] [Html.text <| toString e.score]
                   ]

  in
    p [ (Html.Attributes.id "top-scores") ] <|
      case top of
        Nothing ->
            [Html.text ("Your score is " ++ (toString model.score))]
        Just list -> [table [] <|
            [tr [] [th [] [Html.text "No."],th [] [Html.text "Name"],th [] [Html.text "Score"]]]
            ++ (List.map toTableRow list)
            ++ (Maybe.map (\e -> [
                   tr [] [td [] [], td [] [Html.text "..."], td [] []],
                   toTableRow e
               ]) outOfTopScore |> Maybe.withDefault [])
          ]


viewNotifications : Model -> List (Html Msg)
viewNotifications model =
  let
    text n =
      case n.text of
        S s -> s
        M f -> f model

  in
    case model.state of
      Running ->
            [ div [ (Html.Attributes.id "notifications") ]
              [ svg 0 0 600 400
                  <| Graphics.Render.group <|
                    List.indexedMap (\i n -> viewNotification model.time (text n) i n) model.notifications
              ]
            ]
      _ -> []


viewNotification : Float -> String -> Int -> Notification -> Form Msg
viewNotification time text index n =
  let
    lifetime n = (n.started + n.duration - time) / n.duration

    fontSize n = lifetime n |> Ease.outExpo
                            |> (*) 4
                            |> Basics.round
                            |> (+) 12

    paddingTop i n = lifetime n |> Ease.inExpo
                                |> (*) 20
                                |> (-) ((toFloat i) * 20)
                                |> (-) 270
  in
    position (220, paddingTop index n)
             <| rightJustified
             <| fontFamily "Arial"
             <| fontColor n.color
             <| bold
             <| Graphics.Render.text (fontSize n) text

viewSidebar : Model -> List (Html Msg)
viewSidebar model =
  case model.state of
    Running ->
      case model.focusSidebar of
        True ->
          [ div [ (Html.Attributes.id "sidebar") ]
            ( (viewScoreAndTimer model.score model.timer) ++
              (viewControls model) ++
              (viewGroupOrders model)
            )
          ]
        False ->
          []
    _ ->
      []


viewScoreAndTimer : Int -> Maybe Int -> List (Html Msg)
viewScoreAndTimer score timer =
  [ div [ (Html.Attributes.id "score-and-timer") ]
    [ div []
      [ div [ (Html.Attributes.id "score") ]
        [ div [ (Html.Attributes.id "score-label") ]
          [ (Html.text "Score") ]
        , div [ (Html.Attributes.id "score-points") ]
          [ (Html.text (toString score)) ]
        ]
      , div [ (Html.Attributes.id "timer") ]
        [ div [ (Html.Attributes.id "timer-label") ]
          [ (Html.text "Timer") ]
        , div [ (Html.Attributes.id "timer-points") ]
          [ (Html.text (timerToStr timer)) ]
        ]
      ]
    ]
  ]


viewControls : Model -> List (Html Msg)
viewControls model =
  case model.config.showControls of
    True ->
      [ div [ (Html.Attributes.id "controls") ]
        [ div [ (Html.Attributes.id "control-label") ]
          [ (Html.text "Speed") ]
        , div [ (Html.Attributes.id "control-value") ]
          [ (Html.text (toString (Maybe.withDefault 0 model.timer))) ]
        , div [ (Html.Attributes.id "control-value") ]
          [ (Html.text (toString (Basics.round model.player.speed))) ]
        , div [ (Html.Attributes.id "control-label") ]
          [ (Html.text "Heading") ]
        , div [ (Html.Attributes.id "control-value") ]
          [ (Html.text (toString (Basics.round model.player.bearing))) ]
        , div [ (Html.Attributes.id "control-label") ]
          [ (Html.text "Stearing Angle") ]
        , div [ (Html.Attributes.id "control-value") ]
          [ (Html.text (toString (Basics.round model.player.stearingAngle))) ]
        ]
      ]
    False ->
      []


viewGroupOrders : Model -> List (Html Msg)
viewGroupOrders model =
  [ div [ (Html.Attributes.id "group-orders") ]
    [ div [ (Html.Attributes.id "group-orders-label") ]
      [ (Html.text "Incoming Orders") ]
    , ol []
      (List.map (\n -> viewGroupOrder (n, distanceTo model.player.location n.location) model.player) model.groupOrders)
    ]
  ]


viewGroupOrder : (GroupOrder, Float) -> Player -> Html Msg
viewGroupOrder (groupOrder, distanceRaw) player =
  let
    sandwichText = if groupOrder.count == 1 then " Sandwich" else " Sandwiches"
    (active, distanceToPlayer, timeToPlayer) =
      let
        distance = distanceInKm distanceRaw
        time = Basics.round (distance / 0.25)
      in
        ( isInRange distanceRaw groupOrder.radius
        , (toString distance) ++ " km"
        , (toString time) ++ " min"
        )
    class = if active then
               "active"
            else if groupOrder.selected then
              "selected"
            else if groupOrder.highlighted then
              "highlighted"
            else
              ""
  in
    li [ Html.Events.onClick (SelectGroupOrder groupOrder.id), (Html.Attributes.class class) ]
      [ h3 [] [ Html.text ((toString groupOrder.count) ++ sandwichText) ]
      , p [] [ ( Html.text (distanceToPlayer ++ " / " ++ timeToPlayer) ) ]
      ]



-- PORTS


port restart : () -> Cmd msg

port initializeMap : () -> Cmd msg

port initializeControls : () -> Cmd msg

port updateZoomLevel : Int -> Cmd msg

port updateTrafficLightView : (Id, Maybe TrafficLight) -> Cmd msg

port updateGroupOrder : (Id, Maybe GroupOrder) -> Cmd msg

port updatePlayer : Player -> Cmd msg

port updatePlayerGroupOrderDirections : (Player, List GroupOrder) -> Cmd msg

port createBikerRoute : () -> Cmd msg

port updateBikersView : List BikerJs -> Cmd msg

type alias BikerJs =
  { id: Int
  , route: List Location
  , speed: Speed
  , location: Location
  , lastLocation: Location
  }

bikersToJs : List Biker -> List BikerJs
bikersToJs bikers =
  List.map (\n -> BikerJs n.id (Cons.toList n.route) n.speed n.location n.lastLocation) bikers


port tell : String -> Cmd msg



-- SUBSCRIPTIONS


port onMapInitialized: (() -> msg) -> Sub msg

port bikerRouteCreated: ((List Location, Speed) -> msg) -> Sub msg

port playerMovements : (PlayerMovement -> msg) -> Sub msg

port controllerInput : (String -> msg) -> Sub msg

port playerDistanceToRoads: (Float -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onMapInitialized (always OnMapInitialized)
    , bikerRouteCreated CreateBiker
    , Time.every (model.config.interval * millisecond) Tick
    , playerMovements UpdatePlayerMovement
    , controllerInput ControllerInput
    , playerDistanceToRoads UpdatePlayerDistanceToRoads
    ]
