module App.GroupOrder exposing (..)

import App.Map exposing (Id, Location, Road, createLocation, isNearby)
import App.Player exposing (Player)
import GeoJson exposing (GeoJson, GeoJsonObject(..))
import Time exposing (Time)


type alias GroupOrderLocation =
  { location : Location
  }


type alias GroupOrder =
  { id: Id
  , location : Location
  , radius : Float
  , count : Int
  , activeSince : Maybe Time
  , highlighted : Bool
  , selected : Bool
  }


type SelectionCommand = Up | Down | Select


up : SelectionCommand
up = Up


down : SelectionCommand
down = Down


select : SelectionCommand
select = Select


createGroupOrderLocation : Location -> GroupOrderLocation
createGroupOrderLocation location =
  { location = location
  }


createGroupOrder : Id -> Location -> Int -> GroupOrder
createGroupOrder id location count =
  { id = id
  , location = location
  , radius = 50
  , count = count
  , activeSince = Nothing
  , highlighted = False
  , selected = False
  }


generateGroupOrder : Id -> List GroupOrderLocation -> Int -> Int -> Maybe GroupOrder
generateGroupOrder id groupOrderLocations locationIndex count =
  let
    location = List.head (List.drop locationIndex groupOrderLocations)
  in
    case location of
      Just l ->
        Just (createGroupOrder id l.location count)

      Nothing ->
        Nothing


updateGroupOrders : List GroupOrder -> Player -> Time -> Time -> ((Id, Maybe GroupOrder) -> Cmd m) -> (List GroupOrder, List (Cmd m), Int)
updateGroupOrders groupOrders player time timeForScore createUpdateCmd =
  let
    isActive : GroupOrder -> Player -> Bool
    isActive g p = isNearby p.location g.location g.radius

    update : GroupOrder -> Player -> (Maybe GroupOrder, Cmd m, Int)
    update g p =
      case (isActive g p, g.activeSince) of
        (True, Nothing) ->
          updateGroupOrder g ( Just { g | activeSince = Just time } ) createUpdateCmd

        (True, Just t) ->
          case (t < time - timeForScore, g.count) of
            (True, 0) ->
              updateGroupOrder g Nothing createUpdateCmd

            (True, 1) ->
              updateGroupOrder g Nothing createUpdateCmd

            (True, c) ->
              updateGroupOrder g ( Just { g | activeSince = Just time
                  , count = g.count - 1 }
              ) createUpdateCmd

            (False, _) ->
              updateGroupOrder g (Just g) createUpdateCmd

        (False, Just _) ->
          updateGroupOrder g (Just { g | activeSince = Nothing }) createUpdateCmd

        (False, Nothing) ->
          updateGroupOrder g (Just g) createUpdateCmd
  in
    List.foldr (\g (l, c, s) ->
      case update g player of
        (Just e, cmd, ns) ->
          (e :: l, cmd :: c, s + ns)

        (Nothing, cmd, ns) ->
          (l, cmd :: c, s + ns)

    ) ([], [], 0) groupOrders


updateGroupOrder : GroupOrder -> Maybe GroupOrder -> ((Id, Maybe GroupOrder) -> Cmd m) -> (Maybe GroupOrder, Cmd m, Int)
updateGroupOrder oldGroupOrder newGroupOrder createUpdateCmd =
  let
    score = case newGroupOrder of
      Just n -> oldGroupOrder.count - n.count
      Nothing -> oldGroupOrder.count
  in
  ( newGroupOrder
  , if Just oldGroupOrder == newGroupOrder then
      Cmd.none
    else
      createUpdateCmd (oldGroupOrder.id, newGroupOrder)
  , score
  )


updateSelectedGroupOrder : List GroupOrder -> SelectionCommand -> ((Id, Maybe GroupOrder) -> Cmd m) -> (List GroupOrder, List (Cmd m))
updateSelectedGroupOrder groupOrders selectionCommand updateGroupOrder =
  let
    updateCommands l =
      List.map (\n -> updateGroupOrder (n.id, Just n)) l

    (newGroupOrders, cmds, done) = List.foldr
      (\n (l, c, done) ->
        case selectionCommand of
          Up ->
            case l of
              p :: t ->
                case (p.highlighted, done) of
                  (True, False) ->
                    let
                      new = { n | highlighted = True }
                      newP = { p | highlighted = False }
                    in
                      (new :: newP :: t, (updateCommands [new, newP]) ++ c, True)

                  _ ->
                    (n :: l, c, done)

              _ ->
                (n :: l, c, done)

          Down ->
            case (n.highlighted, l) of
              (True, p::t) ->
                let
                  new = { n | highlighted = False }
                  newP = { p | highlighted = True }
                in
                  (new :: newP :: t, (updateCommands [new, newP]) ++ c, True)

              (True, []) ->
                (n :: l, c, True)

              _ ->
                (n :: l, c, done)

          Select ->
            case (n.highlighted, n.selected) of
              (True, _) ->
                let
                  new = { n | selected = True }
                in
                  ( new :: l
                  , (updateCommands [new]) ++ c
                  , True
                  )
              (_, True) ->
                let
                  new = { n | selected = False }
                in
                  ( new :: l
                  , (updateCommands [new]) ++ c
                  , done
                  )
              (_, _) ->
                (n :: l, c, done)


      )
      ([], [], False)
      groupOrders
  in
    case done of
      True ->
        (newGroupOrders, cmds)
      False ->
        case selectionCommand of
          Up ->
            case groupOrders of
              h :: t ->
                let
                  new = { h | highlighted = True }
                in
                (new :: t, [])
              _ ->
                (groupOrders, [])
          Down ->
            case groupOrders of
              h :: t ->
                let
                  new = { h | highlighted = True }
                in
                (new :: t, [])
              _ ->
                ([], [])
          Select ->
            (groupOrders, [])



-- parsing


readGroupOrderLocations : GeoJson -> Maybe (List GroupOrderLocation)
readGroupOrderLocations geoJson =
  case geoJson of
    (FeatureCollection features, _) ->
      Just (List.foldl
              (\f l -> case (readGroupOrderLocation f) of
                Just gl -> gl :: l
                Nothing -> l
              )
              []
              features)
    _ -> Nothing


readGroupOrderLocation : GeoJson.FeatureObject -> Maybe GroupOrderLocation
readGroupOrderLocation feature =
  case feature.geometry of
    Just (GeoJson.Point (lng, lat, _)) ->
      Just (createGroupOrderLocation (createLocation lat lng))
    _ -> Nothing
