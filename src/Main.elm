-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


module Main exposing (update_game, view_game)

import Dict
import Html exposing (b, p)
import Playground exposing (..)
import Set



-- PDict


type alias PDict a =
    Dict.Dict Int (Dict.Dict Int a)


empty : PDict a
empty =
    Dict.empty


get : Position -> PDict a -> Maybe a
get { posX, posY } d =
    Dict.get posX d |> Maybe.andThen (Dict.get posY)


insert : Position -> a -> PDict a -> PDict a
insert { posX, posY } value d =
    case Dict.get posX d of
        Nothing ->
            Dict.insert posX (Dict.singleton posY value) d

        Just d2 ->
            Dict.insert posX (Dict.insert posY value d2) d


remove : Position -> PDict a -> PDict a
remove { posX, posY } d =
    case Dict.get posX d of
        Nothing ->
            d

        Just d2 ->
            Dict.insert posX (Dict.remove posY d2) d


toList : PDict a -> List ( Position, a )
toList d =
    List.concatMap (\( posX, d2 ) -> List.map (\( posY, value ) -> ( Position posX posY, value )) (Dict.toList d2)) (Dict.toList d)


fromList : List ( Position, a ) -> PDict a
fromList =
    List.foldr (\( pos, value ) d -> insert pos value d) empty


map : (a -> b) -> PDict a -> PDict b
map f =
    fromList << List.map (\( p, x ) -> ( p, f x )) << toList


union : PDict a -> PDict a -> PDict a
union left right =
    fromList (toList left ++ toList right)


unions : List (PDict a) -> PDict a
unions ds =
    fromList (List.concatMap toList ds)



-- Model


type GameColor
    = Red
    | Blue
    | Green


type alias Position =
    { posX : Int
    , posY : Int
    }


type alias Drone =
    { color : GameColor
    , size : Int
    , carry : Maybe CarriedDrone
    }


type alias CarriedDrone =
    { color : GameColor
    , size : Int
    }


type Direction
    = Horizontal
    | Vertical


type alias Wall =
    { start : Position
    , direction : Direction
    , length : Int
    }


type alias Garage =
    { color : GameColor
    }


type alias Portal =
    { color : GameColor
    , size : Int
    , open : Bool
    }


type Item
    = WallItem
    | DroneItem Drone
    | GarageItem Garage
    | PortalItem Portal


type alias Items =
    PDict Item


type alias Board =
    { size : Int
    , playerPos : Position
    , playerDrone : Drone
    , items : Items
    , ticker : Int
    }


type alias InnertKeys =
    { counting : Bool
    , counter : Int
    }


type alias Meta =
    { innert_keys : InnertKeys
    }


type alias Game =
    { board : Board
    , meta : Meta
    }



-- MAIN


expandWall : Wall -> List Position
expandWall wall =
    let
        go posX posY =
            Position posX posY

        blocks base =
            List.map (\diff -> base + diff) (List.range 0 wall.length)
    in
    case wall.direction of
        Vertical ->
            List.map (go wall.start.posX) (blocks wall.start.posY)

        Horizontal ->
            List.map (flip go wall.start.posY) (blocks wall.start.posX)


wallItems : List Wall -> List ( Position, Item )
wallItems walls =
    List.map (\pos -> ( pos, WallItem )) (List.concatMap expandWall walls)


make_game : Board -> Game
make_game board =
    let
        innert_keys =
            { counting = False, counter = 0 }

        meta =
            { innert_keys = innert_keys }

        game =
            { board = board, meta = meta }
    in
    game


level : Board
level =
    { size = 15
    , playerPos = Position 0 0
    , playerDrone = Drone Blue 4 Nothing
    , items =
        fromList
            (wallItems
                [ Wall (Position 5 5) Vertical 4
                , Wall (Position 5 5) Horizontal 7
                ]
                ++ [ ( Position 9 3, DroneItem (Drone Red 3 Nothing) )
                   , ( Position 10 10, GarageItem (Garage Red) )
                   , ( Position 0 14, DroneItem (Drone Red 6 Nothing) )
                   , ( Position 1 14, DroneItem (Drone Red 5 Nothing) )
                   , ( Position 3 12, PortalItem (Portal Red 5 False) )
                   , ( Position 6 12, PortalItem (Portal Green 6 False) )
                   , ( Position 9 12, PortalItem (Portal Blue 7 False) )
                   ]
            )
    , ticker = 0
    }


main =
    game view_game update_game (make_game level)



-- Logic


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x



-- VIEW


view_game : Computer -> Game -> List Shape
view_game computer game =
    view_board computer game.board


view_board : Computer -> Board -> List Shape
view_board computer board =
    let
        w : Number
        w =
            min computer.screen.width computer.screen.height

        r : Number
        r =
            w / toFloat board.size
    in
    image w w "../res/sand.jpg"
        :: drawBoard r board


droneColor : GameColor -> Color
droneColor c =
    case c of
        Red ->
            red

        Blue ->
            blue

        Green ->
            green


drawBoard : Number -> Board -> List Shape
drawBoard r board =
    drawGridX r board.size
        ++ drawGridY r board.size
        --        ++ drawTiles r board.size
        ++ List.concatMap (drawItem r board.size board.ticker) (toList board.items)
        ++ drawDrone r board.size board.playerPos board.playerDrone


allPositions : Int -> List Position
allPositions n =
    let
        range =
            List.range 0 (n - 1)
    in
    List.concatMap (\x -> List.map (\y -> Position x y) range) range


drawTiles : Number -> Int -> List Shape
drawTiles r n =
    let
        drawTile pos =
            moveToPosition r n pos (image (r - 2) (r - 2) "../res/sand.jpg")
    in
    List.map drawTile (allPositions n)


drawItem : Number -> Int -> Int -> ( Position, Item ) -> List Shape
drawItem r n ticker ( pos, item ) =
    case item of
        WallItem ->
            [ moveToPosition r n pos (drawWall r) ]

        DroneItem drone ->
            drawDrone r n pos drone

        GarageItem garage ->
            [ drawGarage r n pos garage ]

        PortalItem portal ->
            if portal.open then
                drawOpenPortal r n pos portal.color portal.size ticker

            else
                drawClosedPortal r n pos portal.color portal.size


drawOpenPortal : Number -> Int -> Position -> GameColor -> Int -> Int -> List Shape
drawOpenPortal r n pos color size ticker =
    let
        file_infix =
            case color of
                Red ->
                    "red"

                Blue ->
                    "blue"

                Green ->
                    "green"

        size_color =
            case color of
                Red ->
                    black

                Blue ->
                    black

                Green ->
                    red

        file_name =
            "../res/portal_" ++ file_infix ++ ".jpg"

        portal_size =
            words size_color (String.fromInt size)

        show_portal_size =
            if (modBy 10 ticker >= 0) && (modBy 10 ticker < 5) then
                [ portal_size ]

            else
                []

        portal =
            image r r file_name
    in
    List.map (moveToPosition r n pos) (portal :: show_portal_size)


drawClosedPortal : Number -> Int -> Position -> GameColor -> Int -> List Shape
drawClosedPortal r n pos color size =
    let
        file_infix =
            case color of
                Red ->
                    "red"

                Blue ->
                    "blue"

                Green ->
                    "green"

        size_color =
            case color of
                Red ->
                    black

                Blue ->
                    black

                Green ->
                    red

        file_name =
            "../res/portal_" ++ file_infix ++ ".jpg"

        portal_size =
            words size_color (String.fromInt size)

        portal =
            image r r file_name
    in
    List.map (moveToPosition r n pos) [ portal, portal_size ]


drawGarage : Number -> Int -> Position -> Garage -> Shape
drawGarage r n pos garage =
    moveToPosition r n pos (circle (droneColor garage.color) (r / 2))


drawWall : Number -> Shape
drawWall r =
    image r r "../res/brick.jpg"


drawDrone : Number -> Int -> Position -> Drone -> List Shape
drawDrone r n pos drone =
    let
        drone_image =
            image r r "../res/drone.jpg"

        drone_carry =
            case drone.carry of
                Nothing ->
                    []

                Just carried ->
                    let
                        fn =
                            moveToPosition r n pos << (\shape -> shape |> moveY (r / 4))
                    in
                    List.map fn [ carried_pane carried, carried_number carried ]

        carried_pane carried =
            rectangle (droneColor carried.color) (r / 4) (r / 4)

        carried_number carried =
            words white (String.fromInt carried.size)

        drone_color =
            rectangle (droneColor drone.color) (r / 4) (r / 4)

        drone_size =
            words white (String.fromInt drone.size)
    in
    List.map (moveToPosition r n pos) [ drone_image, drone_color, drone_size ] ++ drone_carry


moveToPosition : Number -> Int -> Position -> Shape -> Shape
moveToPosition r n position shape =
    shape
        |> moveX (-1 * r / 2)
        |> moveY (-1 * r / 2)
        |> moveX (-1 * r * (toFloat n / 2 - 1))
        |> moveY (-1 * r * (toFloat n / 2 - 1))
        |> moveX (r * toFloat position.posX)
        |> moveY (r * toFloat position.posY)


drawGridX : Number -> Int -> List Shape
drawGridX r n =
    let
        go : Int -> Shape
        go m =
            rectangle black 1 (r * toFloat n)
                |> moveX ((toFloat n / 2 - toFloat m) * r)
    in
    List.map go
        (List.range 0 n)


drawGridY : Number -> Int -> List Shape
drawGridY r n =
    let
        go : Int -> Shape
        go m =
            rectangle black (r * toFloat n) 1
                |> moveY ((toFloat n / 2 - toFloat m) * r)
    in
    List.map go
        (List.range 0 n)



-- UPDATE


innert_key_pressing : Computer -> InnertKeys -> ( InnertKeys, Bool )
innert_key_pressing computer state =
    let
        is_pressed =
            not (Set.isEmpty computer.keyboard.keys)
    in
    case ( is_pressed, state.counting ) of
        ( False, False ) ->
            ( state, False )

        ( False, True ) ->
            ( { counting = False, counter = 0 }, False )

        ( True, False ) ->
            ( { counting = True, counter = 0 }, True )

        ( True, True ) ->
            if state.counter < 10 then
                ( { counting = True, counter = state.counter + 1 }, False )

            else
                ( { counting = True, counter = 0 }, True )


update_game : Computer -> Game -> Game
update_game computer game =
    let
        ( new_innert_keys, key_down ) =
            innert_key_pressing computer game.meta.innert_keys

        new_meta =
            { innert_keys = new_innert_keys }

        deltaX =
            if key_down then
                round (toX computer.keyboard)

            else
                0

        go_left =
            if Set.member "ArrowLeft" computer.keyboard.keys then
                -1

            else
                0

        go_right =
            if Set.member "ArrowRight" computer.keyboard.keys then
                1

            else
                0

        delta_x =
            if key_down then
                go_left + go_right

            else
                0

        go_up =
            if Set.member "ArrowUp" computer.keyboard.keys then
                1

            else
                0

        go_down =
            if Set.member "ArrowDown" computer.keyboard.keys then
                -1

            else
                0

        delta_y =
            if key_down then
                go_up + go_down

            else
                0

        drop =
            key_down && Set.member "d" computer.keyboard.keys

        new_board =
            update_board delta_x delta_y game.board

        new_game =
            { board = new_board, meta = new_meta }
    in
    new_game


update_board : Int -> Int -> Board -> Board
update_board deltaX deltaY board =
    let
        new_ticker =
            board.ticker + 1

        pos_x =
            boundaries 0 (board.size - 1) (board.playerPos.posX + deltaX)

        pos_y =
            boundaries 0 (board.size - 1) (board.playerPos.posY + deltaY)

        pos =
            Position pos_x pos_y

        ( can_move, new_player_drone, new_items ) =
            move_player pos board.playerDrone board.items

        new_items_with_modified_portals =
            possibly_open_close_portals new_player_drone.size new_items

        new_pos =
            if can_move then
                pos

            else
                board.playerPos

        new_board =
            { size = board.size, playerPos = new_pos, playerDrone = new_player_drone, items = new_items_with_modified_portals, ticker = new_ticker }
    in
    new_board


move_player : Position -> Drone -> Items -> ( Bool, Drone, Items )
move_player position player_drone items =
    case get position items of
        Nothing ->
            ( True, player_drone, items )

        Just WallItem ->
            ( False, player_drone, items )

        Just (DroneItem drone) ->
            if player_drone.size <= drone.size then
                ( False, player_drone, items )

            else
                case player_drone.carry of
                    Just _ ->
                        ( False, player_drone, items )

                    Nothing ->
                        let
                            carried_drone =
                                CarriedDrone drone.color drone.size

                            new_player_drone =
                                { player_drone | carry = Just carried_drone }

                            new_items =
                                remove position items
                        in
                        ( True, new_player_drone, new_items )

        Just (GarageItem garage) ->
            case player_drone.carry of
                Nothing ->
                    ( True, player_drone, items )

                Just carried_drone ->
                    if garage.color /= carried_drone.color then
                        ( False, player_drone, items )

                    else
                        let
                            new_player_drone =
                                { player_drone
                                    | size = player_drone.size + carried_drone.size
                                    , carry = Nothing
                                }
                        in
                        ( True, new_player_drone, items )

        Just (PortalItem portal) ->
            if portal.size > player_drone.size then
                ( False, player_drone, items )

            else
                ( True, player_drone, items )


possibly_open_close_portals : Int -> Items -> Items
possibly_open_close_portals player_drone_size items =
    let
        open item =
            case item of
                PortalItem portal ->
                    if portal.size == player_drone_size then
                        PortalItem { portal | open = True }

                    else
                        PortalItem { portal | open = False }

                _ ->
                    item
    in
    map open items


boundaries : comparable -> comparable -> comparable -> comparable
boundaries lower_bound upper_bound x =
    if x < lower_bound then
        lower_bound

    else if x > upper_bound then
        upper_bound

    else
        x
