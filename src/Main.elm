-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


module Main exposing (main, update_game, view_game)

import Dict
import Playground exposing (..)



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
    }


type alias Meta =
    { key_down : Bool
    , counter : Int
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
    { board = board, meta = { key_down = False, counter = 0 } }


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
        ++ List.concatMap (drawItem r board.size) (toList board.items)
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


drawItem : Number -> Int -> ( Position, Item ) -> List Shape
drawItem r n ( pos, item ) =
    case item of
        WallItem ->
            [ moveToPosition r n pos (drawWall r) ]

        DroneItem drone ->
            drawDrone r n pos drone

        GarageItem garage ->
            [ drawGarage r n pos garage ]

        PortalItem portal ->
            if portal.open then
                drawOpenPortal r n pos portal.color portal.size

            else
                drawClosedPortal r n pos portal.color portal.size


drawOpenPortal : Number -> Int -> Position -> GameColor -> Int -> List Shape
drawOpenPortal r n pos color size =
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


drawClosedPortal : Number -> Int -> Position -> GameColor -> Int -> List Shape
drawClosedPortal r n pos color size =
    drawOpenPortal r n pos color size


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


update_game : Computer -> Game -> Game
update_game computer game =
    let
        deltaX =
            round (toX computer.keyboard)

        deltaY =
            round (toY computer.keyboard)
    in
    case ( is_pressed computer, game.meta.key_down ) of
        ( False, False ) ->
            game

        ( True, False ) ->
            { board = update_board deltaX deltaY game.board, meta = { key_down = True, counter = 0 } }

        ( True, True ) ->
            if game.meta.counter < 10 then
                let
                    meta =
                        game.meta
                in
                { board = game.board, meta = { meta | counter = meta.counter + 1 } }

            else
                { board = update_board deltaX deltaY game.board, meta = { key_down = True, counter = 0 } }

        ( False, True ) ->
            { board = game.board, meta = { key_down = False, counter = 0 } }


is_pressed : Computer -> Bool
is_pressed computer =
    computer.keyboard.up || computer.keyboard.down || computer.keyboard.left || computer.keyboard.right


update_board : Int -> Int -> Board -> Board
update_board deltaX deltaY board =
    let
        newPosX =
            boundaries 0 (board.size - 1) (board.playerPos.posX + deltaX)

        newPosY =
            boundaries 0 (board.size - 1) (board.playerPos.posY + deltaY)

        newPos =
            Position newPosX newPosY
    in
    case get newPos board.items of
        Nothing ->
            { board | playerPos = newPos }

        Just WallItem ->
            board

        Just (DroneItem drone) ->
            if board.playerDrone.size <= drone.size then
                board

            else
                case board.playerDrone.carry of
                    Nothing ->
                        let
                            carried_drone =
                                CarriedDrone drone.color drone.size

                            player_drone =
                                board.playerDrone
                        in
                        { board
                            | playerPos = newPos
                            , playerDrone = { player_drone | carry = Just carried_drone }
                            , items = remove newPos board.items
                        }

                    Just carried_drone ->
                        let
                            player_drone =
                                board.playerDrone
                        in
                        { board
                            | playerPos = newPos
                            , playerDrone = { player_drone | carry = Just (CarriedDrone drone.color drone.size) }
                            , items = insert newPos (DroneItem (Drone carried_drone.color carried_drone.size Nothing)) board.items
                        }

        Just (GarageItem garage) ->
            case board.playerDrone.carry of
                Nothing ->
                    { board | playerPos = newPos }

                Just carried_drone ->
                    if garage.color /= carried_drone.color then
                        board

                    else
                        let
                            player_drone =
                                board.playerDrone
                        in
                        { board
                            | playerPos = newPos
                            , playerDrone =
                                { player_drone
                                    | size = player_drone.size + carried_drone.size
                                    , carry = Nothing
                                }
                        }

        Just (PortalItem portal) ->
            if portal.size > board.playerDrone.size then
                board

            else
                { board | playerPos = newPos }


boundaries : comparable -> comparable -> comparable -> comparable
boundaries lower_bound upper_bound x =
    if x < lower_bound then
        lower_bound

    else if x > upper_bound then
        upper_bound

    else
        x
