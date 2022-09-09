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
    Dict.Dict Number (Dict.Dict Number a)


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
    { posX : Number
    , posY : Number
    }


type Drone
    = Drone
        { color : GameColor
        , size : Number
        , carry : Maybe Drone
        }


type Direction
    = Horizontal
    | Vertical


type alias Wall =
    { start : Position
    , direction : Direction
    , length : Number
    }


type alias Garage =
    { color : GameColor
    }


type Item
    = WallItem
    | DroneItem Drone
    | GarageItem Garage


type alias Items =
    PDict Item


type alias Board =
    { size : Number
    , playerPos : Position
    , playerDrone : Drone
    , items : Items
    }


type alias Meta =
    { key_down : Bool }


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
            List.map (\diff -> base + toFloat diff) (List.range 0 (round wall.length))
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
    { board = board, meta = { key_down = False } }


level : Board
level =
    { size = 20
    , playerPos = Position 0 0
    , playerDrone = Drone { color = Blue, size = 3, carry = Nothing }
    , items =
        fromList
            (wallItems
                [ Wall (Position 5 5) Vertical 4
                , Wall (Position 5 5) Horizontal 7
                ]
                ++ [ ( Position 9 3, DroneItem (Drone { color = Red, size = 3, carry = Nothing }) )
                   , ( Position 10 10, GarageItem (Garage Red) )
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
            w / board.size
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
        ++ drawDrone r board.size board.playerPos board.playerDrone
        ++ List.concatMap (drawItem r board.size) (toList board.items)


allPositions : Number -> List Position
allPositions n =
    let
        range =
            List.range 0 (round n - 1)
    in
    List.concatMap (\x -> List.map (\y -> Position (toFloat x) (toFloat y)) range) range


drawTiles : Number -> Number -> List Shape
drawTiles r n =
    let
        drawTile pos =
            moveToPosition r n pos (image (r - 2) (r - 2) "../res/sand.jpg")
    in
    List.map drawTile (allPositions n)


drawItem : Number -> Number -> ( Position, Item ) -> List Shape
drawItem r n ( pos, item ) =
    case item of
        WallItem ->
            [ moveToPosition r n pos (drawWall r) ]

        DroneItem drone ->
            drawDrone r n pos drone

        GarageItem garage ->
            [ drawGarage r n pos garage ]


drawGarage : Number -> Number -> Position -> Garage -> Shape
drawGarage r n pos garage =
    moveToPosition r n pos (circle (droneColor garage.color) r)


drawWall : Number -> Shape
drawWall r =
    image r r "../res/brick.jpg"


drawDrone : Number -> Number -> Position -> Drone -> List Shape
drawDrone r n pos (Drone drone) =
    let
        justDrone =
            image r r "../res/drone.jpg"

        -- rectangle (droneColor drone.color) r r
        droneCarry =
            case drone.carry of
                Nothing ->
                    rectangle (droneColor drone.color) (r / 6) (r / 6)

                Just (Drone carried) ->
                    rectangle (droneColor carried.color) (r / 3) (r / 3)
    in
    List.map (moveToPosition r n pos) [ justDrone, droneCarry ]


moveToPosition : Number -> Number -> Position -> Shape -> Shape
moveToPosition r n position shape =
    shape
        |> moveX (-1 * r / 2)
        |> moveY (-1 * r / 2)
        |> moveX (-1 * r * (n / 2 - 1))
        |> moveY (-1 * r * (n / 2 - 1))
        |> moveX (r * position.posX)
        |> moveY (r * position.posY)


drawGridX : Number -> Number -> List Shape
drawGridX r n =
    let
        go : Int -> Shape
        go m =
            rectangle black 1 (r * n)
                |> moveX ((n / 2 - toFloat m) * r)
    in
    List.map go
        (List.range 0 (round n))


drawGridY : Number -> Number -> List Shape
drawGridY r n =
    let
        go : Int -> Shape
        go m =
            rectangle black (r * n) 1
                |> moveY ((n / 2 - toFloat m) * r)
    in
    List.map go
        (List.range 0 (round n))



-- UPDATE


update_game : Computer -> Game -> Game
update_game computer game =
    case ( game.meta.key_down, is_pressed computer ) of
        ( False, False ) ->
            game

        ( False, True ) ->
            { board = update_board computer game.board, meta = { key_down = True } }

        ( True, True ) ->
            game

        ( True, False ) ->
            { board = game.board, meta = { key_down = False } }


is_pressed : Computer -> Bool
is_pressed computer =
    computer.keyboard.up || computer.keyboard.down || computer.keyboard.left || computer.keyboard.right


update_board : Computer -> Board -> Board
update_board computer board =
    case board.playerDrone of
        Drone { color, size, carry } ->
            let
                newPosX =
                    boundaries 0 (board.size - 1) (board.playerPos.posX + toX computer.keyboard)

                newPosY =
                    boundaries 0 (board.size - 1) (board.playerPos.posY + toY computer.keyboard)

                newPos =
                    Position newPosX newPosY
            in
            case get newPos board.items of
                Nothing ->
                    { size = board.size
                    , playerPos = newPos
                    , playerDrone = Drone { color = color, size = size, carry = carry }
                    , items = board.items
                    }

                Just WallItem ->
                    board

                Just (DroneItem drone) ->
                    { size = board.size
                    , playerPos = newPos
                    , playerDrone = Drone { color = color, size = size, carry = Just drone }
                    , items = remove newPos board.items
                    }

                Just (GarageItem garage) ->
                    case carry of
                        Nothing ->
                            board

                        Just carried_drone ->
                            case carried_drone of
                                Drone { c=color, s=size, c=carry } ->
                                    if garage.color /= carried_color then
                                        board

                                    else
                                        { size = board.size
                                        , playerPos = newPos
                                        , playerDrone = Drone { color = color, size = size + carried_size, carry = Nothing }
                                        , items = remove newPos board.items
                                        }


boundaries : comparable -> comparable -> comparable -> comparable
boundaries lower_bound upper_bound x =
    if x < lower_bound then
        lower_bound

    else if x > upper_bound then
        upper_bound

    else
        x
