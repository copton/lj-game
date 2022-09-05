-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


module Main exposing (main, update, view)

import Playground exposing (..)



-- Model


type GameColor
    = Red
    | Blue
    | Green


type alias Position =
    { posX : Int
    , posY : Int
    }


type Drone
    = Drone
        { color : GameColor
        , position : Position
        , size : Int
        , carry : Maybe Drone
        }


type Direction
    = Horizontal
    | Vertical


type alias Wall =
    { start : Position
    , direction : Direction
    , length : Int
    }


type alias Board =
    { size : Int
    , drones : List Drone
    , walls : List Wall
    }



-- MAIN


level =
    Board
        20
        [ Drone {Blue, (Position 0 0), 3, Nothing}
        ]
        [ Wall (Position 5 5) Vertical 4
        , Wall (Position 5 5) Horizontal 7
        ]


main =
    game view update level



-- Logic


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


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x



-- VIEW


view : Computer -> Board -> List Shape
view computer board =
    let
        w : Int
        w =
            min (round computer.screen.width) (round computer.screen.height)

        r : Int
        r =
            w // board.size

        b =
            computer.screen.bottom
    in
    rectangle (rgb 174 238 238) (toFloat w) (toFloat w)
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


drawBoard : Int -> Board -> List Shape
drawBoard r board =
    drawGridX r board.size
        ++ drawGridY r board.size
        ++ List.concatMap (drawWall r board.size) board.walls
        ++ List.concatMap (drawDrone r board.size) board.drones


drawDrone : Int -> Int -> Drone -> List Shape
drawDrone r n (Drone drone) =
    let
        rf =
            toFloat r

        justDrone =
            rectangle (droneColor drone.color) rf rf

        droneCarry =
            case drone.carry of
                Nothing ->
                    rectangle black 2 2

                Just (Drone carried) ->
                    rectangle (droneColor carried.color) (rf / 2) (rf / 2)
    in
    List.map (moveToPosition r n drone.position) [ justDrone, droneCarry ]


drawWall : Int -> Int -> Wall -> List Shape
drawWall r n wall =
    let
        rf =
            toFloat r

        nf =
            toFloat n

        go position =
            moveToPosition r n position (rectangle grey rf rf)
    in
    List.map go (expandWall wall)


moveToPosition : Int -> Int -> Position -> Shape -> Shape
moveToPosition r n position shape =
    let
        rf =
            toFloat r

        nf =
            toFloat n
    in
    shape
        |> moveX (-1 * rf / 2)
        |> moveY (-1 * rf / 2)
        |> moveX (-1 * rf * (nf / 2 - 1))
        |> moveY (-1 * rf * (nf / 2 - 1))
        |> moveX (rf * toFloat position.posX)
        |> moveY (rf * toFloat position.posY)


drawGridX : Int -> Int -> List Shape
drawGridX r n =
    let
        go : Int -> Shape
        go m =
            rectangle black 1 (toFloat (r * n))
                |> moveX ((toFloat n / 2 - toFloat m) * toFloat r)
    in
    List.map go
        (List.range 0 n)


drawGridY : Int -> Int -> List Shape
drawGridY r n =
    let
        go : Int -> Shape
        go m =
            rectangle black (toFloat (r * n)) 1
                |> moveY ((toFloat n / 2 - toFloat m) * toFloat r)
    in
    List.map go
        (List.range 0 n)



-- UPDATE


update computer board =
    board
