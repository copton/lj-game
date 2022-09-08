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
    { posX : Number
    , posY : Number
    }


type Drone
    = Drone
        { color : GameColor
        , position : Position
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


type alias Board =
    { size : Number
    , player : Drone
    , drones : List Drone
    , walls : List Wall
    }


type alias Meta =
    { key_down : Bool }


type alias Game =
    { board : Board
    , meta : Meta
    }



-- MAIN


make_game : Board -> Game
make_game board =
    { board = board, meta = { key_down = False } }


level : Board
level =
    let
        small_drone =
            Drone { color = Green, position = Position 1 1, size = 1, carry = Nothing }
    in
    { size = 20
    , player = Drone { color = Blue, position = Position 0 0, size = 3, carry = Nothing }
    , drones = [ Drone { color = Red, position = Position 9 3, size = 3, carry = Just small_drone } ]
    , walls =
        [ Wall (Position 5 5) Vertical 4
        , Wall (Position 5 5) Horizontal 7
        ]
    }


main =
    game view update (make_game level)



-- Logic


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


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x



-- VIEW


view : Computer -> Game -> List Shape
view computer game =
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

        b =
            computer.screen.bottom
    in
    rectangle (rgb 174 238 238) w w
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
        ++ drawDrone r board.size board.player
        ++ List.concatMap (drawWall r board.size) board.walls
        ++ List.concatMap (drawDrone r board.size) board.drones


drawDrone : Number -> Number -> Drone -> List Shape
drawDrone r n (Drone drone) =
    let
        justDrone =
            rectangle (droneColor drone.color) r r

        droneCarry =
            case drone.carry of
                Nothing ->
                    rectangle black 2 2

                Just (Drone carried) ->
                    rectangle (droneColor carried.color) (r / 2) (r / 2)
    in
    List.map (moveToPosition r n drone.position) [ justDrone, droneCarry ]


drawWall : Number -> Number -> Wall -> List Shape
drawWall r n wall =
    let
        go position =
            moveToPosition r n position (rectangle grey r r)
    in
    List.map go (expandWall wall)


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


update : Computer -> Game -> Game
update computer game =
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
    case board.player of
        Drone { color, position, size, carry } ->
            let
                dx =
                    toX computer.keyboard

                dy =
                    toY computer.keyboard

                { posX, posY } =
                    position

                newPosX =
                    boundaries 0 (board.size - 1) (posX + dx)

                newPosY =
                    boundaries 0 (board.size - 1) (posY + dy)
            in
            { size = board.size
            , player = Drone { color = color, position = Position newPosX newPosY, size = size, carry = carry }
            , drones = board.drones
            , walls = board.walls
            }


boundaries : comparable -> comparable -> comparable -> comparable
boundaries lower_bound upper_bound x =
    if x < lower_bound then
        lower_bound

    else if x > upper_bound then
        upper_bound

    else
        x
