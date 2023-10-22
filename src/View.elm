module View exposing (..)

import Model exposing (..)
import Pdict exposing (Position, toList)
import Playground exposing (..)


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
