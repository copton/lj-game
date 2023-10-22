-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


module Main exposing (..)

import Functools exposing (..)
import Model exposing (..)
import Pdict exposing (..)
import Playground exposing (..)
import Update exposing (..)
import View exposing (..)



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


make_game : ( Int, Position, Drone, Items ) -> Game
make_game ( n, pos, player, items ) =
    let
        innert_keys =
            { counting = False, counter = 0 }

        meta =
            { innert_keys = innert_keys, action = Nothing }

        config =
            { board_size = n }

        items_with_player =
            insert pos (PlayerItem player) items

        board =
            { items = items_with_player, playter= (Position 0 0) ] }

        game =
            { board = board, config = config, meta = meta, ticker = 0 }
    in
    game


level : ( Int, Position, Drone, Items )
level =
    ( 15
    , Position 0 0
    , Drone Blue 4 Nothing
    , fromList
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
    )


main =
    game view_game update_game (make_game level)
