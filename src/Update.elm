module Update exposing (..)

import Functools exposing (..)
import Html.Attributes exposing (action)
import Model exposing (..)
import Pdict exposing (..)
import Playground exposing (..)
import Set


update_game : Computer -> Game -> Game
update_game computer game =
    game |> capture_action computer |> increase_ticker |> move_player |> drop_drone


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


key_to_action : Set.Set String -> Maybe Action
key_to_action keys =
    let
        go key dir =
            case dir of
                Just _ ->
                    dir

                Nothing ->
                    case key of
                        "ArrowUp" ->
                            Just (Move Up)

                        "ArrowDown" ->
                            Just (Move Down)

                        "ArrowLeft" ->
                            Just (Move Left)

                        "ArrowRight" ->
                            Just (Move Right)

                        "d" ->
                            Just Drop

                        _ ->
                            Nothing
    in
    Set.foldl go Nothing keys


capture_action : Computer -> Game -> Game
capture_action computer game =
    let
        ( new_innert_keys, key_down ) =
            innert_key_pressing computer game.meta.innert_keys

        action =
            if key_down then
                key_to_action computer.keyboard.keys

            else
                Nothing

        new_meta =
            { innert_keys = new_innert_keys, action = action }
    in
    { game | meta = new_meta }


increase_ticker : Game -> Game
increase_ticker game =
    { game | ticker = game.ticker + 1 }


move_player : Game -> Game
move_player game =
    game


drop_drone : Game -> Game
drop_drone game =
    game


move_player : Config -> Items -> Maybe Action -> Position -> Drone -> ( Position, Drone )
move_player config board mbAction pos drone =
    case mbAction of
        Just (Move d) ->
            let
                new_pos =
                    move_pos config d pos
            in
            player

        _ ->
            player
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


move_pos : Int -> MoveDirection -> Position -> Position
move_pos board_size dir pos =
    let
        ( dx, dy ) =
            case dir of
                Up ->
                    ( 0, 1 )

                Right ->
                    ( 1, 0 )

                Down ->
                    ( 0, -1 )

                Left ->
                    ( -1, 0 )

        new_pos =
            { posX = pos.posX + dx, posY = pos.posY + dy }
    in
    clip board_size new_pos


clip : Int -> Position -> Position
clip board_size pos =
    { posX = boundaries 0 (board_size - 1) pos.posX
    , posY = boundaries 0 (board_size - 1) pos.posY
    }


boundaries : comparable -> comparable -> comparable -> comparable
boundaries lower_bound upper_bound x =
    if x < lower_bound then
        lower_bound

    else if x > upper_bound then
        upper_bound

    else
        x



-- update_board : Int -> Int -> Bool -> Board -> Board
-- update_board deltaX deltaY drop board =
--     let
--         new_ticker =
--             board.ticker + 1
--         pos_x =
--             boundaries 0 (board.size - 1) (board.playerPos.posX + deltaX)
--         pos_y =
--             boundaries 0 (board.size - 1) (board.playerPos.posY + deltaY)
--         pos =
--             Position pos_x pos_y
--         ( can_move, new_player_drone, new_items ) =
--             move_player pos board.playerDrone board.items
--         new_items_2 =
--             possibly_open_close_portals new_player_drone.size new_items
--         ( new_items_3, new_player_drone_2 ) =
--             if drop then
--                 drop_drone board.size pos new_items_2
--             else
--                 ( new_items_2, new_player_drone )
--         new_pos =
--             if can_move then
--                 pos
--             else
--                 board.playerPos
--         new_board =
--             { size = board.size, playerPos = new_pos, playerDrone = new_player_drone, items = new_items_3, ticker = new_ticker }
--     in
--     game |> move_player action |> drop_drone action
-- update_player : Maybe Action -> Game -> Game
-- update_player mbAction game = case mbAction of
--     Just (Move d) -> move_player d game
--     Just Drop -> drop_drone game
--     _ -> game
-- drop_drone : Int -> Position -> Items -> ( Items, Drone )
-- drop_drone board_size pos items =
--     let
--         options =
--             [ ( 0, -1 ), ( -1, 0 ), ( 0, 1 ), ( 1, 0 ) ]
--         possiblities =
--             List.map test options
--         test ( dx, dy ) =
--             let
--                 new_pos =
--                     { posX = pos.posX + dx, posY = pos.posY + dy }
--             in
--             if in_bounds 0 (board_size - 1) new_pos.posX && in_bounds 0 (board_size - 1) new_pos.posY && is_free new_pos items then
--                 Just new_pos
--             else
--                 Nothing
--         isJust x =
--             case x of
--                 Nothing ->
--                     False
--                 _ ->
--                     True
--         choice =
--             List.head (List.filter isJust possiblities)
--     in
--     items
-- possibly_open_close_portals : Int -> Items -> Items
-- possibly_open_close_portals player_drone_size items =
--     let
--         open item =
--             case item of
--                 PortalItem portal ->
--                     if portal.size == player_drone_size then
--                         PortalItem { portal | open = True }
--                     else
--                         PortalItem { portal | open = False }
--                 _ ->
--                     item
--     in
--     map open items
-- in_bounds : comparable -> comparable -> comparable -> Bool
-- in_bounds lower_bound upper_bound x =
--     if x < lower_bound then
--         False
--     else if x > upper_bound then
--         False
--     else
--         True
-- boundaries : comparable -> comparable -> comparable -> comparable
-- boundaries lower_bound upper_bound x =
--     if x < lower_bound then
--         lower_bound
--     else if x > upper_bound then
--         upper_bound
--     else
--         x
