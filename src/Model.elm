module Model exposing (..)

import Pdict exposing (..)


type GameColor
    = Red
    | Blue
    | Green


type alias Drone =
    { color : GameColor
    , size : Int
    , carry : Maybe CarriedDrone
    }


type alias CarriedDrone =
    { color : GameColor
    , size : Int
    }


type Action
    = Move MoveDirection
    | Drop


type MoveDirection
    = Up
    | Down
    | Left
    | Right


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
    | PlayerItem Drone


type alias Items =
    PDict Item


type alias InnertKeys =
    { counting : Bool
    , counter : Int
    }


type alias Meta =
    { innert_keys : InnertKeys
    , action : Maybe Action
    }


type alias Config =
    { board_size : Int
    }


type alias Game =
    { board : Items
    , config : Config
    , meta : Meta
    , ticker : Int
    }
