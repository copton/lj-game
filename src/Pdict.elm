module Pdict exposing (..)

import Dict
import Functools exposing (..)
import List


type alias PDict a =
    Dict.Dict Int (Dict.Dict Int a)


type alias Position =
    { posX : Int
    , posY : Int
    }


empty : PDict a
empty =
    Dict.empty


get : Position -> PDict a -> Maybe a
get { posX, posY } d =
    Dict.get posX d |> Maybe.andThen (Dict.get posY)


is_free : Position -> PDict a -> Bool
is_free pos d =
    case get pos d of
        Nothing ->
            True

        _ ->
            False


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


map_with_pos : (Position -> a -> b) -> PDict a -> PDict b
map_with_pos f =
    fromList << List.map (\( p, x ) -> ( p, f p x )) << toList


foldr_with_pos : (Position -> a -> PDict a -> PDict a) -> PDict a -> PDict a
foldr_with_pos f d =
    List.foldr (uncurry f) empty (toList d)


find : (a -> Bool) -> PDict a -> Maybe Position
find f dict =
    let
        go pos x =
            if f x then
                Just pos

            else
                Nothing
    in
    dict |> toList |> List.map go |> List.find isJust


union : PDict a -> PDict a -> PDict a
union left right =
    fromList (toList left ++ toList right)


unions : List (PDict a) -> PDict a
unions ds =
    fromList (List.concatMap toList ds)
