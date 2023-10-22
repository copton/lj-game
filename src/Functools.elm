module Functools exposing (..)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y


mapSnd : (b -> c) -> ( a, b ) -> ( a, c )
mapSnd f ( x, y ) =
    ( x, f y )


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


applyAll : List (a -> a) -> a -> a
applyAll fs x =
    let
        go f res =
            f res
    in
    List.foldr go x fs


isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        _ ->
            True
