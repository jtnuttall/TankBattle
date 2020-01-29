module Utility exposing (..)

-- Pseudo map function preventing repitition of the following pattern


mapTuple : (a -> b) -> (c -> d) -> ( a, c ) -> ( b, d )
mapTuple f1 f2 =
    Tuple.mapFirst f1 << Tuple.mapSecond f2


mapTupleUniform : (a -> b) -> ( a, a ) -> ( b, b )
mapTupleUniform f =
    mapTuple f f


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


cycleF : Float -> Float -> Float -> Float
cycleF min max val =
    if val <= min then
        max

    else if val >= max then
        min

    else
        val


clampF : Float -> Float -> Float -> Float
clampF min max val =
    if val <= min then
        min

    else if val >= max then
        max

    else
        val


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
