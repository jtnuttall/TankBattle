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
