module Utility exposing (..)

-- Pseudo map function preventing repitition of the following pattern


mapTuple : (a -> b) -> (c -> d) -> ( a, c ) -> ( b, d )
mapTuple f1 f2 =
    Tuple.mapFirst f1 << Tuple.mapSecond f2
