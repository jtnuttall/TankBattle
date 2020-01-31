module Utility exposing (..)

import Array as Array exposing (Array)



-- I hate that I had to do this, but to build for production, you can't have
-- Debug.crash here. The only way to forcibly obtain a Just from a Maybe
-- is infinite recursion or a default. The issue is that I can't use Debug.crash
-- in a production build, but I also can't just emit an error. My indices should be
-- cyclical. The other obvious solution is a cyclical list, but this runs into
-- similar issues
--veryUglyUnsafeGet : Int -> Array a -> a
--veryUglyUnsafeGet i arr =
--    case Array.get i arr of
--        Just val ->
--            val
--        Nothing ->
--            veryUglyUnsafeGet i arr


mapTuple : (a -> b) -> (c -> d) -> ( a, c ) -> ( b, d )
mapTuple f1 f2 =
    Tuple.mapFirst f1 << Tuple.mapSecond f2


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


commasEveryThird : Int -> List Char -> List Char
commasEveryThird n cs =
    case cs of
        [] ->
            []

        c :: rest ->
            if n == 3 then
                ',' :: c :: commasEveryThird 1 rest

            else
                c :: commasEveryThird (n + 1) rest



-- Not a great way to do this, but the strings are short and the project is personal


prettyInt : Int -> String
prettyInt int =
    int
        |> String.fromInt
        |> String.toList
        |> List.reverse
        |> commasEveryThird 0
        |> List.reverse
        |> String.fromList
