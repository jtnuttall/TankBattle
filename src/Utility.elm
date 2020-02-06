module Utility exposing (..)

import Array as Array exposing (Array)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f xsin ysin =
    case ( xsin, ysin ) of
        ( x :: xs, y :: ys ) ->
            f x y :: zipWith f xs ys

        ( _, _ ) ->
            []


anyOf : List a -> List a -> Bool
anyOf queryList sampleList =
    sampleList
        |> List.repeat (List.length queryList)
        |> zipWith List.member queryList
        |> List.any identity



-- if only we had typeclasses... :(


cycleI : Int -> Int -> Int -> Int
cycleI min max val =
    if val <= min then
        max

    else if val >= max then
        min

    else
        val


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
