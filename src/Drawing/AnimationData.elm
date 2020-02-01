module Drawing.AnimationData exposing (AnimationData, AnimationUpdates, getFrame, update)

import Array as Array exposing (Array)
import Canvas.Texture exposing (Texture)
import Maybe exposing (withDefault)
import Utility exposing (cycleI)


type alias AnimationData =
    { frameIndex : Int
    , nTimes : Maybe Int
    , time : Float
    , step : Float
    , frames : Array Texture
    , zeroFrame : Texture
    }


type alias AnimationUpdates =
    { tank :
        { body : Maybe Int
        , gun : Maybe Int
        }
    }


getFrame : AnimationData -> Texture
getFrame data =
    data.frames
        |> Array.get data.frameIndex
        |> withDefault data.zeroFrame


updateAnimation : Float -> AnimationData -> AnimationData
updateAnimation deltaTime data =
    let
        doUpdate =
            data.time >= data.step

        cycleFrames =
            cycleI 0 (Array.length data.frames - 1)
    in
    { data
        | time =
            if not doUpdate then
                data.time + deltaTime

            else
                0
        , frameIndex =
            if doUpdate then
                cycleFrames (data.frameIndex + 1)

            else
                data.frameIndex
    }


updateData : Float -> Maybe Int -> AnimationData -> AnimationData
updateData deltaTime nTimesIn data =
    let
        dataUpdated =
            updateAnimation deltaTime data
    in
    case ( data.nTimes, nTimesIn ) of
        ( Just n, Just newN ) ->
            if n > 0 then
                { dataUpdated | nTimes = Just (n - 1) }

            else
                { data | nTimes = Just newN }

        ( Just n, Nothing ) ->
            if n > 0 then
                { dataUpdated | nTimes = Just (n - 1) }

            else
                data

        ( Nothing, Just newN ) ->
            { dataUpdated | nTimes = Just newN }

        _ ->
            dataUpdated


update : Float -> AnimationUpdates -> AnimationData -> AnimationData
update deltaTime animationUpdates data =
    data
        |> updateData deltaTime animationUpdates.tank.body
        |> updateData deltaTime animationUpdates.tank.gun


run : AnimationData -> AnimationData
run data =
    { data
        | time = 0
        , frameIndex = 0
        , nTimes = Nothing
    }


runN : Int -> AnimationData -> AnimationData
runN n data =
    { data
        | time = 0
        , frameIndex = 0
        , nTimes = Just n
    }
