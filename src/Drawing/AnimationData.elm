module Drawing.AnimationData exposing (AnimationData, getFrame, update)

import Array as Array exposing (Array)
import Canvas.Texture exposing (Texture)
import Maybe exposing (withDefault)
import Utility exposing (cycleI)


type alias AnimationData =
    { frameIndex : Int
    , time : Float
    , step : Float
    , frames : Array Texture
    , zeroFrame : Texture
    }


getFrame : AnimationData -> Texture
getFrame data =
    data.frames
        |> Array.get data.frameIndex
        |> withDefault data.zeroFrame


update : Float -> AnimationData -> AnimationData
update deltaTime data =
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
