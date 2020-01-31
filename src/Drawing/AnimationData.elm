module Drawing.AnimationData exposing (AnimationData, getFrame, running, update)

import Array as Array exposing (Array)
import Canvas.Texture exposing (Texture)
import Maybe exposing (withDefault)
import Utility exposing (cycleI)


type alias AnimationData =
    { frameIndex : Int
    , running : Bool
    , done : Bool
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
    if data.running then
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

    else
        data


running : Bool -> AnimationData -> AnimationData
running startRun data =
    { data
        | time = 0
        , running = startRun || not data.done
        , frameIndex = 0
    }
