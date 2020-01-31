module Drawing.AnimationData exposing (..)

import Array as Array exposing (Array)
import Canvas.Texture exposing (Texture)
import Maybe exposing (withDefault)


type alias AnimationData =
    { frameIndex : Int
    , frames : Array Texture
    , zeroFrame : Texture
    }


getFrame : AnimationData -> Texture
getFrame data =
    data.frames
        |> Array.get data.frameIndex
        |> withDefault data.zeroFrame
