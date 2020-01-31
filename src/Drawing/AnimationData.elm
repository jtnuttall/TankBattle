module AnimationData exposing (..)

import Array as Array exposing (Array)
import Canvas.Texture exposing (Texture)
import Maybe exposing (withDefault)


type alias AnimationData =
    { animationFrame : Int
    , zeroFrame : Texture
    , frames : Array Texture
    }


getFrame : AnimationData -> Texture
getFrame data =
    data
        |> Array.get data.animationFrame
        |> withDefault data.zeroFrame
