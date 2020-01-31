module Drawing.Sprites exposing (Load(..), Sprites, init, update)

import Array
import Canvas.Texture as Texture exposing (Texture)
import Constants
import Drawing.AnimationData exposing (AnimationData)


type Load a
    = Loading
    | Failure
    | Success a


type alias Sprites =
    { tank :
        { body : AnimationData
        , gun : AnimationData
        }
    }


sprite : Float -> Float -> Texture -> Texture
sprite x y sheet =
    Texture.sprite
        { x = x * 128
        , y = y * 128
        , width = 128
        , height = 128
        }
        sheet


init : Texture -> Sprites
init sheet =
    let
        ( gunStart, gunEnd ) =
            ( 3, 11 )

        ( bodyStart, bodyEnd ) =
            ( 1, 2 )

        animationFrames start end =
            List.range start end
                |> List.map toFloat
                |> List.map (\x -> sprite x 0 sheet)
                |> Array.fromList
    in
    { tank =
        { gun =
            { frameIndex = 0
            , time = 0
            , step = Constants.gunAnimationStep
            , zeroFrame = sprite gunStart 0 sheet
            , frames = animationFrames gunStart gunEnd
            }
        , body =
            { frameIndex = 0
            , time = 0
            , step = Constants.bodyAnimationStep
            , zeroFrame = sprite bodyStart 0 sheet
            , frames = animationFrames bodyStart bodyEnd
            }
        }
    }


update : Float -> Load Sprites -> Load Sprites
update deltaTime loadSprites =
    case loadSprites of
        Success sprites ->
            loadSprites

        _ ->
            loadSprites
