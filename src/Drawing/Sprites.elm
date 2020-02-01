module Drawing.Sprites exposing (Load(..), Sprites, init, update)

import Array
import Canvas.Texture as Texture exposing (Texture)
import Constants
import Drawing.AnimationData as AnimationData exposing (AnimationData, AnimationUpdates)


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
        animationFrames start end =
            List.range start end
                |> List.map toFloat
                |> List.map (\x -> sprite x 0 sheet)
                |> Array.fromList
    in
    { tank =
        { gun =
            { frameIndex = 0
            , nTimes = Just 0
            , time = 0
            , step = Constants.gunAnimationStep
            , zeroFrame = sprite (toFloat Constants.gunStart) 0 sheet
            , frames = animationFrames Constants.gunStart Constants.gunEnd
            }
        , body =
            { frameIndex = 0
            , nTimes = Just 0
            , time = 0
            , step = Constants.bodyAnimationStep
            , zeroFrame = sprite (toFloat Constants.bodyStart) 0 sheet
            , frames = animationFrames Constants.bodyStart Constants.bodyEnd
            }
        }
    }


update : Float -> AnimationUpdates -> Load Sprites -> Load Sprites
update deltaTime nTimes loadSprites =
    case loadSprites of
        Success sprites ->
            let
                tank =
                    sprites.tank
            in
            { sprites
                | tank =
                    { tank
                        | gun = AnimationData.update deltaTime nTimes tank.gun
                        , body = AnimationData.update deltaTime nTimes tank.body
                    }
            }
                |> Success

        _ ->
            loadSprites
