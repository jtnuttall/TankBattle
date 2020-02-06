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

        -- Due to the way arrays work, we must provide a default for array indexing.
        -- This will work.
        gunZeroFrame =
            sprite (toFloat Constants.gunStart) 0 sheet

        gunFrames =
            animationFrames Constants.gunStart Constants.gunEnd

        bodyZeroFrame =
            sprite (toFloat Constants.bodyStart) 0 sheet

        bodyFrames =
            animationFrames Constants.bodyStart Constants.bodyEnd
    in
    { tank =
        { gun = AnimationData.init Constants.gunAnimationStep gunZeroFrame gunFrames
        , body = AnimationData.init Constants.bodyAnimationStep bodyZeroFrame bodyFrames
        }
    }


update : Float -> AnimationUpdates -> Load Sprites -> Load Sprites
update deltaTime updates loadSprites =
    case loadSprites of
        Success sprites ->
            let
                tank =
                    sprites.tank
            in
            { sprites
                | tank =
                    { tank
                        | gun = AnimationData.update deltaTime updates.tank.gun tank.gun
                        , body = AnimationData.update deltaTime updates.tank.body tank.body
                    }
            }
                |> Success

        _ ->
            loadSprites
