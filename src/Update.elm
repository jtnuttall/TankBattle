module Update exposing (update)

import Array
import Canvas.Texture as Texture exposing (Texture)
import Component.Gun as Gun
import Component.Player as Player exposing (Player)
import Component.Projectile as Projectile exposing (Projectile)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyChange(..), KeyParser, RawKey(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Model exposing (Load(..), Model, Sprites)
import Msg exposing (Msg(..))


gameKeys : KeyParser
gameKeys =
    Keyboard.oneOf [ arrowKey, Keyboard.uiKey, Keyboard.whitespaceKey ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame deltaTime ->
            ( deltaTimeUpdate (deltaTime / 1000) model, Cmd.none )

        Resize width height ->
            ( { model | gameDims = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        KeyPress key ->
            ( playerKeyboardUpdate key model, Cmd.none )

        TextureLoaded Nothing ->
            ( { model | sprites = Failure }, Cmd.none )

        TextureLoaded (Just sheet) ->
            ( { model | sprites = Success <| spritesUpdate sheet }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


deltaTimeUpdate : Float -> Model -> Model
deltaTimeUpdate deltaTime model =
    let
        player =
            model.player
    in
    { model
        | deltaTime = deltaTime
        , player =
            player
                |> Player.updateProjectiles deltaTime model.gameDims
                |> Player.transform deltaTime model.gameDims
    }


playerKeyboardUpdate : Keyboard.Msg -> Model -> Model
playerKeyboardUpdate key model =
    let
        pressedKeys =
            Keyboard.updateWithParser gameKeys key model.player.pressedKeys

        player =
            model.player
    in
    { model
        | isPaused =
            if List.member Escape pressedKeys then
                not model.isPaused

            else
                model.isPaused
        , player = { player | pressedKeys = pressedKeys }
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


spritesUpdate : Texture -> Sprites
spritesUpdate sheet =
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
            , zeroFrame = sprite gunStart 0 sheet
            , frames = animationFrames gunStart gunEnd
            }
        , body =
            { frameIndex = 0
            , zeroFrame = sprite bodyStart 0 sheet
            , frames = animationFrames bodyStart bodyEnd
            }
        }
    }
