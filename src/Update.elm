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
            if model.isPaused then
                ( model, Cmd.none )

            else
                ( deltaTimeUpdate (deltaTime / 1000) model, Cmd.none )

        Resize width height ->
            ( { model | gameDims = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        KeyPress key ->
            ( playerMoveUpdate key model, Cmd.none )

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


playerMoveUpdate : Keyboard.Msg -> Model -> Model
playerMoveUpdate key model =
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


spritesUpdate : Texture -> Sprites
spritesUpdate sheet =
    let
        sprite x y =
            Texture.sprite
                { x = x * 128
                , y = y * 128
                , width = 128
                , height = 128
                }
                sheet
    in
    { tank =
        { gun =
            List.range 3 11
                |> List.map toFloat
                |> List.map (\x -> sprite x 0)
                |> Array.fromList
        , body =
            List.range 1 2
                |> List.map toFloat
                |> List.map (\x -> sprite x 0)
                |> Array.fromList
        }
    }
