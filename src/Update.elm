module Update exposing (update)

import Array
import Canvas.Texture as Texture
import Component.Gun as Gun
import Component.Player as Player exposing (Player)
import Component.Projectile as Projectile exposing (Projectile)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyChange(..), KeyParser, RawKey(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Model exposing (Load(..), Model)
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

        TextureLoaded (Just texture) ->
            ( { model
                | sprites =
                    let
                        sprite x y t =
                            Texture.sprite
                                { x = x * (128 + 10)
                                , y = y * (128 + 10)
                                , width = 128
                                , height = 128
                                }
                                t
                    in
                    Success
                        { tank =
                            { gun = Array.fromList [ sprite 1 0 texture ]
                            , body = Array.fromList [ sprite 3 0 texture ]
                            }
                        }
              }
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
