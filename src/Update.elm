module Update exposing (update)

import Array
import Canvas.Texture as Texture exposing (Texture)
import Component.Gun as Gun
import Component.Player as Player exposing (Player)
import Component.Projectile as Projectile exposing (Projectile)
import Constants
import Drawing.Sprites as Sprites exposing (Sprites)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyChange(..), KeyParser, RawKey(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Model exposing (Model)
import Msg exposing (Msg(..))


gameKeys : KeyParser
gameKeys =
    Keyboard.oneOf [ arrowKey, Keyboard.uiKey, Keyboard.whitespaceKey ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame deltaTime ->
            ( deltaTimeUpdate (deltaTime / 1000) model
            , Cmd.none
            )

        Resize width height ->
            ( { model | gameDims = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        KeyPress key ->
            ( playerKeyboardUpdate key model
            , Cmd.none
            )

        TextureLoaded Nothing ->
            ( { model | sprites = Sprites.Failure }
            , Cmd.none
            )

        TextureLoaded (Just sheet) ->
            ( { model | sprites = Sprites.Success <| Sprites.init sheet }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


deltaTimeUpdate : Float -> Model -> Model
deltaTimeUpdate deltaTime model =
    let
        player =
            model.player

        animationData =
            { tank =
                { gun =
                    if Player.shouldFire player then
                        Just 1

                    else
                        Just 0
                , body = Nothing
                }
            }
    in
    { model
        | deltaTime = deltaTime
        , sprites = Sprites.update deltaTime animationData model.sprites
        , player = Player.update deltaTime model.gameDims model.player
    }


playerKeyboardUpdate : Keyboard.Msg -> Model -> Model
playerKeyboardUpdate key model =
    let
        pressedKeys =
            Keyboard.updateWithParser gameKeys key model.player.pressedKeys

        player =
            model.player

        animationData =
            model.sprites
    in
    { model
        | isPaused =
            if List.member Escape pressedKeys then
                not model.isPaused

            else
                model.isPaused
        , player = { player | pressedKeys = pressedKeys }
    }
