module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Components.GameCanvas as GameCanvas
import Components.Player as Player
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyParser, RawKey(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import Utility exposing (mapTuple)


gameKeys : KeyParser
gameKeys (RawKey key) =
    case String.toUpper key of
        "W" ->
            Just <| Character "W"

        "A" ->
            Just <| Character "A"

        "S" ->
            Just <| Character "S"

        "D" ->
            Just <| Character "D"

        "E" ->
            Just <| Character "E"

        "Q" ->
            Just <| Character "Q"

        "Escape" ->
            Just <| Escape

        _ ->
            Nothing



--parser : KeyParser
--parser (RawKey _) =
--    arrowKey
---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame deltaTime ->
            if model.isPaused then
                ( model, Cmd.none )

            else
                ( { model
                    | count = model.count + 1
                    , deltaTime = deltaTime / 1000
                  }
                , Cmd.none
                )

        Resize width height ->
            ( { model
                | windowDims =
                    ( toFloat width
                    , toFloat height
                    )
              }
            , Cmd.none
            )

        KeyPress key ->
            let
                ( pressedKeys, keyChange ) =
                    Keyboard.updateWithKeyChange gameKeys key model.player.pressedKeys

                { x, y } =
                    wasd pressedKeys

                player =
                    model.player

                transform =
                    model.player.transform

                rotate =
                    if List.member (Character "Q") pressedKeys then
                        -1.0

                    else if List.member (Character "E") pressedKeys then
                        1.0

                    else
                        0
            in
            ( { model
                | player =
                    if model.isPaused then
                        player

                    else
                        { player
                            | rotation = player.rotation + (model.deltaTime * player.rotateSpeed * degrees rotate)
                            , position =
                                mapTuple
                                    ((+) << (*) model.deltaTime << (*) model.player.moveSpeed <| toFloat x)
                                    ((+) << (*) model.deltaTime << (*) model.player.moveSpeed << negate <| toFloat y)
                                    player.position
                            , pressedKeys = pressedKeys
                        }
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        ( width, height ) =
            model.gameDims
    in
    { title = "Tank Battle!"
    , body =
        [ div [ id "gameContainer" ]
            [ Canvas.toHtml
                ( floor width, floor height )
                []
                (GameCanvas.canvas model)
            ]
        ]
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.isPaused then
            Sub.none

          else
            onAnimationFrameDelta Frame
        , onResize Resize

        --, Sub.map KeyDown Keyboard.downs
        --, Sub.map KeyUp Keyboard.ups
        , Sub.map KeyPress Keyboard.subscriptions
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = Model.init
        , update = update
        , subscriptions = subscriptions
        }
