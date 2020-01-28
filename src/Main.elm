module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Components.GameCanvas as GameCanvas
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Lib.Keyboard as Keyboard
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import Update


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , if model.isPaused then
            Sub.none

          else
            onAnimationFrameDelta Frame
        , onResize Resize
        ]


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = Model.init
        , update = Update.update
        , subscriptions = subscriptions
        }
