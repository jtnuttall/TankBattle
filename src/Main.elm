module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Components.GameCanvas as GameCanvas
import Components.Player as Player
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Keyboard exposing (Key(..))
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))



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
                    , deltaTime = deltaTime
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
            ( { model | pressedKeys = Keyboard.update key model.pressedKeys }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        ( width, height ) =
            model.windowDims
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
