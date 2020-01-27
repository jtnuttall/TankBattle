module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Components.GameCanvas as GameCanvas
import Components.Player as Player
import Components.Projectile as Projectile
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyParser, RawKey(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import Utility exposing (cycleF, mapTuple)


gameKeys : KeyParser
gameKeys =
    Keyboard.oneOf [ arrowKey, Keyboard.uiKey, Keyboard.whitespaceKey ]



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame deltaTime ->
            let
                player =
                    model.player
            in
            if model.isPaused then
                ( model, Cmd.none )

            else
                ( { model
                    | deltaTime = deltaTime / 1000
                    , player =
                        { player
                            | timeSinceFiring =
                                player.timeSinceFiring + deltaTime / 1000
                            , projectiles = List.map (Projectile.update deltaTime) player.projectiles
                        }
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
            ( keyboardUpdate key model, Cmd.none )

        _ ->
            ( model, Cmd.none )


keyboardUpdate : Keyboard.Msg -> Model -> Model
keyboardUpdate key model =
    let
        ( pressedKeys, keyChange ) =
            Keyboard.updateWithKeyChange gameKeys key model.player.pressedKeys

        player =
            model.player

        { x, y } =
            wasd pressedKeys

        rotate =
            toFloat x

        move =
            toFloat y

        xprime =
            move * sin (degrees player.rotation)

        yprime =
            move * cos (degrees player.rotation)
    in
    { model
        | isPaused =
            if List.member Escape pressedKeys then
                not model.isPaused

            else
                model.isPaused
        , player =
            if model.isPaused then
                { player | pressedKeys = pressedKeys }

            else
                { player
                    | timeSinceFiring =
                        if List.member Spacebar pressedKeys then
                            0

                        else
                            player.timeSinceFiring
                    , projectiles =
                        if List.member Spacebar pressedKeys then
                            { position = Player.center model.player
                            , direction = ( 1, 1 )
                            , speed = 0.2
                            , damage = 100
                            }
                                :: player.projectiles

                        else
                            player.projectiles
                    , rotation = player.rotation + (model.deltaTime * player.rotateSpeed * rotate) |> cycleF 0 360
                    , position =
                        mapTuple
                            (xprime * player.moveSpeed * model.deltaTime |> (+))
                            (yprime * player.moveSpeed * model.deltaTime |> negate |> (+))
                            player.position
                    , pressedKeys = pressedKeys
                }
    }



--if List.member Escape pressedKeys then
--    { model | isPaused = not model.isPaused }
--else if List.member Spacebar pressedKeys then
--    if not model.isPaused && player.timeSinceFiring > 1 then
--    else
--        model
--else
--    let
--    in
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
