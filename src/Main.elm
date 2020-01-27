module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Components.GameCanvas as GameCanvas
import Components.Player as Player
import Components.Projectile as Projectile exposing (Projectile)
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
                            , projectiles =
                                player.projectiles
                                    |> purge model.gameDims
                                    |> List.map (Projectile.update deltaTime << Debug.log "Projectile")
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
                        if List.member Spacebar pressedKeys && player.timeSinceFiring > 0.25 then
                            0

                        else
                            player.timeSinceFiring
                    , projectiles =
                        if List.member Spacebar pressedKeys && player.timeSinceFiring > 0.25 then
                            { position = Player.center model.player
                            , direction = ( sin (degrees player.rotation), cos (degrees player.rotation) )
                            , angle = player.rotation
                            , speed = 0.09
                            , damage = 100
                            }
                                :: player.projectiles

                        else
                            player.projectiles
                    , rotation = player.rotation + (model.deltaTime * player.rotateSpeed * rotate) |> cycleF 0 360
                    , position =
                        player.position
                            |> Tuple.mapFirst (xprime * player.moveSpeed * model.deltaTime |> (+))
                            |> Tuple.mapSecond (yprime * player.moveSpeed * model.deltaTime |> negate |> (+))
                    , pressedKeys = pressedKeys
                }
    }


purge : ( Float, Float ) -> List Projectile -> List Projectile
purge ( maxx, maxy ) =
    let
        beyondEdge ( x, y ) =
            x < 0 || y < 0 || x > maxx || y > maxy
    in
    List.filter (not << beyondEdge << .position)


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
