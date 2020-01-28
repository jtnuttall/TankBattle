module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Components.GameCanvas as GameCanvas
import Components.Player as Player
import Components.Projectile as Projectile exposing (Projectile)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyChange(..), KeyParser, RawKey(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import Utility exposing (cycleF, flip)


gameKeys : KeyParser
gameKeys =
    Keyboard.oneOf [ arrowKey, Keyboard.uiKey, Keyboard.whitespaceKey ]


moveKeys : List Key
moveKeys =
    [ Character "w"
    , Character "a"
    , Character "s"
    , Character "d"
    ]



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
                                    |> cull model.gameDims
                                    |> List.map (Projectile.update deltaTime)
                        }
                            |> Player.translate (deltaTime / 1000)
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
            ( playerMoveUpdate key model, Cmd.none )

        _ ->
            ( model, Cmd.none )


playerMoveUpdate : Keyboard.Msg -> Model -> Model
playerMoveUpdate key model =
    let
        ( pressedKeys, keyChange ) =
            Keyboard.updateWithKeyChange gameKeys key model.player.pressedKeys

        player =
            model.player

        { x, y } =
            wasd pressedKeys

        dirx =
            sin (degrees player.rotation)

        diry =
            cos (degrees player.rotation)
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
                        if List.member Spacebar pressedKeys && player.timeSinceFiring > player.firingInterval then
                            0

                        else
                            player.timeSinceFiring
                    , projectiles =
                        if List.member Spacebar pressedKeys && player.timeSinceFiring > player.firingInterval then
                            { position = Player.center model.player
                            , direction = ( dirx, diry )
                            , speed = 0.09
                            , damage = 100
                            }
                                :: player.projectiles

                        else
                            player.projectiles
                    , currentSpeed =
                        if x > 0 then
                            player.moveSpeed

                        else if x < 0 then
                            -player.moveSpeed

                        else
                            0
                    , pressedKeys = pressedKeys
                }
    }


isMovingKeyUp : List Key -> Key -> Bool
isMovingKeyUp pressedKeys upKey =
    let
        upKeyPrime =
            Keyboard.map String.toLower upKey

        filteredKeys =
            pressedKeys
                |> List.map (Keyboard.map String.toLower)
                |> List.filter
                    (\pressedKey ->
                        Keyboard.map String.toLower upKeyPrime
                            /= pressedKey
                    )

        { x, y } =
            wasd filteredKeys
    in
    x /= 0 || y /= 0


cull : ( Float, Float ) -> List Projectile -> List Projectile
cull ( maxx, maxy ) =
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
        [ Sub.map KeyPress Keyboard.subscriptions
        , if model.isPaused then
            Sub.none

          else
            onAnimationFrameDelta Frame
        , onResize Resize
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
