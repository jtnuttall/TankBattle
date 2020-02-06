module Update exposing (update)

import Array
import Canvas.Texture as Texture exposing (Texture)
import Component.Gun as Gun
import Component.Player as Player exposing (Player)
import Component.Projectile as Projectile exposing (Projectile)
import Constants
import Drawing.AnimationData as Animation exposing (AnimationCommand(..))
import Drawing.Sprites as Sprites exposing (Sprites)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyChange(..), KeyParser)
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import List.Extra as List
import Model exposing (Model)
import Msg exposing (Msg(..))
import Utility exposing (anyOf)


wasd : List Key
wasd =
    [ Character "W"
    , Character "A"
    , Character "S"
    , Character "D"
    , Character "w"
    , Character "a"
    , Character "s"
    , Character "d"
    ]


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
                    case player.keyChange of
                        Just (KeyDown Spacebar) ->
                            Animation.Run

                        Just (KeyUp Spacebar) ->
                            Animation.Finish

                        _ ->
                            if List.member Spacebar player.pressedKeys then
                                Animation.Run

                            else
                                Animation.NoChange
                , body =
                    if anyOf wasd player.pressedKeys then
                        Animation.Run

                    else
                        Animation.Stop

                --if List.any (List.map List.member wasd) then
                --    Animation.Run
                --else
                --    Animation.Stop
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
        ( pressedKeys, keyChange ) =
            Keyboard.updateWithKeyChange gameKeys key model.player.pressedKeys

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
        , player =
            { player
                | pressedKeys = pressedKeys
                , keyChange = keyChange
            }
    }
