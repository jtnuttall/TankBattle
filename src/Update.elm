module Update exposing (update)

import Components.Player as Player exposing (Player)
import Components.Projectile as Projectile exposing (Projectile)
import Lib.Keyboard as Keyboard exposing (Key(..), KeyChange(..), KeyParser, RawKey(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Model exposing (Model)
import Msg exposing (Msg(..))


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame deltaTime ->
            if model.isPaused then
                ( model, Cmd.none )

            else
                deltaTimeUpdate (deltaTime / 1000) model

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


deltaTimeUpdate : Float -> Model -> ( Model, Cmd Msg )
deltaTimeUpdate deltaTime model =
    let
        player =
            model.player

        gun =
            player.gun

        shouldFire =
            List.member Spacebar player.pressedKeys
                && gun.timeSinceFiring
                > gun.firingInterval

        newProjectile projectiles =
            if shouldFire then
                { position = Player.endOfGun player
                , direction = Player.forward player
                , speed = 100
                , damage = 100
                }
                    :: projectiles

            else
                projectiles
    in
    ( { model
        | deltaTime = deltaTime
        , player =
            { player
                | projectiles =
                    player.projectiles
                        |> Projectile.cull model.gameDims
                        |> List.map (Projectile.update deltaTime)
                        |> newProjectile
                , gun =
                    { gun
                        | timeSinceFiring =
                            if shouldFire then
                                0

                            else
                                player.gun.timeSinceFiring + deltaTime
                    }

                --, gunCenter = uncurry Player.gunCenter player.gunDims player.position
            }
                |> Player.translate deltaTime
      }
    , Cmd.none
    )


playerMoveUpdate : Keyboard.Msg -> Model -> Model
playerMoveUpdate key model =
    let
        ( pressedKeys, keyChange ) =
            Keyboard.updateWithKeyChange gameKeys key model.player.pressedKeys

        player =
            model.player

        { x, y } =
            wasd pressedKeys

        ( dirx, diry ) =
            Player.forward player
    in
    { model
        | isPaused =
            if List.member Escape pressedKeys then
                not model.isPaused

            else
                model.isPaused
        , player = { player | pressedKeys = pressedKeys }
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
