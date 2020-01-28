module Components.Player exposing (..)

import Components.Projectile as Projectile exposing (Projectile)
import Lib.Keyboard exposing (Key(..), KeyChange(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Msg exposing (Msg)
import Utility exposing (cycleF, mapTuple)


type alias Player =
    { playerId : Int
    , playerName : String
    , position : ( Float, Float )
    , rotation : Float
    , direction : ( Float, Float )
    , size : ( Float, Float )
    , isMoving : Bool
    , moveSpeed : Float
    , currentSpeed : Float
    , rotateSpeed : Float
    , pressedKeys : List Key
    , timeSinceFiring : Float
    , firingInterval : Float
    , isFiring : Bool
    , projectiles : List Projectile
    }


init : ( Float, Float ) -> Int -> String -> ( Player, Cmd Msg )
init position playerId playerName =
    ( { playerId = playerId
      , playerName = playerName
      , position = position
      , rotation = 0
      , direction = ( 0, 1 )
      , size = ( 25, 40 )
      , isMoving = False
      , moveSpeed = 200
      , currentSpeed = 0
      , rotateSpeed = 200
      , pressedKeys = []
      , timeSinceFiring = 0
      , firingInterval = 0.25
      , isFiring = False
      , projectiles = []
      }
    , Cmd.none
    )


center : Player -> ( Float, Float )
center player =
    let
        ( sizex, sizey ) =
            player.size

        ( x, y ) =
            player.position
    in
    ( x + sizex / 2, y + sizey / 2 )


translate : Float -> Player -> Player
translate deltaTime player =
    let
        { x, y } =
            wasd player.pressedKeys

        rotate =
            toFloat x

        move =
            toFloat y

        dirx =
            sin (degrees player.rotation)

        diry =
            cos (degrees player.rotation)

        xprime =
            move * dirx

        yprime =
            move * diry
    in
    { player
        | rotation = player.rotation + (deltaTime * player.rotateSpeed * rotate) |> cycleF 0 360
        , position =
            player.position
                |> Tuple.mapFirst (xprime * player.moveSpeed * deltaTime |> (+))
                |> Tuple.mapSecond (yprime * player.moveSpeed * deltaTime |> negate |> (+))
    }
