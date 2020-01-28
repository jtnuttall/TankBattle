module Components.Player exposing (..)

import Components.Collider as Collider exposing (Collider)
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
    , size : ( Float, Float )
    , moveSpeed : Float
    , currentSpeed : Float
    , rotateSpeed : Float
    , pressedKeys : List Key
    , timeSinceFiring : Float
    , firingInterval : Float
    , isFiring : Bool
    , projectiles : List Projectile
    , collider : Collider
    }


init : ( Float, Float ) -> Int -> String -> ( Player, Cmd Msg )
init position playerId playerName =
    let
        width =
            25

        height =
            40

        aabb =
            { position =
                { x = Tuple.first position
                , y = Tuple.second position
                }
            , dimensions =
                { width = width
                , height = height
                }
            }
    in
    ( { playerId = playerId
      , playerName = playerName
      , position = position
      , rotation = 0
      , size = ( width, height )
      , moveSpeed = 75
      , currentSpeed = 0
      , rotateSpeed = 75
      , pressedKeys = []
      , timeSinceFiring = 0
      , firingInterval = 0.25
      , isFiring = False
      , projectiles = []
      , collider = { aabb = aabb }
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
