module Components.Player exposing (..)

import Components.Projectile as Projectile exposing (Projectile)
import Lib.Keyboard exposing (Key(..), KeyChange(..))
import Msg exposing (Msg)
import Utility exposing (mapTuple)


type alias Player =
    { playerId : Int
    , playerName : String
    , position : ( Float, Float )
    , rotation : Float
    , direction : ( Float, Float )
    , size : ( Float, Float )
    , moveSpeed : Float
    , currentSpeed : Float
    , rotateSpeed : Float
    , pressedKeys : List Key
    , timeSinceFiring : Float
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
      , moveSpeed = 200
      , currentSpeed = 0
      , rotateSpeed = 200
      , pressedKeys = []
      , timeSinceFiring = 0
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
