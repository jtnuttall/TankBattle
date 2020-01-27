module Components.Player exposing (..)

import Lib.Keyboard exposing (Key(..), KeyChange(..))
import Msg exposing (Msg)
import Utility exposing (mapTuple)


type alias Player =
    { playerId : Int
    , playerName : String
    , position : ( Float, Float )
    , rotation : Float
    , size : ( Float, Float )
    , moveSpeed : Float
    , rotateSpeed : Float
    , pressedKeys : List Key
    }


init : ( Float, Float ) -> Int -> String -> ( Player, Cmd Msg )
init position playerId playerName =
    ( { playerId = playerId
      , playerName = playerName
      , position = position
      , rotation = 0
      , size = ( 25, 40 )
      , moveSpeed = 200
      , rotateSpeed = 100
      , pressedKeys = []
      }
    , Cmd.none
    )
