module Components.Player exposing (..)

import Components.Character as Character exposing (Character)
import Msg exposing (Msg)
import Utility exposing (mapTuple)


type alias Player =
    { playerId : Int
    , playerName : String
    , character : Character
    , position : ( Float, Float )
    }


init : ( Float, Float ) -> Int -> String -> ( Player, Cmd Msg )
init position playerId playerName =
    let
        ( character, cmd ) =
            Character.init
    in
    ( { playerId = playerId
      , playerName = playerName
      , character = character
      , position = position
      }
    , Cmd.batch [ cmd ]
    )
