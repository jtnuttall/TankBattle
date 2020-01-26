module Components.Character exposing (..)

import Msg exposing (Msg)


type alias Character =
    { name : String
    , position : ( Float, Float )
    }


init : ( Character, Cmd Msg )
init =
    ( { name = "__init__"
      , position = ( 0.0, 0.0 )
      }
    , Cmd.none
    )
