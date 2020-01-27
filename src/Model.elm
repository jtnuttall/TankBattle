module Model exposing (Flags, Model, init)

import Components.Player as Player exposing (Player, init)
import Msg exposing (Msg(..))
import Utility exposing (mapTupleUniform)


type alias Model =
    { count : Float
    , deltaTime : Float
    , isPaused : Bool
    , windowDims : ( Float, Float )
    , gameDims : ( Float, Float )
    , player : Player
    }


type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        gameDims =
            ( 640, 480 )

        ( player1, player1Cmd ) =
            Player.init (mapTupleUniform (\x -> x / 2) gameDims) 1 "__player1__"
    in
    ( { count = 0
      , deltaTime = 0
      , isPaused = False
      , windowDims = ( flags.windowWidth, flags.windowHeight )
      , gameDims = gameDims
      , player = player1
      }
    , Cmd.batch [ player1Cmd ]
    )
