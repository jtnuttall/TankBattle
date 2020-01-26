module Model exposing (Flags, Model, init)

import Components.Player as Player exposing (Player, init)
import Keyboard exposing (Key(..))
import Msg exposing (Msg(..))
import Utility exposing (mapTuple)


type alias Model =
    { count : Float
    , deltaTime : Float
    , isPaused : Bool
    , speed : Float
    , pressedKeys : List Key
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
            Player.init (mapTuple ((/) 2) ((/) 2) gameDims) 1 "__player1__"
    in
    ( { count = 0
      , deltaTime = 0
      , isPaused = False
      , speed = 200
      , pressedKeys = []
      , windowDims = ( flags.windowWidth, flags.windowHeight )
      , gameDims = gameDims
      , player = player1
      }
    , Cmd.batch [ player1Cmd ]
    )
