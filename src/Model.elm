module Model exposing (Flags, Model, init)

import Array exposing (Array)
import Canvas.Texture exposing (Texture)
import Component.Player as Player exposing (Player, init)
import Drawing.AnimationData exposing (AnimationData)
import Drawing.Sprites as Sprites exposing (Sprites)
import Msg exposing (Msg(..))


type alias Model =
    { count : Float
    , deltaTime : Float
    , isPaused : Bool
    , windowDims : ( Float, Float )
    , gameDims : ( Float, Float )
    , sprites : Sprites.Load Sprites
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
            ( flags.windowWidth, flags.windowHeight )

        playerPosition =
            let
                ( x, y ) =
                    gameDims
            in
            ( x / 2 - 64, y / 2 - 64 )

        ( player1, player1Cmd ) =
            Player.init playerPosition 1 "__player1__"
    in
    ( { count = 0
      , deltaTime = 0
      , isPaused = False
      , windowDims = ( flags.windowWidth, flags.windowHeight )
      , gameDims = gameDims
      , sprites = Sprites.Loading
      , player = player1
      }
    , Cmd.batch [ player1Cmd ]
    )
