module Msg exposing (Msg(..))

import Lib.Keyboard as Keyboard


type Msg
    = Frame Float
    | Resize Int Int
    | KeyPress Keyboard.Msg
    | NoOp
