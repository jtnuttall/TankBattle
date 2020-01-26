module Msg exposing (Msg(..))

import Keyboard


type Msg
    = Frame Float
    | Resize Int Int
    | KeyPress Keyboard.Msg
    | NoOp
