module Msg exposing (Msg(..))

import Canvas.Texture as Canvas
import Lib.Keyboard as Keyboard


type Msg
    = Frame Float
    | Resize Int Int
    | KeyPress Keyboard.Msg
    | TextureLoaded (Maybe Canvas.Texture)
    | NoOp
