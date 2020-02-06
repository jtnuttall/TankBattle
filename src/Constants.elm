module Constants exposing (..)

--import Keyboard exposing (Key(..))
-- to be imported qualified


textureWidth : Float
textureWidth =
    128


textureHeight : Float
textureHeight =
    128


gunAnimationStep : Float
gunAnimationStep =
    0.055


bodyAnimationStep : Float
bodyAnimationStep =
    0.1


gunStart : Int
gunStart =
    3


gunEnd : Int
gunEnd =
    10


gunFrames : Int
gunFrames =
    gunEnd - gunStart + 1


bodyStart : Int
bodyStart =
    0


bodyEnd : Int
bodyEnd =
    2
