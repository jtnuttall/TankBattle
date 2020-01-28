module Components.Gun exposing (..)


type alias Gun =
    { gunDims : ( Float, Float )
    , gunCenter : ( Float, Float )
    , timeSinceFiring : Float
    , firingInterval : Float
    }


init : ( Float, Float ) -> ( Float, Float ) -> Gun
init ( playerWidth, playerHeight ) playerPosition =
    { gunDims = ( playerWidth / 4, playerHeight / 2 )
    , gunCenter = gunCenter playerWidth playerHeight playerPosition
    , timeSinceFiring = 0
    , firingInterval = 0.5
    }


gunCenter : Float -> Float -> ( Float, Float ) -> ( Float, Float )
gunCenter width height parentPosition =
    parentPosition
        |> Tuple.mapFirst (\x -> x + (3 * width) / 8)
        |> Tuple.mapSecond (\y -> y - height / 4)
