module Component.Gun exposing (..)


type alias Gun =
    { dimensions : ( Float, Float )
    , timeSinceFiring : Float
    , firingInterval : Float
    }


init : ( Float, Float ) -> ( Float, Float ) -> Gun
init ( playerWidth, playerHeight ) playerPosition =
    { dimensions = ( playerWidth / 4, playerHeight / 2 )

    --, gunCenter = gunCenter playerWidth playerHeight playerPosition
    , timeSinceFiring = 0
    , firingInterval = 0.5
    }


position : ( Float, Float ) -> ( Float, Float ) -> Gun -> ( Float, Float )
position playerPosition ( playerSizex, playerSizey ) { dimensions } =
    playerPosition
        |> Tuple.mapFirst (\x -> x + (playerSizex - Tuple.first dimensions) / 2)
        |> Tuple.mapSecond (\y -> y - (playerSizey - Tuple.second dimensions) / 2)


end : ( Float, Float ) -> ( Float, Float ) -> Float -> Gun -> ( Float, Float )
end playerPosition ( playerSizex, playerSizey ) rotation { dimensions } =
    playerPosition
        |> Tuple.mapFirst (\x0 -> x0 + (playerSizex * sin (degrees rotation)) + playerSizex / 2)
        |> Tuple.mapSecond (\y0 -> y0 - (playerSizex * cos (degrees rotation)) + playerSizey / 2)
