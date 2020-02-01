module Component.Gun exposing (..)


type alias Gun =
    { dimensions : ( Float, Float )
    , timeSinceFiring : Float
    , firingInterval : Float
    , isFiring : Bool
    }


init : ( Float, Float ) -> ( Float, Float ) -> Gun
init ( playerWidth, playerHeight ) playerPosition =
    { dimensions = ( playerWidth / 4, playerHeight / 2 )
    , timeSinceFiring = 0
    , firingInterval = 0.5
    , isFiring = False
    }


end : ( Float, Float ) -> ( Float, Float ) -> Float -> Gun -> ( Float, Float )
end playerPosition ( playerSizex, playerSizey ) rotation { dimensions } =
    playerPosition
        |> Tuple.mapFirst (\x0 -> x0 + (playerSizex * 0.45 * sin rotation) + playerSizex / 2)
        |> Tuple.mapSecond (\y0 -> y0 - (playerSizex * 0.45 * cos rotation) + playerSizey / 2)
