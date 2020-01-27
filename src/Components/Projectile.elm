module Components.Projectile exposing (..)


type alias Projectile =
    { position : ( Float, Float )
    , direction : ( Float, Float )
    , speed : Float
    , damage : Float
    }
