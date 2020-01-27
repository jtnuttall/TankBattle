module Components.Projectile exposing (..)

import Utility exposing (mapTupleUniform)


type alias Projectile =
    { position : ( Float, Float )
    , direction : ( Float, Float )
    , speed : Float
    , damage : Float
    }


update : Float -> Projectile -> Projectile
update deltaTime projectile =
    let
        ( oldx, oldy ) =
            projectile.position

        newx =
            oldx + projectile.speed * deltaTime

        newy =
            oldy + projectile.speed * deltaTime
    in
    { projectile | position = ( newx, newy ) }
