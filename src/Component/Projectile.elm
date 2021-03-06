module Component.Projectile exposing (..)


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

        ( dirx, diry ) =
            projectile.direction

        newx =
            oldx + dirx * projectile.speed * deltaTime

        newy =
            oldy + -diry * projectile.speed * deltaTime
    in
    { projectile | position = ( newx, newy ) }


cull : ( Float, Float ) -> List Projectile -> List Projectile
cull ( maxx, maxy ) =
    let
        beyondEdge ( x, y ) =
            x < 0 || y < 0 || x > maxx || y > maxy
    in
    List.filter (not << beyondEdge << .position)
