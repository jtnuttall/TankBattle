module Component.Collider exposing (Collider(..))


type alias AABB =
    { position :
        { x : Float
        , y : Float
        }
    , dimensions :
        { width : Float
        , height : Float
        }
    }


type alias Circle =
    { x : Float
    , y : Float
    , radius : Float
    }


type Collider
    = AABBCollider AABB
    | CircleCollider Circle
