module Components.Collider exposing (..)


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


type alias Collider =
    { aabb : AABB }
