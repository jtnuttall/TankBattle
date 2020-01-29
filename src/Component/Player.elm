module Component.Player exposing (..)

import Component.Collider as Collider exposing (Collider(..))
import Component.Gun as Gun exposing (Gun)
import Component.Projectile as Projectile exposing (Projectile)
import Lib.Keyboard exposing (Key(..), KeyChange(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Msg exposing (Msg)
import Utility exposing (cycleF, mapTuple)


type alias Player =
    { playerId : Int
    , playerName : String
    , position : ( Float, Float )
    , rotation : Float
    , size : ( Float, Float )
    , moveSpeed : Float
    , rotateSpeed : Float
    , pressedKeys : List Key
    , projectiles : List Projectile
    , collider : Collider
    , gun : Gun
    }


init : ( Float, Float ) -> Int -> String -> ( Player, Cmd Msg )
init position playerId playerName =
    let
        width =
            25

        height =
            40

        aabb =
            { position =
                { x = Tuple.first position
                , y = Tuple.second position
                }
            , dimensions =
                { width = width
                , height = height
                }
            }
    in
    ( { playerId = playerId
      , playerName = playerName
      , position = position
      , rotation = 0
      , size = ( width, height )
      , moveSpeed = 100
      , rotateSpeed = 2
      , pressedKeys = []
      , projectiles = []
      , collider = AABBCollider aabb
      , gun = Gun.init ( width, height ) position
      }
    , Cmd.none
    )


forward : Player -> ( Float, Float )
forward player =
    ( sin player.rotation, cos player.rotation )


center : Player -> ( Float, Float )
center player =
    let
        ( sizex, sizey ) =
            player.size

        ( x, y ) =
            player.position
    in
    ( x + sizex / 2, y + sizey / 2 )


transform : Float -> Player -> Player
transform deltaTime player =
    let
        { x, y } =
            wasd player.pressedKeys

        rotate =
            toFloat x

        move =
            toFloat y

        dirx =
            sin player.rotation

        diry =
            cos player.rotation

        xprime =
            move * dirx

        yprime =
            move * diry
    in
    { player
        | rotation = player.rotation + (deltaTime * player.rotateSpeed * rotate) |> cycleF 0 (2 * pi)
        , position =
            player.position
                |> Tuple.mapFirst (xprime * player.moveSpeed * deltaTime |> (+))
                |> Tuple.mapSecond (yprime * player.moveSpeed * deltaTime |> negate |> (+))
    }
