module Component.Player exposing (..)

import Component.Collider as Collider exposing (Collider(..))
import Component.Gun as Gun exposing (Gun)
import Component.Projectile as Projectile exposing (Projectile)
import Lib.Keyboard exposing (Key(..), KeyChange(..))
import Lib.Keyboard.Arrows exposing (arrowKey, wasd)
import Msg exposing (Msg)
import Utility exposing (clampF, cycleF, uncurry)


type alias Player =
    { playerId : Int
    , playerName : String
    , score : Int
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



-- TODO: Use texture width and height, not hardcoded 128


init : ( Float, Float ) -> Int -> String -> ( Player, Cmd Msg )
init position playerId playerName =
    let
        width =
            128

        height =
            128

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
      , score = 0
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



-- position = Gun.end player.position apparentDims player.rotation player.gun


updateProjectiles : Float -> ( Float, Float ) -> Player -> Player
updateProjectiles deltaTime gameDims player =
    let
        gun =
            player.gun

        shouldFire =
            List.member Spacebar player.pressedKeys
                && gun.timeSinceFiring
                > gun.firingInterval

        newProjectile projectiles =
            if shouldFire then
                { position = Gun.end player.position player.size player.rotation player.gun
                , direction = forward player
                , speed = 500
                , damage = 100
                }
                    :: projectiles

            else
                projectiles
    in
    { player
        | projectiles =
            player.projectiles
                |> Projectile.cull gameDims
                |> List.map (Projectile.update deltaTime)
                |> newProjectile
        , gun =
            { gun
                | timeSinceFiring =
                    if shouldFire then
                        0

                    else
                        player.gun.timeSinceFiring + deltaTime
            }
    }


transform : Float -> ( Float, Float ) -> Player -> Player
transform deltaTime ( gameWidth, gameHeight ) player =
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
                |> Tuple.mapFirst (clampF 0 gameWidth)
                |> Tuple.mapSecond (clampF 0 gameHeight)
    }
