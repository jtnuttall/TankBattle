module Components.GameCanvas exposing (canvas)

import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced as Canvas exposing (rotate, scale, transform, translate)
import Color
import Components.Player as Player
import Components.Projectile exposing (Projectile)
import Model exposing (Model)
import Utility exposing (mapTuple, mapTupleUniform, uncurry)


canvas : Model -> List Renderable
canvas model =
    []
        |> screen model.gameDims Color.black
        |> tank model
        |> (if model.isPaused then
                pauseOverlay model

            else
                identity
           )
        |> renderProjectiles model.player.projectiles


screen : ( Float, Float ) -> Color.Color -> List Renderable -> List Renderable
screen ( width, height ) color renderable =
    renderable
        ++ [ shapes [ fill color ]
                [ rect ( 0, 0 )
                    width
                    height
                ]
           ]


rotateAround : ( Float, Float ) -> Float -> List Canvas.Transform
rotateAround ( centerx, centery ) degrees =
    [ translate centerx centery
    , rotate degrees
    , translate -centerx -centery
    ]


tank : Model -> List Renderable -> List Renderable
tank model renderable =
    let
        player =
            model.player

        ( sizex, sizey ) =
            player.size

        gunSizex =
            sizex / 4

        gunSizey =
            sizey / 2

        gunPos =
            player.position
                |> Tuple.mapFirst (\x -> x + (sizex - gunSizex) / 2)
                |> Tuple.mapSecond (\y -> y - sizey / 4)
    in
    renderable
        ++ [ shapes
                [ fill Color.red
                , transform <|
                    List.concat
                        [ rotateAround (Player.center player) (degrees player.rotation)
                        ]
                ]
                [ rect model.player.position
                    sizex
                    sizey
                ]
           , shapes
                [ fill Color.green
                , transform <|
                    List.concat
                        [ rotateAround (Player.center player) (degrees player.rotation)
                        ]
                ]
                [ rect gunPos
                    gunSizex
                    gunSizey
                ]
           ]


pauseOverlay : Model -> List Renderable -> List Renderable
pauseOverlay model renderable =
    let
        sizex =
            10

        sizey =
            100
    in
    renderable
        ++ [ shapes [ fill Color.white ]
                [ rect (mapTuple (\x -> x / 2 - 2 * sizex) (\y -> y / 2 - sizey) model.gameDims)
                    sizex
                    sizey
                , rect (mapTuple (\x -> x / 2 + 2 * sizex) (\y -> y / 2 - sizey) model.gameDims)
                    sizex
                    sizey
                ]
           ]


renderProjectiles : List Projectile -> List Renderable -> List Renderable
renderProjectiles projectiles renderable =
    renderable ++ [ shapes [ fill Color.blue ] <| List.map (\p -> circle p.position 5) projectiles ]
