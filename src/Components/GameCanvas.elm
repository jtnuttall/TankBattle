module Components.GameCanvas exposing (canvas)

import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced as Canvas exposing (rotate, scale, transform, translate)
import Color
import Components.Gun as Gun
import Components.Player as Player exposing (Player)
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
        |> renderProjectiles model.player


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

        ( gunSizex, gunSizey ) =
            ( sizex / 4, sizey / 2 )

        gunPos =
            Gun.position player.position player.size player.gun
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
                [ rect gunPos gunSizex gunSizey
                ]
           ]


pauseOverlay : Model -> List Renderable -> List Renderable
pauseOverlay model renderable =
    let
        ( pauseBarSizex, pauseBarSizey ) =
            ( 10, 100 )
    in
    renderable
        ++ [ shapes
                [ fill Color.white ]
                [ rect
                    (mapTuple
                        (\x -> x / 2 - 2 * pauseBarSizex)
                        (\y -> y / 2 - pauseBarSizey)
                        model.gameDims
                    )
                    pauseBarSizex
                    pauseBarSizey
                , rect
                    (mapTuple
                        (\x -> x / 2 + 2 * pauseBarSizex)
                        (\y -> y / 2 - pauseBarSizey)
                        model.gameDims
                    )
                    pauseBarSizex
                    pauseBarSizey
                ]
           ]
        ++ [ shapes
                [ fill Color.grey ]
                []
           ]


renderProjectiles : Player -> List Renderable -> List Renderable
renderProjectiles player renderable =
    renderable
        ++ [ player.projectiles
                |> List.map (\p -> circle p.position 5)
                |> shapes
                    [ fill Color.blue
                    , transform <|
                        List.concat
                            [--rotateAround (Player.center player) (degrees player.rotation)
                            ]
                    ]
           ]
