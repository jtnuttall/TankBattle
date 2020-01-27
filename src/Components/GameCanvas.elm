module Components.GameCanvas exposing (canvas)

import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced as Canvas exposing (rotate, scale, transform, translate)
import Color
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

        ( x, y ) =
            player.position

        center =
            ( x + sizex / 2, y + sizey / 2 )
    in
    renderable
        ++ [ shapes
                [ fill Color.red
                , transform <|
                    List.concat
                        [ rotateAround center (degrees player.rotation)
                        ]
                ]
                [ rect model.player.position
                    sizex
                    sizey
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
