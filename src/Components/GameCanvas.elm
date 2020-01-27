module Components.GameCanvas exposing (canvas)

import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced as Canvas exposing (rotate, scale, transform, translate)
import Color
import Model exposing (Model)
import Utility exposing (mapTupleUniform, uncurry)


canvas : Model -> List Renderable
canvas model =
    []
        |> screen model.gameDims Color.black
        |> renderTank model


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


renderTank : Model -> List Renderable -> List Renderable
renderTank model renderable =
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
                        [ rotateAround center player.rotation
                        ]
                ]
                [ rect model.player.position
                    sizex
                    sizey
                ]
           ]
