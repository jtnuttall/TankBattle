module Components.GameCanvas exposing (canvas)

import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Model exposing (Model)
import Utility exposing (mapTuple)


canvas : Model -> List Renderable
canvas model =
    []
        |> screen model.gameDims Color.black
        |> player1 model


screen : ( Float, Float ) -> Color.Color -> List Renderable -> List Renderable
screen ( width, height ) color renderable =
    renderable
        ++ [ shapes [ fill color ]
                [ rect ( 0, 0 )
                    width
                    height
                ]
           ]


player1 : Model -> List Renderable -> List Renderable
player1 model renderable =
    renderable
        ++ [ shapes [ fill Color.red ]
                [ rect model.player.position
                    50
                    50
                ]
           ]
