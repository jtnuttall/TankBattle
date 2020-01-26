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
        |> player2 model


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
    let
        position =
            Debug.log "position" model.player.position
    in
    renderable
        ++ [ shapes [ fill Color.red ]
                [ rect model.player.position
                    100
                    100
                ]
           ]


player2 : Model -> List Renderable -> List Renderable
player2 model renderable =
    renderable ++ []
