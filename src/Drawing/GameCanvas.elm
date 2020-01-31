module Drawing.GameCanvas exposing (canvas)

import Array
import Canvas as Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced as Canvas exposing (rotate, scale, transform, translate)
import Canvas.Settings.Text as Canvas
import Canvas.Texture as Texture
import Color
import Component.Gun as Gun
import Component.Player as Player exposing (Player)
import Component.Projectile exposing (Projectile)
import Model exposing (Load(..), Model, Sprites)
import Utility exposing (mapTuple, prettyInt, uncurry, veryUglyUnsafeGet)


canvas : Model -> List Renderable
canvas model =
    case model.sprites of
        Loading ->
            []
                |> screen model.gameDims Color.white
                |> renderText model "Loading..."

        Failure ->
            []
                |> screen model.gameDims Color.white
                |> renderText model "Failure."

        Success sprites ->
            []
                |> screen model.gameDims Color.black
                |> renderTank model sprites
                |> renderProjectiles model.player
                |> (if model.isPaused then
                        pauseOverlay model

                    else
                        identity
                   )
                |> renderHud model


renderText : Model -> String -> List Renderable -> List Renderable
renderText model txt renderable =
    let
        ( w, h ) =
            model.gameDims
    in
    renderable
        ++ [ Canvas.text
                [ Canvas.font { size = 48, family = "sans-serif" }
                , Canvas.align Canvas.Center
                ]
                ( w / 2, h / 2 - 24 )
                txt
           ]


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
rotateAround ( centerx, centery ) radians =
    [ translate centerx centery
    , rotate radians
    , translate -centerx -centery
    ]


renderTank : Model -> Sprites -> List Renderable -> List Renderable
renderTank model sprites renderable =
    let
        player =
            model.player

        ( sizex, sizey ) =
            player.size

        body =
            veryUglyUnsafeGet 0 sprites.tank.body

        gun =
            veryUglyUnsafeGet 0 sprites.tank.gun
    in
    renderable
        ++ [ Canvas.texture
                [ transform <|
                    List.concat
                        [ rotateAround (Player.center player) (radians player.rotation)
                        ]
                ]
                player.position
                body
           , Canvas.texture
                [ transform <|
                    List.concat
                        [ rotateAround (Player.center player) (radians player.rotation)
                        ]
                ]
                player.position
                gun
           ]


pauseOverlay : Model -> List Renderable -> List Renderable
pauseOverlay model renderable =
    let
        ( pauseBarWidth, pauseBarHeight ) =
            ( 10, 100 )

        rightPauseBarPosition =
            model.gameDims
                |> Tuple.mapFirst (\x -> x / 2 - 2 * pauseBarWidth)
                |> Tuple.mapSecond (\y -> y / 2 - pauseBarHeight)

        leftPauseBarPosition =
            rightPauseBarPosition
                |> Tuple.mapFirst (\x -> x + 4 * pauseBarWidth)
    in
    renderable
        ++ [ shapes
                [ fill Color.white ]
                [ rect rightPauseBarPosition pauseBarWidth pauseBarHeight
                , rect leftPauseBarPosition pauseBarWidth pauseBarHeight
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
                            []
                    ]
           ]


renderHud : Model -> List Renderable -> List Renderable
renderHud model renderable =
    let
        ( width, height ) =
            model.gameDims
    in
    renderable
        ++ [ Canvas.text
                [ Canvas.font
                    { size = 14
                    , family = "monospace"
                    }
                , fill Color.white
                , Canvas.align Canvas.Left
                ]
                ( 30, 30 )
                model.player.playerName
           , Canvas.text
                [ Canvas.font
                    { size = 14
                    , family = "monospace"
                    }
                , Canvas.align Canvas.Center
                , fill Color.white
                ]
                ( width / 2, 30 )
                (if model.isPaused then
                    "PAUSED"

                 else
                    ""
                )
           , Canvas.text
                [ Canvas.font
                    { size = 14
                    , family = "monospace"
                    }
                , Canvas.align Canvas.Right
                , fill Color.white
                ]
                ( width - 30, 30 )
                ("SCORE: " ++ prettyInt model.player.score)
           ]
