module Component.GameCanvas exposing (canvas)

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
import Utility exposing (mapTuple, mapTupleUniform, prettyInt, uncurry)


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
                        [ rotateAround (Player.center player) (radians player.rotation)
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
                        [ rotateAround (Player.center player) (radians player.rotation)
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
