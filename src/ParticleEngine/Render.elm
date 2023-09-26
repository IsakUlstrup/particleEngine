module ParticleEngine.Render exposing
    ( RenderConfig
    , setHeight
    , setWidth
    , viewWorld
    )

import ParticleEngine.World exposing (World)
import Svg exposing (Svg)
import Svg.Attributes


type alias RenderConfig =
    { width : Float
    , height : Float
    }


setWidth : Float -> RenderConfig -> RenderConfig
setWidth width config =
    { config | width = width }


setHeight : Float -> RenderConfig -> RenderConfig
setHeight height config =
    { config | height = height }


viewBox : RenderConfig -> Svg.Attribute msg
viewBox config =
    Svg.Attributes.viewBox <|
        String.fromFloat -(config.width / 2)
            ++ " "
            ++ String.fromFloat -(config.height / 2)
            ++ " "
            ++ String.fromFloat config.width
            ++ " "
            ++ String.fromFloat config.height


viewWorld : (World renderSystem -> renderSystem -> Svg msg) -> RenderConfig -> World renderSystem -> Svg msg
viewWorld runRenderSystem config world =
    Svg.svg
        [ viewBox config
        , Svg.Attributes.id "game-view"
        ]
        (world.renderSystems
            |> List.filter Tuple.first
            |> List.map Tuple.second
            |> List.map (runRenderSystem world)
        )
