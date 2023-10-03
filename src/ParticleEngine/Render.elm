module ParticleEngine.Render exposing
    ( RenderConfig
    , setHeight
    , setPosition
    , setWidth
    , setZoom
    , viewWorld
    )

import ParticleEngine.Vector2 exposing (Vector2)
import ParticleEngine.World exposing (World)
import Svg exposing (Svg)
import Svg.Attributes


type alias RenderConfig =
    { width : Float
    , height : Float
    , cameraZoom : Float
    , cameraPosition : Vector2
    }


setWidth : Float -> RenderConfig -> RenderConfig
setWidth width config =
    { config | width = width }


setHeight : Float -> RenderConfig -> RenderConfig
setHeight height config =
    { config | height = height }


setZoom : Float -> RenderConfig -> RenderConfig
setZoom zoom config =
    { config | cameraZoom = zoom }


setPosition : Vector2 -> RenderConfig -> RenderConfig
setPosition position config =
    { config | cameraPosition = position }


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


cameraTransform : RenderConfig -> Svg.Attribute msg
cameraTransform config =
    Svg.Attributes.transform <|
        "scale("
            ++ String.fromFloat config.cameraZoom
            ++ ") translate("
            ++ String.fromFloat -config.cameraPosition.x
            ++ ", "
            ++ String.fromFloat -config.cameraPosition.y
            ++ ")"


viewWorld : (World a b -> a -> Maybe (Svg msg)) -> RenderConfig -> World a b -> Svg msg
viewWorld runSystem config world =
    Svg.svg
        [ viewBox config
        , Svg.Attributes.id "game-view"
        ]
        [ Svg.g
            [ Svg.Attributes.class "camera"
            , cameraTransform config
            ]
            (world.systems
                |> List.filter Tuple.first
                |> List.map Tuple.second
                |> List.filterMap (runSystem world)
            )
        ]
