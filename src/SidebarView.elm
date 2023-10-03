module SidebarView exposing (buttonGroup, viewLabeledInput, viewLabeledNumberInput, viewSidebar, viewVector2Input)

import Html exposing (Html, details, div, input, label, strong, summary, text)
import Html.Attributes exposing (class, for, id, step, type_, value)
import Html.Events exposing (onInput)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


viewDetails : ( String, List (Html msg) ) -> Html msg
viewDetails ( summaryText, content ) =
    details []
        [ summary [] [ strong [] [ text summaryText ] ]
        , div [ class "content" ] content
        ]


viewLabeledInput : String -> String -> String -> (String -> msg) -> Html msg
viewLabeledInput inputType inputValue labelValue inputMsg =
    div [ class "labeled-input" ]
        [ label [ for labelValue ] [ text labelValue ]
        , input
            [ id labelValue
            , type_ inputType
            , onInput inputMsg
            , value inputValue
            ]
            []
        ]


viewLabeledNumberInput : Float -> Float -> String -> (Float -> msg) -> Html msg
viewLabeledNumberInput stepSize inputValue labelValue inputMsg =
    div [ class "labeled-input" ]
        [ label [ for labelValue ] [ text labelValue ]
        , input
            [ id labelValue
            , type_ "number"
            , onInput (String.toFloat >> Maybe.withDefault 0 >> inputMsg)
            , value (String.fromFloat inputValue)
            , step (String.fromFloat stepSize)
            ]
            []
        ]


viewVector2Input : Vector2 -> (Vector2 -> msg) -> Html msg
viewVector2Input vector inputMsg =
    div [ class "vector2-input" ]
        [ viewLabeledInput "number" (String.fromFloat vector.x) "x" (\i -> inputMsg (Vector2.mapX (always (Maybe.withDefault 0 (String.toFloat i))) vector))
        , viewLabeledInput "number" (String.fromFloat vector.y) "y" (\i -> inputMsg (Vector2.mapY (always (Maybe.withDefault 0 (String.toFloat i))) vector))
        ]


buttonGroup : List (Html msg) -> Html msg
buttonGroup buttons =
    div [ class "button-group" ] buttons


viewSidebar : List ( String, List (Html msg) ) -> Html msg
viewSidebar children =
    Html.section [ Html.Attributes.class "sidebar" ]
        (List.map viewDetails children)
