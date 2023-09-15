module SidebarView exposing (viewDetails, viewLabeledInput, viewVector2Input)

import Html exposing (Html, details, div, input, label, strong, summary, text)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onInput)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


viewDetails : String -> List (Html msg) -> Html msg
viewDetails summaryText content =
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


viewVector2Input : Vector2 -> (Vector2 -> msg) -> Html msg
viewVector2Input vector inputMsg =
    div [ class "vector2-input" ]
        [ viewLabeledInput "number" (String.fromFloat vector.x) "x" (\i -> inputMsg (Vector2.mapX (always (Maybe.withDefault 0 (String.toFloat i))) vector))
        , viewLabeledInput "number" (String.fromFloat vector.y) "y" (\i -> inputMsg (Vector2.mapY (always (Maybe.withDefault 0 (String.toFloat i))) vector))
        ]
