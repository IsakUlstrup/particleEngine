module SidebarView exposing (viewDetails)

import Html exposing (Html, details, div, strong, summary, text)
import Html.Attributes exposing (class)


viewDetails : String -> List (Html msg) -> Html msg
viewDetails summaryText content =
    details []
        [ summary [] [ strong [] [ text summaryText ] ]
        , div [ class "content" ] content
        ]
