module ParticleEngine.Boundary exposing (Boundary, bottom, left, new, right, setHeight, setWidth, top)

import ParticleEngine.Vector2 exposing (Vector2)


type alias Boundary =
    { center : Vector2
    , width : Float
    , height : Float
    }


new : Vector2 -> Float -> Float -> Boundary
new center width height =
    Boundary center width height


setWidth : Float -> Boundary -> Boundary
setWidth width boundary =
    { boundary | width = width }


setHeight : Float -> Boundary -> Boundary
setHeight height boundary =
    { boundary | height = height }


{-| Get left side of boundary in world coordinates
-}
left : Boundary -> Float
left boundary =
    boundary.center.x - (boundary.width / 2)


{-| Get right side of boundary in world coordinates
-}
right : Boundary -> Float
right boundary =
    boundary.center.x + (boundary.width / 2)


{-| Get top side of boundary in world coordinates
-}
top : Boundary -> Float
top boundary =
    boundary.center.y - (boundary.height / 2)


{-| Get bottom side of boundary in world coordinates
-}
bottom : Boundary -> Float
bottom boundary =
    boundary.center.y + (boundary.height / 2)
