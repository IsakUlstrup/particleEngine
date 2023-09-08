module ParticleEngine.Vector2 exposing
    ( Vector2
    , add
    , divide
    , new
    , scale
    , subtract
    )


type alias Vector2 =
    { x : Float
    , y : Float
    }


new : Float -> Float -> Vector2
new x y =
    Vector2 x y


{-| Add the components of two vectors together
-}
add : Vector2 -> Vector2 -> Vector2
add v1 v2 =
    { v1 | x = v1.x + v2.x, y = v1.y + v2.y }


{-| Subtract the components of sub from vector
-}
subtract : Vector2 -> Vector2 -> Vector2
subtract sub vector =
    { vector | x = vector.x - sub.x, y = vector.y - sub.y }


{-| Multiply components by given value
-}
scale : Float -> Vector2 -> Vector2
scale amount vector =
    { vector | x = vector.x * amount, y = vector.y * amount }


{-| Divide components by given value
-}
divide : Float -> Vector2 -> Vector2
divide value vector =
    { vector | x = vector.x / value, y = vector.y / value }
