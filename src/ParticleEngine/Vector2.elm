module ParticleEngine.Vector2 exposing
    ( Vector2
    , new
    )


type alias Vector2 =
    { x : Float
    , y : Float
    }


new : Float -> Float -> Vector2
new x y =
    Vector2 x y
