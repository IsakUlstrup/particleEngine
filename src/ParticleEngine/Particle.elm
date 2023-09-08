module ParticleEngine.Particle exposing
    ( Particle
    , new
    , step
    )

import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


{-| A particle meant to be used with Verlet integration
-}
type alias Particle =
    { position : Vector2
    , oldPosition : Vector2
    , mass : Float
    }


{-| Particle constructor
-}
new : Vector2 -> Float -> Particle
new position mass =
    Particle position position mass


{-| Step forwards using Verlet integration
-}
step : Vector2 -> Float -> Particle -> Particle
step force dt particle =
    { particle
        | oldPosition = particle.position
        , position =
            particle.position
                |> Vector2.scale 2
                |> Vector2.subtract particle.oldPosition
                |> Vector2.add (Vector2.scale (dt ^ 2) force)
    }
