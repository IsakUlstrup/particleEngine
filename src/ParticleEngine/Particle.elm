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


{-| Derive velocity vector based on old position
-}
velocity : Particle -> Vector2
velocity particle =
    particle.position
        |> Vector2.subtract particle.oldPosition


{-| Step forwards using Verlet integration
-}
step : Vector2 -> Float -> Particle -> Particle
step force dt particle =
    let
        acceleration : Vector2
        acceleration =
            Vector2.divide particle.mass force
    in
    { particle
        | oldPosition = particle.position
        , position =
            particle.position
                |> Vector2.add
                    (acceleration
                        |> Vector2.add (velocity particle)
                        |> Vector2.scale (dt ^ 2)
                    )
    }
