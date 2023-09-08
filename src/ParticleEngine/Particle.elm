module ParticleEngine.Particle exposing
    ( Particle
    , new
    )

import ParticleEngine.Vector2 exposing (Vector2)


type alias Particle =
    { position : Vector2
    , oldPosition : Vector2
    , mass : Float
    }


new : Vector2 -> Float -> Particle
new position mass =
    Particle position position mass
