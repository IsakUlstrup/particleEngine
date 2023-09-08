module ParticleEngine.Particle exposing (Particle)

import ParticleEngine.Vector2 exposing (Vector2)


type alias Particle =
    { position : Vector2
    , oldPosition : Vector2
    , mass : Float
    }
