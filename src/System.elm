module System exposing
    ( System(..)
    , constrain
    , force
    , particleVelocity
    , particles
    , springStress
    , springs
    , toString
    )

import ParticleEngine.Boundary exposing (Boundary)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


type System
    = RenderParticles
    | RenderParticleVelocity
    | RenderSprings Float
    | RenderSpringStress
    | ConstrainParticles Boundary
    | Force Vector2


particles : System
particles =
    RenderParticles


particleVelocity : System
particleVelocity =
    RenderParticleVelocity


springs : Float -> System
springs width =
    RenderSprings width


springStress : System
springStress =
    RenderSpringStress


constrain : Boundary -> System
constrain b =
    ConstrainParticles b


force : Vector2 -> System
force f =
    Force f


toString : System -> String
toString renderSystem =
    case renderSystem of
        RenderParticles ->
            "RenderParticles"

        RenderParticleVelocity ->
            "RenderParticleVelocity"

        RenderSprings w ->
            "RenderSprings " ++ String.fromFloat w

        RenderSpringStress ->
            "RenderSpringStress"

        ConstrainParticles _ ->
            "ConstrainParticles"

        Force f ->
            "Force " ++ Vector2.toString f
