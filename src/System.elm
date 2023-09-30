module System exposing
    ( System(..)
    , breakSprings
    , constrain
    , force
    , gravity
    , particleVelocity
    , particles
    , spawnParticle
    , springStress
    , springs
    , toString
    )

import ParticleEngine.Boundary exposing (Boundary)
import ParticleEngine.Particle exposing (Particle)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


type System
    = RenderParticles
    | RenderParticleVelocity
    | RenderSprings Float
    | RenderSpringStress
    | ConstrainParticles Boundary
    | Force Vector2
    | Gravity Vector2
    | BreakSprings
    | SpawnParticle ( Float, Float ) Particle


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


gravity : Vector2 -> System
gravity f =
    Gravity f


breakSprings : System
breakSprings =
    BreakSprings


spawnParticle : Float -> Particle -> System
spawnParticle cooldown particle =
    SpawnParticle ( cooldown, cooldown ) particle


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

        Gravity f ->
            "Gravity " ++ Vector2.toString f

        BreakSprings ->
            "BreakSprings"

        SpawnParticle ( cd, maxCd ) _ ->
            "SpawnParticle " ++ String.fromFloat cd ++ "/" ++ String.fromFloat maxCd
