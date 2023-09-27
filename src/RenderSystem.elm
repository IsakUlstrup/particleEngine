module RenderSystem exposing
    ( RenderSystem(..)
    , boundary
    , particleVelocity
    , particles
    , springStress
    , springs
    , toString
    )


type RenderSystem
    = RenderParticles
    | RenderParticleVelocity
    | RenderSprings Float
    | RenderSpringStress
    | RenderBoundary


particles : RenderSystem
particles =
    RenderParticles


particleVelocity : RenderSystem
particleVelocity =
    RenderParticleVelocity


springs : Float -> RenderSystem
springs width =
    RenderSprings width


springStress : RenderSystem
springStress =
    RenderSpringStress


boundary : RenderSystem
boundary =
    RenderBoundary


toString : RenderSystem -> String
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

        RenderBoundary ->
            "RenderBoundary"
