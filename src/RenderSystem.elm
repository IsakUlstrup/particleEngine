module RenderSystem exposing
    ( RenderSystem(..)
    , boundary
    , particleVelocity
    , particles
    , springs
    , toString
    )


type RenderSystem
    = RenderParticles
    | RenderParticleVelocity
    | RenderSprings Float
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

        RenderBoundary ->
            "RenderBoundary"
