module RenderSystem exposing (RenderSystem(..), boundary, particles, springs, toString)


type RenderSystem
    = RenderParticles
    | RenderSprings
    | RenderBoundary


particles : RenderSystem
particles =
    RenderParticles


springs : RenderSystem
springs =
    RenderSprings


boundary : RenderSystem
boundary =
    RenderBoundary


toString : RenderSystem -> String
toString renderSystem =
    case renderSystem of
        RenderParticles ->
            "RenderParticles"

        RenderSprings ->
            "RenderSprings"

        RenderBoundary ->
            "RenderBoundary"
