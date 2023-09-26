module RenderSystem exposing (RenderSystem(..), boundary, particles, springs, toString)


type RenderSystem
    = RenderParticles
    | RenderSprings Float
    | RenderBoundary


particles : RenderSystem
particles =
    RenderParticles


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

        RenderSprings w ->
            "RenderSprings " ++ String.fromFloat w

        RenderBoundary ->
            "RenderBoundary"
