module ParticleEngine.Particle exposing
    ( Particle
    , applyForce
    , applyForces
    , applySpringForce
    , constrain
    , new
    , radius
    , step
    )

import ParticleEngine.Boundary as Boundary exposing (Boundary)
import ParticleEngine.Force exposing (Force(..))
import ParticleEngine.Spring exposing (Spring)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


{-| Particle radius constant
-}
radius : Float
radius =
    15


{-| A particle meant to be used with Verlet integration
-}
type alias Particle =
    { position : Vector2
    , oldPosition : Vector2
    , acceleration : Vector2
    , mass : Float
    }


{-| Particle constructor
-}
new : Vector2 -> Float -> Particle
new position mass =
    Particle position position Vector2.zero mass


applyForce : Force -> Particle -> Particle
applyForce force particle =
    if particle.mass /= 0 then
        case force of
            Realative f ->
                { particle | acceleration = Vector2.add particle.acceleration (Vector2.divide particle.mass f) }

            Absolute f ->
                { particle | acceleration = Vector2.add particle.acceleration f }

    else
        particle


applyForces : List Force -> Particle -> Particle
applyForces forces particle =
    List.foldl applyForce particle forces


{-| Derive velocity vector based on old position
-}
velocity : Particle -> Vector2
velocity particle =
    particle.position
        |> Vector2.subtract particle.oldPosition


{-| Step forwards using Verlet integration
-}
step : Float -> Particle -> Particle
step dt particle =
    { particle
        | position =
            particle.position
                |> Vector2.add (velocity particle)
                |> Vector2.add (Vector2.scale (dt ^ 2) particle.acceleration)
        , oldPosition = particle.position
        , acceleration = Vector2.zero
    }


constrain : Boundary -> Particle -> Particle
constrain boundary particle =
    let
        vel : Vector2
        vel =
            velocity particle
    in
    if particle.position.x + radius > Boundary.right boundary then
        { particle
            | position = Vector2.mapX (always (Boundary.right boundary - radius)) particle.position
            , oldPosition = Vector2.mapX (always <| (Boundary.right boundary - radius) + vel.x) particle.oldPosition
        }

    else if particle.position.x - radius < Boundary.left boundary then
        { particle
            | position = Vector2.mapX (always (Boundary.left boundary + radius)) particle.position
            , oldPosition = Vector2.mapX (always <| (Boundary.left boundary + radius) + vel.x) particle.oldPosition
        }

    else if particle.position.y + radius > Boundary.bottom boundary then
        { particle
            | position = Vector2.mapY (always (Boundary.bottom boundary - radius)) particle.position
            , oldPosition = Vector2.mapY (always <| (Boundary.bottom boundary - radius) + vel.y) particle.oldPosition
        }

    else if particle.position.y - radius < Boundary.top boundary then
        { particle
            | position = Vector2.mapY (always (Boundary.top boundary + radius)) particle.position
            , oldPosition = Vector2.mapY (always <| (Boundary.top boundary + radius) + vel.y) particle.oldPosition
        }

    else
        particle


relativeVelocity : Particle -> Particle -> Vector2
relativeVelocity target particle =
    Vector2.subtract (velocity target) (velocity particle)


applySpringForce : Particle -> Spring -> Particle -> Particle
applySpringForce from spring particle =
    let
        deltaDistance : Float
        deltaDistance =
            Vector2.distance from.position particle.position - spring.length

        force : Vector2
        force =
            Vector2.direction from.position particle.position
                |> Vector2.scale deltaDistance
                |> Vector2.scale -spring.rate

        damperForce : Vector2
        damperForce =
            relativeVelocity from particle
                |> Vector2.scale -spring.damping

        sumForces : Vector2
        sumForces =
            Vector2.add force damperForce
    in
    applyForce (Realative sumForces) particle
