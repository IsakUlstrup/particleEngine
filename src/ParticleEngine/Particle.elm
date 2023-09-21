module ParticleEngine.Particle exposing
    ( Particle
    , applyForce
    , constrain
    , enforceConstraint
    , new
    , radius
    , step
    )

import ParticleEngine.Boundary as Boundary exposing (Boundary)
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


applyForce : Vector2 -> Particle -> Particle
applyForce force particle =
    { particle | acceleration = Vector2.add particle.acceleration (Vector2.divide particle.mass force) }


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


massRatio : Particle -> Particle -> Float
massRatio target particle =
    if particle.mass == 0 then
        0

    else if target.mass == 0 then
        1

    else
        ((particle.mass / target.mass) + 1) * 0.5


enforceConstraint : Spring -> ( Particle, Particle ) -> ( Particle, Particle )
enforceConstraint spring ( p1, p2 ) =
    let
        deltaDistance : Float
        deltaDistance =
            Vector2.distance p1.position p2.position - spring.length

        offset : Vector2
        offset =
            Vector2.direction p1.position p2.position
                |> Vector2.scale (deltaDistance * spring.rate)
                |> Vector2.divide 2
    in
    ( { p1 | position = p1.position |> Vector2.add (Vector2.scale (massRatio p2 p1) offset) }
    , { p2 | position = p2.position |> Vector2.subtract (Vector2.scale (massRatio p1 p2) offset) }
    )
