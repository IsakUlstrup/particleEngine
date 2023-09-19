module ParticleEngine.Particle exposing
    ( Particle
    , applyForce
    , constrain
    , enforceConstraint
    , new
    , radius
    , step
    )

import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


{-| Particle radius constant
-}
radius : Float
radius =
    10


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


constrain : Float -> Float -> Particle -> Particle
constrain width height particle =
    let
        vel : Vector2
        vel =
            velocity particle
    in
    if particle.position.x + radius > width then
        { particle
            | position = Vector2.mapX (always (width - radius)) particle.position
            , oldPosition = Vector2.mapX (always <| (width - radius) + vel.x) particle.oldPosition
        }

    else if particle.position.x - radius < -width then
        { particle
            | position = Vector2.mapX (always -(width - radius)) particle.position
            , oldPosition = Vector2.mapX (always <| -(width - radius) + vel.x) particle.oldPosition
        }

    else if particle.position.y + radius > height then
        { particle
            | position = Vector2.mapY (always (height - radius)) particle.position
            , oldPosition = Vector2.mapY (always <| (height - radius) + vel.y) particle.oldPosition
        }

    else if particle.position.y - radius < -height then
        { particle
            | position = Vector2.mapY (always -(height - radius)) particle.position
            , oldPosition = Vector2.mapY (always <| -(height - radius) + vel.y) particle.oldPosition
        }

    else
        particle


enforceConstraint : Float -> ( Particle, Particle ) -> ( Particle, Particle )
enforceConstraint length ( p1, p2 ) =
    let
        deltaDistance : Float
        deltaDistance =
            Vector2.distance p1.position p2.position - length

        offset : Vector2
        offset =
            Vector2.direction p1.position p2.position
                |> Vector2.scale (deltaDistance * 0.2)
                |> Vector2.divide 2
    in
    ( { p1 | position = p1.position |> Vector2.add offset }
    , { p2 | position = p2.position |> Vector2.subtract offset }
    )
