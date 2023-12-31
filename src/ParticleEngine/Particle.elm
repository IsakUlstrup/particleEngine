module ParticleEngine.Particle exposing
    ( Particle
    , applyForce
    , applyGravity
    , applySpringForce
    , constrain
    , enforceSpring
    , new
    , radius
    , setMass
    , setPosition
    , step
    , velocity
    )

import Dict exposing (Dict)
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
type alias Particle a =
    { position : Vector2
    , oldPosition : Vector2
    , acceleration : Vector2
    , mass : Float
    , components : List a
    }


{-| Particle constructor
-}
new : Vector2 -> Float -> Particle a
new position mass =
    Particle position position Vector2.zero mass []


applyForce : Vector2 -> Particle a -> Particle a
applyForce force particle =
    if particle.mass /= 0 then
        { particle | acceleration = Vector2.add particle.acceleration (Vector2.divide particle.mass force) }

    else
        particle


applyGravity : Vector2 -> Particle a -> Particle a
applyGravity force particle =
    if particle.mass /= 0 then
        { particle | acceleration = Vector2.add particle.acceleration force }

    else
        particle


{-| Derive velocity vector based on old position
-}
velocity : Particle a -> Vector2
velocity particle =
    Vector2.subtract particle.oldPosition particle.position


setPosition : Vector2 -> Particle a -> Particle a
setPosition position particle =
    { particle | position = position, oldPosition = Vector2.subtract (velocity particle) position }


setMass : Float -> Particle a -> Particle a
setMass mass particle =
    { particle | mass = max 0 mass }


{-| Step forwards using Verlet integration
-}
step : Float -> Particle a -> Particle a
step dt particle =
    { particle
        | position =
            particle.position
                |> Vector2.add (velocity particle)
                |> Vector2.add (Vector2.scale (dt ^ 2) particle.acceleration)
        , oldPosition = particle.position
        , acceleration = Vector2.zero
    }


constrain : Boundary -> Particle a -> Particle a
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


relativeVelocity : Particle a -> Particle a -> Vector2
relativeVelocity target particle =
    Vector2.subtract (velocity target) (velocity particle)


applySpringForce : Particle a -> Spring -> Particle a -> Particle a
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
                -- divide by two, so both particles together move by 1
                |> Vector2.scale 0.5

        damperForce : Vector2
        damperForce =
            relativeVelocity from particle
                |> Vector2.scale -spring.damping

        sumForces : Vector2
        sumForces =
            Vector2.add force damperForce
    in
    applyForce sumForces particle


enforceSpring : ( ( Int, Int ), Spring ) -> Dict Int (Particle b) -> Dict Int (Particle b)
enforceSpring ( ( from, to ), spring ) particles =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            let
                ( p1, p2 ) =
                    ( applySpringForce target spring origin, applySpringForce origin spring target )
            in
            particles
                |> Dict.insert from p1
                |> Dict.insert to p2

        _ ->
            particles
