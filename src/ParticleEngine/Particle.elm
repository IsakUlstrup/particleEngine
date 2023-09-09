module ParticleEngine.Particle exposing
    ( Particle
    , constrain
    , new
    , step
    )

import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


{-| A particle meant to be used with Verlet integration
-}
type alias Particle =
    { position : Vector2
    , oldPosition : Vector2
    , mass : Float
    }


{-| Particle constructor
-}
new : Vector2 -> Float -> Particle
new position mass =
    Particle position position mass


{-| Derive velocity vector based on old position
-}
velocity : Particle -> Vector2
velocity particle =
    particle.position
        |> Vector2.subtract particle.oldPosition


{-| Step forwards using Verlet integration
-}
step : Vector2 -> Float -> Particle -> Particle
step force dt particle =
    let
        acceleration : Vector2
        acceleration =
            Vector2.divide particle.mass force
                |> Vector2.scale (dt ^ 2)
    in
    { particle
        | position =
            particle.position
                |> Vector2.add (velocity particle)
                |> Vector2.add acceleration
        , oldPosition = particle.position
    }


constrain : Float -> Float -> Particle -> Particle
constrain width height particle =
    let
        vel =
            velocity particle
    in
    if particle.position.x > width then
        { particle
            | position = Vector2.mapX (always width) particle.position
            , oldPosition = Vector2.mapX (always <| width + vel.x) particle.oldPosition
        }

    else if particle.position.x < -width then
        { particle
            | position = Vector2.mapX (always -width) particle.position
            , oldPosition = Vector2.mapX (always <| -width + vel.x) particle.oldPosition
        }

    else if particle.position.y > height then
        { particle
            | position = Vector2.mapY (always height) particle.position
            , oldPosition = Vector2.mapY (always <| height + vel.y) particle.oldPosition
        }

    else if particle.position.y < -height then
        { particle
            | position = Vector2.mapY (always -height) particle.position
            , oldPosition = Vector2.mapY (always <| -height + vel.y) particle.oldPosition
        }

    else
        particle


type alias Constraint =
    { p1 : Particle
    , p2 : Particle
    , length : Float
    }
