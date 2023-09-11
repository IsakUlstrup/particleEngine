module ParticleEngine.Particle exposing
    ( Particle
    , Stick(..)
    , constrain
    , constrainStick
    , mapStick
    , new
    , radius
    , step
    , updateStick
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


constrainStick : Float -> ( Particle, Particle ) -> ( Particle, Particle )
constrainStick length ( p1, p2 ) =
    let
        dist =
            Vector2.distance p1.position p2.position

        diff =
            length - dist

        percent =
            (diff / dist) / 2

        offset =
            Vector2.scale percent (Vector2.subtract p1.position p2.position)
    in
    ( { p1 | position = p1.position |> Vector2.subtract offset }
    , { p2 | position = p2.position |> Vector2.add offset }
    )


updateStick : Stick -> Stick
updateStick stick =
    case stick of
        None _ ->
            stick

        Link dist p1 p2 p3 ->
            let
                ( newp1, newp2 ) =
                    constrainStick dist ( p1, p2 )

                ( newp22, newp3 ) =
                    constrainStick dist ( newp2, p3 )

                ( newp32, newp12 ) =
                    constrainStick dist ( newp3, newp1 )
            in
            Link dist newp12 newp22 newp32


type Stick
    = None Particle
    | Link Float Particle Particle Particle


mapStick : (Particle -> Particle) -> Stick -> Stick
mapStick f stick =
    case stick of
        None a ->
            None <| f a

        Link dist p1 p2 p3 ->
            Link dist (f p1) (f p2) (f p3)
