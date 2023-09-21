module ParticleEngine.World exposing (World, addConstraint, addForce, addParticle, addParticles, constrainParticles, empty, removeConstraint, setForce, setParticlePosition, sumForces, toggleForce, updateParticles)

import Dict exposing (Dict)
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Spring exposing (Spring)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


type alias World =
    { particles : Dict Int Particle
    , idCounter : Int
    , constraints : Dict ( Int, Int ) Spring
    , forces : List ( Bool, Vector2 )
    }


empty : World
empty =
    World Dict.empty 0 Dict.empty []


addParticle : Particle -> World -> World
addParticle particle world =
    { world
        | particles = world.particles |> Dict.insert world.idCounter particle
        , idCounter = world.idCounter + 1
    }


addParticles : List Particle -> World -> World
addParticles particles world =
    List.foldl addParticle world particles


addConstraint : Int -> Int -> World -> World
addConstraint from to world =
    case particleDistance from to world.particles of
        Just dist ->
            { world | constraints = world.constraints |> Dict.insert ( from, to ) (Spring dist 0.5) }

        Nothing ->
            world


updateParticles : (Int -> Particle -> Particle) -> World -> World
updateParticles f world =
    { world | particles = Dict.map f world.particles }


particleDistance : Int -> Int -> Dict Int Particle -> Maybe Float
particleDistance one two particles =
    case ( Dict.get one particles, Dict.get two particles ) of
        ( Just p1, Just p2 ) ->
            Just <| Vector2.distance p1.position p2.position

        _ ->
            Nothing


constrainPair : ( ( Int, Int ), Spring ) -> Dict Int Particle -> Dict Int Particle
constrainPair ( ( from, to ), spring ) particles =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            let
                ( p1, p2 ) =
                    Particle.enforceConstraint spring ( origin, target )
            in
            particles
                |> Dict.insert from p1
                |> Dict.insert to p2

        _ ->
            particles


constrainParticles : World -> World
constrainParticles world =
    { world | particles = List.foldl constrainPair world.particles (Dict.toList world.constraints) }


toggleForce : Int -> World -> World
toggleForce index world =
    let
        helper i force =
            if i == index then
                Tuple.mapFirst not force

            else
                force
    in
    { world | forces = List.indexedMap helper world.forces }


addForce : Vector2 -> Bool -> World -> World
addForce force enabled world =
    { world | forces = ( enabled, force ) :: world.forces }


setForce : Int -> Vector2 -> World -> World
setForce index force world =
    let
        helper : Int -> ( Bool, Vector2 ) -> ( Bool, Vector2 )
        helper i f =
            if index == i then
                Tuple.mapSecond (always force) f

            else
                f
    in
    { world | forces = List.indexedMap helper world.forces }


{-| Get sum of all enabled forces
-}
sumForces : World -> Vector2
sumForces world =
    List.foldl Vector2.add
        Vector2.zero
        (List.filterMap
            (\( e, f ) ->
                if e then
                    Just f

                else
                    Nothing
            )
            world.forces
        )


setParticlePosition : Int -> Vector2 -> World -> World
setParticlePosition id position world =
    let
        updatePosition : Particle -> Particle
        updatePosition p =
            { p | position = position, oldPosition = position }
    in
    { world | particles = Dict.update id (Maybe.map updatePosition) world.particles }


removeConstraint : ( Int, Int ) -> World -> World
removeConstraint constraint world =
    let
        keepConstraint : ( Int, Int ) -> Spring -> Bool
        keepConstraint ids _ =
            ids /= constraint
    in
    { world | constraints = Dict.filter keepConstraint world.constraints }
