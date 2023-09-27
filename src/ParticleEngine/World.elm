module ParticleEngine.World exposing
    ( World
    , addAutoSpring
    , addForce
    , addParticle
    , addParticles
    , addRenderSystem
    , addSpring
    , applyForces
    , applySpringForces
    , empty
    , removeSpring
    , setForce
    , setParticleMass
    , setParticlePosition
    , toggleForce
    , updateParticles
    , updateSpring
    )

import Dict exposing (Dict)
import ParticleEngine.Force exposing (Force)
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Spring exposing (Spring)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


type alias World renderSystem =
    { particles : Dict Int Particle
    , idCounter : Int
    , springs : Dict ( Int, Int ) Spring
    , forces : List ( Bool, Force )
    , renderSystems : List ( Bool, renderSystem )
    }


empty : World renderSystem
empty =
    World Dict.empty 0 Dict.empty [] []


addParticle : Particle -> World renderSystem -> World renderSystem
addParticle particle world =
    { world
        | particles = world.particles |> Dict.insert world.idCounter particle
        , idCounter = world.idCounter + 1
    }


addParticles : List Particle -> World renderSystem -> World renderSystem
addParticles particles world =
    List.foldl addParticle world particles


addRenderSystem : renderSystem -> Bool -> World renderSystem -> World renderSystem
addRenderSystem system enabled world =
    { world | renderSystems = ( enabled, system ) :: world.renderSystems }


{-| Add spring between two particles, length will be calculated based on current distance between the particles
-}
addAutoSpring : Int -> Int -> Float -> Float -> World renderSystem -> World renderSystem
addAutoSpring from to springRate damping world =
    case particleDistance from to world.particles of
        Just dist ->
            { world | springs = world.springs |> Dict.insert ( from, to ) (Spring dist springRate damping) }

        Nothing ->
            world


addSpring : Int -> Int -> Spring -> World renderSystem -> World renderSystem
addSpring from to spring world =
    case particleDistance from to world.particles of
        Just _ ->
            { world | springs = world.springs |> Dict.insert ( from, to ) spring }

        Nothing ->
            world


updateSpring : ( Int, Int ) -> (Spring -> Spring) -> World renderSystem -> World renderSystem
updateSpring connections f world =
    { world | springs = Dict.update connections (Maybe.map f) world.springs }


updateParticles : (Int -> Particle -> Particle) -> World renderSystem -> World renderSystem
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
                    ( Particle.applySpringForce target spring origin, Particle.applySpringForce origin spring target )
            in
            particles
                |> Dict.insert from p1
                |> Dict.insert to p2

        _ ->
            particles


applySpringForces : World renderSystem -> World renderSystem
applySpringForces world =
    { world | particles = List.foldl constrainPair world.particles (Dict.toList world.springs) }


toggleForce : Int -> World renderSystem -> World renderSystem
toggleForce index world =
    let
        helper : Int -> ( Bool, Force ) -> ( Bool, Force )
        helper i force =
            if i == index then
                Tuple.mapFirst not force

            else
                force
    in
    { world | forces = List.indexedMap helper world.forces }


addForce : Force -> Bool -> World renderSystem -> World renderSystem
addForce force enabled world =
    { world | forces = ( enabled, force ) :: world.forces }


setForce : Int -> Force -> World renderSystem -> World renderSystem
setForce index force world =
    let
        helper : Int -> ( Bool, Force ) -> ( Bool, Force )
        helper i f =
            if index == i then
                Tuple.mapSecond (always force) f

            else
                f
    in
    { world | forces = List.indexedMap helper world.forces }


enabledForces : List ( Bool, Force ) -> List Force
enabledForces forces =
    forces
        |> List.filter Tuple.first
        |> List.map Tuple.second


applyForces : World renderSystem -> World renderSystem
applyForces world =
    world
        |> updateParticles (\_ p -> Particle.applyForces (enabledForces world.forces) p)


setParticlePosition : Int -> Vector2 -> World renderSystem -> World renderSystem
setParticlePosition id position world =
    let
        updatePosition : Particle -> Particle
        updatePosition p =
            { p | position = position, oldPosition = position }
    in
    { world | particles = Dict.update id (Maybe.map updatePosition) world.particles }


setParticleMass : Int -> Float -> World renderSystem -> World renderSystem
setParticleMass id mass world =
    let
        updateMass : Particle -> Particle
        updateMass p =
            { p | mass = mass }
    in
    { world | particles = Dict.update id (Maybe.map updateMass) world.particles }


removeSpring : ( Int, Int ) -> World renderSystem -> World renderSystem
removeSpring constraint world =
    let
        keepConstraint : ( Int, Int ) -> Spring -> Bool
        keepConstraint ids _ =
            ids /= constraint
    in
    { world | springs = Dict.filter keepConstraint world.springs }
