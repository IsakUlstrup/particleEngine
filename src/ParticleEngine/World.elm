module ParticleEngine.World exposing
    ( World
    , addAutoSpring
    , addParticle
    , addParticles
    , addSpring
    , addSystem
    , averageFps
    , empty
    , removeSpring
    , setDtMulti
    , setParticleMass
    , setParticlePosition
    , tick
    , toggleSystem
    , updateParticles
    , updateSpring
    )

import Dict exposing (Dict)
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Spring as Spring exposing (Spring)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


type alias World a =
    { particles : Dict Int Particle
    , idCounter : Int
    , springs : Dict ( Int, Int ) Spring
    , systems : List ( Bool, a )
    , stepTime : Float
    , timeAccum : Float
    , dtMultiplier : Float
    , dtHistory : List Float
    }


empty : World a
empty =
    World Dict.empty 0 Dict.empty [] 0.02 0 1 []



-- PARTICLE


addParticle : Particle -> World a -> World a
addParticle particle world =
    { world
        | particles = world.particles |> Dict.insert world.idCounter particle
        , idCounter = world.idCounter + 1
    }


addParticles : List Particle -> World a -> World a
addParticles particles world =
    List.foldl addParticle world particles


updateParticles : (Particle -> Particle) -> World a -> World a
updateParticles f world =
    { world | particles = Dict.map (\_ p -> f p) world.particles }


particleDistance : Int -> Int -> Dict Int Particle -> Maybe Float
particleDistance one two particles =
    case ( Dict.get one particles, Dict.get two particles ) of
        ( Just p1, Just p2 ) ->
            Just <| Vector2.distance p1.position p2.position

        _ ->
            Nothing


setParticlePosition : Int -> Vector2 -> World a -> World a
setParticlePosition id position world =
    let
        updatePosition : Particle -> Particle
        updatePosition p =
            { p | position = position, oldPosition = position }
    in
    { world | particles = Dict.update id (Maybe.map updatePosition) world.particles }


setParticleMass : Int -> Float -> World a -> World a
setParticleMass id mass world =
    let
        updateMass : Particle -> Particle
        updateMass p =
            { p | mass = mass }
    in
    { world | particles = Dict.update id (Maybe.map updateMass) world.particles }



-- SYSTEM


addSystem : a -> Bool -> World a -> World a
addSystem system enabled world =
    { world | systems = ( enabled, system ) :: world.systems }


toggleSystem : Int -> World a -> World a
toggleSystem index world =
    let
        toggleHelper : Int -> ( Bool, a ) -> ( Bool, a )
        toggleHelper i ( enabled, system ) =
            if i == index then
                ( not enabled, system )

            else
                ( enabled, system )
    in
    { world | systems = List.indexedMap toggleHelper world.systems }


runSystems : (a -> World a -> World a) -> World a -> World a
runSystems f world =
    List.foldl f world (world.systems |> List.filter Tuple.first |> List.map Tuple.second)
        |> applySpringForces
        |> updateParticles (Particle.step world.stepTime)



-- SPRING


{-| Add spring between two particles, length will be calculated based on current distance between the particles
-}
addAutoSpring : Int -> Int -> Float -> Float -> World a -> World a
addAutoSpring from to springRate damping world =
    case particleDistance from to world.particles of
        Just dist ->
            { world | springs = world.springs |> Dict.insert ( from, to ) (Spring.new dist springRate damping) }

        Nothing ->
            world


addSpring : Int -> Int -> Spring -> World a -> World a
addSpring from to spring world =
    case particleDistance from to world.particles of
        Just _ ->
            { world | springs = world.springs |> Dict.insert ( from, to ) spring }

        Nothing ->
            world


removeSpring : ( Int, Int ) -> World a -> World a
removeSpring constraint world =
    let
        keepConstraint : ( Int, Int ) -> Spring -> Bool
        keepConstraint ids _ =
            ids /= constraint
    in
    { world | springs = Dict.filter keepConstraint world.springs }


updateSpring : ( Int, Int ) -> (Spring -> Spring) -> World a -> World a
updateSpring connections f world =
    { world | springs = Dict.update connections (Maybe.map f) world.springs }


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


applySpringForces : World a -> World a
applySpringForces world =
    { world | particles = List.foldl constrainPair world.particles (Dict.toList world.springs) }



-- Timing


addDtHistory : Float -> World a -> World a
addDtHistory dt world =
    { world | dtHistory = dt :: world.dtHistory |> List.take 20 }


setDtMulti : Float -> World a -> World a
setDtMulti multi world =
    { world | dtMultiplier = multi }


fixedUpdate : (World a -> World a) -> Float -> World a -> World a
fixedUpdate f dt world =
    let
        adjustedDt : Float
        adjustedDt =
            (dt * world.dtMultiplier) / 1000
    in
    { world | timeAccum = world.timeAccum + adjustedDt }
        |> addDtHistory dt
        |> updateModel f


updateModel : (World a -> World a) -> World a -> World a
updateModel f world =
    if world.timeAccum >= world.stepTime then
        { world | timeAccum = world.timeAccum - world.stepTime }
            |> f
            |> updateModel f

    else
        world


averageDelta : List Float -> Float
averageDelta dts =
    List.sum dts / toFloat (List.length dts)


averageFps : World a -> Float
averageFps world =
    1000 / averageDelta world.dtHistory


tick : Float -> (a -> World a -> World a) -> World a -> World a
tick dt f world =
    fixedUpdate (runSystems f) dt world
