module ParticleEngine.World exposing
    ( World
    , addAutoSpring
    , addParticle
    , addParticles
    , addSpring
    , addSystem
    , averageFps
    , empty
    , filterSprings
    , mapSystems
    , particleDistance
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


type alias World a b =
    { particles : Dict Int (Particle b)
    , idCounter : Int
    , springs : Dict ( Int, Int ) Spring
    , systems : List ( Bool, a )
    , timeAccum : Float
    , dtMultiplier : Float
    , dtHistory : List Float
    }


{-| Step time constant in seconds
-}
stepTime : Float
stepTime =
    0.02


empty : World a b
empty =
    World Dict.empty 0 Dict.empty [] 0 1 []



-- PARTICLE


addParticle : Particle b -> World a b -> World a b
addParticle particle world =
    { world
        | particles = world.particles |> Dict.insert world.idCounter particle
        , idCounter = world.idCounter + 1
    }


addParticles : List (Particle b) -> World a b -> World a b
addParticles particles world =
    List.foldl addParticle world particles


updateParticles : (Particle b -> Particle b) -> World a b -> World a b
updateParticles f world =
    { world | particles = Dict.map (\_ p -> f p) world.particles }


setParticlePosition : Int -> Vector2 -> World a b -> World a b
setParticlePosition id position world =
    let
        updatePosition : Particle b -> Particle b
        updatePosition p =
            { p | position = position, oldPosition = position }
    in
    { world | particles = Dict.update id (Maybe.map updatePosition) world.particles }


setParticleMass : Int -> Float -> World a b -> World a b
setParticleMass id mass world =
    let
        updateMass : Particle b -> Particle b
        updateMass p =
            { p | mass = mass }
    in
    { world | particles = Dict.update id (Maybe.map updateMass) world.particles }


{-| Get distance between two particles if they exist
-}
particleDistance : Int -> Int -> World a b -> Maybe Float
particleDistance from to world =
    case ( Dict.get from world.particles, Dict.get to world.particles ) of
        ( Just f, Just t ) ->
            Just (Vector2.distance f.position t.position)

        _ ->
            Nothing



-- SYSTEM


addSystem : a -> Bool -> World a b -> World a b
addSystem system enabled world =
    { world | systems = ( enabled, system ) :: world.systems }


toggleSystem : Int -> World a b -> World a b
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


runSystems : (a -> World a b -> World a b) -> World a b -> World a b
runSystems f world =
    List.foldl f world (world.systems |> List.filter Tuple.first |> List.map Tuple.second)
        |> applySpringForces
        |> updateParticles (Particle.step stepTime)


mapSystems : (Bool -> a -> a) -> World a b -> World a b
mapSystems f world =
    { world | systems = List.map (\( enabled, system ) -> ( enabled, f enabled system )) world.systems }



-- SPRING


{-| Add spring between two particles, length will be calculated based on current distance between the particles
-}
addAutoSpring : Int -> Int -> Float -> Float -> World a b -> World a b
addAutoSpring from to springRate damping world =
    case particleDistance from to world of
        Just dist ->
            { world | springs = world.springs |> Dict.insert ( from, to ) (Spring.new dist springRate damping) }

        Nothing ->
            world


addSpring : Int -> Int -> Spring -> World a b -> World a b
addSpring from to spring world =
    case particleDistance from to world of
        Just _ ->
            { world | springs = world.springs |> Dict.insert ( from, to ) spring }

        Nothing ->
            world


removeSpring : ( Int, Int ) -> World a b -> World a b
removeSpring constraint world =
    let
        keepConstraint : ( Int, Int ) -> Spring -> Bool
        keepConstraint ids _ =
            ids /= constraint
    in
    { world | springs = Dict.filter keepConstraint world.springs }


updateSpring : ( Int, Int ) -> (Spring -> Spring) -> World a b -> World a b
updateSpring connections f world =
    { world | springs = Dict.update connections (Maybe.map f) world.springs }


constrainPair : ( ( Int, Int ), Spring ) -> Dict Int (Particle b) -> Dict Int (Particle b)
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


applySpringForces : World a b -> World a b
applySpringForces world =
    { world | particles = List.foldl constrainPair world.particles (Dict.toList world.springs) }


filterSprings : (( Int, Int ) -> Spring -> Bool) -> World a b -> World a b
filterSprings predicate world =
    { world | springs = world.springs |> Dict.filter predicate }



-- Timing


addDtHistory : Float -> World a b -> World a b
addDtHistory dt world =
    { world | dtHistory = dt :: world.dtHistory |> List.take 20 }


setDtMulti : Float -> World a b -> World a b
setDtMulti multi world =
    { world | dtMultiplier = multi }


fixedUpdate : (World a b -> World a b) -> Float -> World a b -> World a b
fixedUpdate f dt world =
    let
        adjustedDt : Float
        adjustedDt =
            (dt * world.dtMultiplier) / 1000
    in
    { world | timeAccum = world.timeAccum + adjustedDt }
        |> addDtHistory dt
        |> updateModel f


updateModel : (World a b -> World a b) -> World a b -> World a b
updateModel f world =
    if world.timeAccum >= stepTime then
        { world | timeAccum = world.timeAccum - stepTime }
            |> f
            |> updateModel f

    else
        world


averageDelta : List Float -> Float
averageDelta dts =
    List.sum dts / toFloat (List.length dts)


averageFps : World a b -> Float
averageFps world =
    1000 / averageDelta world.dtHistory


tick : Float -> (a -> World a b -> World a b) -> World a b -> World a b
tick dt f world =
    fixedUpdate (runSystems f) dt world
