module ParticleEngine.World exposing
    ( World
    , addAutoSpring
    , addParticle
    , addParticles
    , addSpring
    , addSystem
    , averageFps
    , empty
    , filterMapSprings
    , filterSprings
    , getDtMulti
    , getParticles
    , indexedMapSystems
    , mapParticles
    , mapSprings
    , particleDistance
    , removeSpring
    , runRenderSystems
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


type World a b
    = World
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
    World
        { particles = Dict.empty
        , idCounter = 0
        , springs = Dict.empty
        , systems = []
        , timeAccum = 0
        , dtMultiplier = 1
        , dtHistory = []
        }



-- PARTICLE


addParticle : Particle b -> World a b -> World a b
addParticle particle (World world) =
    World
        { world
            | particles = world.particles |> Dict.insert world.idCounter particle
            , idCounter = world.idCounter + 1
        }


addParticles : List (Particle b) -> World a b -> World a b
addParticles particles world =
    List.foldl addParticle world particles


getParticles : World a b -> Dict Int (Particle b)
getParticles (World world) =
    world.particles


updateParticles : (Particle b -> Particle b) -> World a b -> World a b
updateParticles f (World world) =
    World { world | particles = Dict.map (\_ p -> f p) world.particles }


mapParticles : (( Int, Particle b ) -> c) -> World a b -> List c
mapParticles f (World world) =
    world.particles |> Dict.toList |> List.map f


setParticlePosition : Int -> Vector2 -> World a b -> World a b
setParticlePosition id position (World world) =
    let
        updatePosition : Particle b -> Particle b
        updatePosition p =
            { p | position = position, oldPosition = position }
    in
    World { world | particles = Dict.update id (Maybe.map updatePosition) world.particles }


setParticleMass : Int -> Float -> World a b -> World a b
setParticleMass id mass (World world) =
    let
        updateMass : Particle b -> Particle b
        updateMass p =
            { p | mass = mass }
    in
    World { world | particles = Dict.update id (Maybe.map updateMass) world.particles }


{-| Get distance between two particles if they exist
-}
particleDistance : Int -> Int -> World a b -> Maybe Float
particleDistance from to (World world) =
    case ( Dict.get from world.particles, Dict.get to world.particles ) of
        ( Just f, Just t ) ->
            Just (Vector2.distance f.position t.position)

        _ ->
            Nothing



-- SYSTEM


addSystem : a -> Bool -> World a b -> World a b
addSystem system enabled (World world) =
    World { world | systems = ( enabled, system ) :: world.systems }


toggleSystem : Int -> World a b -> World a b
toggleSystem index (World world) =
    let
        toggleHelper : Int -> ( Bool, a ) -> ( Bool, a )
        toggleHelper i ( enabled, system ) =
            if i == index then
                ( not enabled, system )

            else
                ( enabled, system )
    in
    World { world | systems = List.indexedMap toggleHelper world.systems }


runSystems : (a -> World a b -> World a b) -> World a b -> World a b
runSystems f (World world) =
    List.foldl f (World world) (world.systems |> List.filter Tuple.first |> List.map Tuple.second)
        |> applySpringForces
        |> updateParticles (Particle.step stepTime)


runRenderSystems : (World a b -> a -> Maybe c) -> World a b -> List c
runRenderSystems f (World world) =
    world.systems
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> List.filterMap (f (World world))


indexedMapSystems : (Int -> Bool -> a -> c) -> World a b -> List c
indexedMapSystems f (World world) =
    world.systems
        |> List.indexedMap (\index ( enabled, system ) -> f index enabled system)



-- SPRING


{-| Add spring between two particles, length will be calculated based on current distance between the particles
-}
addAutoSpring : Int -> Int -> Float -> Float -> World a b -> World a b
addAutoSpring from to springRate damping (World world) =
    case particleDistance from to (World world) of
        Just dist ->
            World { world | springs = world.springs |> Dict.insert ( from, to ) (Spring.new dist springRate damping) }

        Nothing ->
            World world


addSpring : Int -> Int -> Spring -> World a b -> World a b
addSpring from to spring (World world) =
    case particleDistance from to (World world) of
        Just _ ->
            World { world | springs = world.springs |> Dict.insert ( from, to ) spring }

        Nothing ->
            World world


mapSprings : (( ( Int, Int ), Spring ) -> c) -> World a b -> List c
mapSprings f (World world) =
    world.springs |> Dict.toList |> List.map f


filterMapSprings : (( ( Int, Int ), Spring ) -> Maybe c) -> World a b -> List c
filterMapSprings f (World world) =
    world.springs |> Dict.toList |> List.filterMap f


removeSpring : ( Int, Int ) -> World a b -> World a b
removeSpring constraint (World world) =
    let
        keepConstraint : ( Int, Int ) -> Spring -> Bool
        keepConstraint ids _ =
            ids /= constraint
    in
    World { world | springs = Dict.filter keepConstraint world.springs }


updateSpring : ( Int, Int ) -> (Spring -> Spring) -> World a b -> World a b
updateSpring connections f (World world) =
    World { world | springs = Dict.update connections (Maybe.map f) world.springs }


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
applySpringForces (World world) =
    World { world | particles = List.foldl constrainPair world.particles (Dict.toList world.springs) }


filterSprings : (( Int, Int ) -> Spring -> Bool) -> World a b -> World a b
filterSprings predicate (World world) =
    World { world | springs = world.springs |> Dict.filter predicate }



-- Timing


addDtHistory : Float -> World a b -> World a b
addDtHistory dt (World world) =
    World { world | dtHistory = dt :: world.dtHistory |> List.take 20 }


setDtMulti : Float -> World a b -> World a b
setDtMulti multi (World world) =
    World { world | dtMultiplier = multi }


fixedUpdate : (World a b -> World a b) -> Float -> World a b -> World a b
fixedUpdate f dt (World world) =
    let
        adjustedDt : Float
        adjustedDt =
            (dt * world.dtMultiplier) / 1000
    in
    World { world | timeAccum = world.timeAccum + adjustedDt }
        |> addDtHistory dt
        |> updateModel f


updateModel : (World a b -> World a b) -> World a b -> World a b
updateModel f (World world) =
    if world.timeAccum >= stepTime then
        World { world | timeAccum = world.timeAccum - stepTime }
            |> f
            |> updateModel f

    else
        World world


averageDelta : List Float -> Float
averageDelta dts =
    List.sum dts / toFloat (List.length dts)


averageFps : World a b -> Float
averageFps (World world) =
    1000 / averageDelta world.dtHistory


getDtMulti : World a b -> Float
getDtMulti (World world) =
    world.dtMultiplier


tick : Float -> (a -> World a b -> World a b) -> World a b -> World a b
tick dt f world =
    fixedUpdate (runSystems f) dt world
