module Content.Worlds exposing (bridge, gravity, ring, rope, weightedRope)

import Content.Shapes exposing (nGon)
import ParticleEngine.Force exposing (Force(..))
import ParticleEngine.Particle as Particle
import ParticleEngine.Vector2 as Vector2
import ParticleEngine.World as World exposing (World)


rope : World
rope =
    World.empty
        |> World.addForce (Absolute <| Vector2.new 0 200) True
        |> World.addParticle (Particle.new (Vector2.new 0 -70) 0)
        |> World.addParticle (Particle.new (Vector2.new 50 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -50 70) 1)
        |> World.addAutoSpring 0 1
        |> World.addAutoSpring 1 2


ring : World
ring =
    World.empty
        |> World.addParticles (nGon Vector2.zero 6 40)


bridge : World
bridge =
    World.empty
        |> World.addForce (Absolute <| Vector2.new 0 100) True
        |> World.addParticle (Particle.new (Vector2.new -180 0) 0)
        |> World.addParticle (Particle.new (Vector2.new -150 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -120 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -90 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -60 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -30 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 0 0) 0)
        |> World.addParticle (Particle.new (Vector2.new 30 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 60 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 90 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 120 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 150 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 180 0) 0)
        |> World.addAutoSpring 0 1
        |> World.addAutoSpring 1 2
        |> World.addAutoSpring 2 3
        |> World.addAutoSpring 3 4
        |> World.addAutoSpring 4 5
        |> World.addAutoSpring 5 6
        |> World.addAutoSpring 6 7
        |> World.addAutoSpring 7 8
        |> World.addAutoSpring 8 9
        |> World.addAutoSpring 9 10
        |> World.addAutoSpring 10 11
        |> World.addAutoSpring 11 12


weightedRope : World
weightedRope =
    World.empty
        |> World.addForce (Absolute <| Vector2.new 0 100) True
        |> World.addParticle (Particle.new (Vector2.new 0 -100) 0)
        |> World.addParticle (Particle.new (Vector2.new 0 -80) 1)
        |> World.addSpring 0 1 200


gravity : World
gravity =
    World.empty
        |> World.addForce (Absolute <| Vector2.new 0 100) True
        |> World.addParticle (Particle.new (Vector2.new -50 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new 50 -100) 5)
