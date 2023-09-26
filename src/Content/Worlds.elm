module Content.Worlds exposing (bridge, cloth, gravity, ring, rope)

import Content.Shapes exposing (nGon)
import ParticleEngine.Force exposing (Force(..))
import ParticleEngine.Particle as Particle
import ParticleEngine.Spring exposing (Spring)
import ParticleEngine.Vector2 as Vector2
import ParticleEngine.World as World exposing (World)


rope : World
rope =
    World.empty
        |> World.addForce (Absolute <| Vector2.new 0 100) True
        |> World.addParticle (Particle.new (Vector2.new -180 0) 0)
        |> World.addParticle (Particle.new (Vector2.new -150 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -120 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -90 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -60 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -30 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 0 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 30 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 60 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 90 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 120 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 150 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 180 0) 1)
        |> World.addAutoSpring 0 1 1000 100
        |> World.addAutoSpring 1 2 1000 100
        |> World.addAutoSpring 2 3 1000 100
        |> World.addAutoSpring 3 4 1000 100
        |> World.addAutoSpring 4 5 1000 100
        |> World.addAutoSpring 5 6 1000 100
        |> World.addAutoSpring 6 7 1000 100
        |> World.addAutoSpring 7 8 1000 100
        |> World.addAutoSpring 8 9 1000 100
        |> World.addAutoSpring 9 10 1000 100
        |> World.addAutoSpring 10 11 1000 100
        |> World.addAutoSpring 11 12 1000 100


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
        |> World.addAutoSpring 0 1 1000 100
        |> World.addAutoSpring 1 2 1000 100
        |> World.addAutoSpring 2 3 1000 100
        |> World.addAutoSpring 3 4 1000 100
        |> World.addAutoSpring 4 5 1000 100
        |> World.addAutoSpring 5 6 1000 100
        |> World.addAutoSpring 6 7 1000 100
        |> World.addAutoSpring 7 8 1000 100
        |> World.addAutoSpring 8 9 1000 100
        |> World.addAutoSpring 9 10 1000 100
        |> World.addAutoSpring 10 11 1000 100
        |> World.addAutoSpring 11 12 1000 100


gravity : World
gravity =
    World.empty
        |> World.addForce (Absolute <| Vector2.new 0 100) True
        |> World.addParticle (Particle.new (Vector2.new -50 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new 50 -100) 5)
        |> World.addParticle (Particle.new (Vector2.new 150 -50) 5)
        |> World.addSpring 1 2 (Spring 50 1000 1000)


clothSpring : Spring
clothSpring =
    Spring 50 500 100


cloth : World
cloth =
    World.empty
        |> World.addForce (Absolute <| Vector2.new 0 100) True
        |> World.addForce (Realative <| Vector2.new 100 0) False
        -- row 1, anchors
        |> World.addParticle (Particle.new (Vector2.new -150 -200) 0)
        |> World.addParticle (Particle.new (Vector2.new -100 -200) 0)
        |> World.addParticle (Particle.new (Vector2.new -50 -200) 0)
        |> World.addParticle (Particle.new (Vector2.new 0 -200) 0)
        |> World.addParticle (Particle.new (Vector2.new 50 -200) 0)
        |> World.addParticle (Particle.new (Vector2.new 100 -200) 0)
        |> World.addParticle (Particle.new (Vector2.new 150 -200) 0)
        -- row 2
        |> World.addParticle (Particle.new (Vector2.new -150 -150) 1)
        |> World.addParticle (Particle.new (Vector2.new -100 -150) 1)
        |> World.addParticle (Particle.new (Vector2.new -50 -150) 1)
        |> World.addParticle (Particle.new (Vector2.new 0 -150) 1)
        |> World.addParticle (Particle.new (Vector2.new 50 -150) 1)
        |> World.addParticle (Particle.new (Vector2.new 100 -150) 1)
        |> World.addParticle (Particle.new (Vector2.new 150 -150) 1)
        -- row 2 springs
        |> World.addSpring 0 7 clothSpring
        |> World.addSpring 1 8 clothSpring
        |> World.addSpring 2 9 clothSpring
        |> World.addSpring 3 10 clothSpring
        |> World.addSpring 4 11 clothSpring
        |> World.addSpring 5 12 clothSpring
        |> World.addSpring 6 13 clothSpring
        |> World.addSpring 7 8 clothSpring
        |> World.addSpring 8 9 clothSpring
        |> World.addSpring 9 10 clothSpring
        |> World.addSpring 10 11 clothSpring
        |> World.addSpring 11 12 clothSpring
        |> World.addSpring 12 13 clothSpring
        |> World.addSpring 13 14 clothSpring
        -- row 3
        |> World.addParticle (Particle.new (Vector2.new -150 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new -100 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new -50 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new 0 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new 50 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new 100 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new 150 -100) 1)
        -- row 2 springs
        |> World.addSpring 7 14 clothSpring
        |> World.addSpring 8 15 clothSpring
        |> World.addSpring 9 16 clothSpring
        |> World.addSpring 10 17 clothSpring
        |> World.addSpring 11 18 clothSpring
        |> World.addSpring 12 19 clothSpring
        |> World.addSpring 13 20 clothSpring
        |> World.addSpring 14 15 clothSpring
        |> World.addSpring 15 16 clothSpring
        |> World.addSpring 16 17 clothSpring
        |> World.addSpring 17 18 clothSpring
        |> World.addSpring 18 19 clothSpring
        |> World.addSpring 19 20 clothSpring
        |> World.addSpring 20 21 clothSpring
        -- row 3
        |> World.addParticle (Particle.new (Vector2.new -150 -50) 1)
        |> World.addParticle (Particle.new (Vector2.new -100 -50) 1)
        |> World.addParticle (Particle.new (Vector2.new -50 -50) 1)
        |> World.addParticle (Particle.new (Vector2.new 0 -50) 1)
        |> World.addParticle (Particle.new (Vector2.new 50 -50) 1)
        |> World.addParticle (Particle.new (Vector2.new 100 -50) 1)
        |> World.addParticle (Particle.new (Vector2.new 150 -50) 1)
        -- row 3 springs
        |> World.addSpring 14 21 clothSpring
        |> World.addSpring 15 22 clothSpring
        |> World.addSpring 16 23 clothSpring
        |> World.addSpring 17 24 clothSpring
        |> World.addSpring 18 25 clothSpring
        |> World.addSpring 19 26 clothSpring
        |> World.addSpring 20 27 clothSpring
        |> World.addSpring 21 22 clothSpring
        |> World.addSpring 22 23 clothSpring
        |> World.addSpring 23 24 clothSpring
        |> World.addSpring 24 25 clothSpring
        |> World.addSpring 25 26 clothSpring
        |> World.addSpring 26 27 clothSpring
        |> World.addSpring 27 28 clothSpring
