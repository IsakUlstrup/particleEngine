module Content.Worlds exposing (ball, bridge, cloth, gravity, rope)

import Content.Shapes exposing (nGon)
import ParticleEngine.Boundary as Boundary
import ParticleEngine.Particle as Particle
import ParticleEngine.Spring as Spring exposing (Spring)
import ParticleEngine.Vector2 as Vector2
import ParticleEngine.World as World exposing (World)
import System exposing (System)


rope : World System
rope =
    World.empty
        |> World.addSystem (System.springs 10) False
        |> World.addSystem System.springStress True
        |> World.addSystem (System.constrain <| Boundary.new Vector2.zero 500 500) True
        -- |> World.addForce (Absolute <| Vector2.new 0 100) True
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


ringSpring : Int -> Int -> World a -> World a
ringSpring from to =
    World.addAutoSpring from to 100 5


ball : World System
ball =
    World.empty
        |> World.addSystem System.particles True
        |> World.addSystem (System.springs <| Particle.radius) False
        |> World.addSystem System.springStress True
        |> World.addSystem (System.constrain <| Boundary.new Vector2.zero 500 500) True
        -- |> World.addForce (Absolute <| Vector2.new 70 300) True
        |> World.addParticles (nGon Vector2.zero 6 100)
        |> World.addParticle (Particle.new (Vector2.new -40 -300) 0)
        -- 0
        |> ringSpring 0 1
        |> ringSpring 0 2
        |> ringSpring 0 3
        |> ringSpring 0 4
        |> ringSpring 0 5
        -- 1
        |> ringSpring 1 0
        |> ringSpring 1 2
        |> ringSpring 1 3
        |> ringSpring 1 4
        |> ringSpring 1 5
        -- 2
        |> ringSpring 2 0
        |> ringSpring 2 1
        |> ringSpring 2 3
        |> ringSpring 2 4
        |> ringSpring 2 5
        -- 3
        |> ringSpring 3 0
        |> ringSpring 2 1
        |> ringSpring 3 2
        |> ringSpring 3 4
        |> ringSpring 3 5
        -- 4
        |> ringSpring 4 0
        |> ringSpring 4 1
        |> ringSpring 4 2
        |> ringSpring 4 3
        |> ringSpring 4 5
        -- 5
        |> ringSpring 5 0
        |> ringSpring 5 1
        |> ringSpring 5 2
        |> ringSpring 5 3
        |> ringSpring 5 4
        |> World.addAutoSpring 3 6 100 10


bridge : World System
bridge =
    World.empty
        |> World.addSystem System.particles True
        |> World.addSystem (System.springs <| Particle.radius) True
        |> World.addSystem (System.constrain <| Boundary.new Vector2.zero 500 500) True
        -- |> World.addForce (Absolute <| Vector2.new 0 100) True
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


gravity : World System
gravity =
    World.empty
        |> World.addSystem System.particleVelocity True
        |> World.addSystem System.particles True
        |> World.addSystem (System.springs <| Particle.radius) True
        |> World.addSystem (System.constrain <| Boundary.new Vector2.zero 500 500) True
        |> World.addSystem (System.force <| Vector2.new 0 100) True
        -- |> World.addForce (Absolute <| Vector2.new 0 100) True
        |> World.addParticle (Particle.new (Vector2.new -50 -100) 1)
        |> World.addParticle (Particle.new (Vector2.new 50 -100) 5)
        |> World.addParticle (Particle.new (Vector2.new 150 -50) 5)
        |> World.addSpring 1 2 (Spring.new 50 1000 1000)


clothSpring : Spring
clothSpring =
    Spring.new 50 500 100


cloth : World System
cloth =
    World.empty
        |> World.addSystem System.springStress False
        |> World.addSystem (System.springs 5) True
        |> World.addSystem (System.constrain <| Boundary.new Vector2.zero 500 500) True
        -- |> World.addForce (Absolute <| Vector2.new 0 100) True
        -- |> World.addForce (Realative <| Vector2.new 100 0) False
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
