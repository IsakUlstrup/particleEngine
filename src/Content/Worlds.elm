module Content.Worlds exposing (bridge, ring, rope)

import Content.Shapes exposing (nGon)
import ParticleEngine.Particle as Particle
import ParticleEngine.Vector2 as Vector2
import ParticleEngine.World as World exposing (World)


rope : World
rope =
    World.empty
        |> World.addForce (Vector2.new 0 200) True
        |> World.addParticle (Particle.new (Vector2.new 0 -70) 0)
        |> World.addParticle (Particle.new (Vector2.new 50 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -50 70) 1)
        |> World.addConstraint 0 1
        |> World.addConstraint 1 2


ring : World
ring =
    World.empty
        |> World.addParticles (nGon Vector2.zero 6 40)


bridge : World
bridge =
    World.empty
        |> World.addForce (Vector2.new 0 100) True
        |> World.addParticle (Particle.new (Vector2.new -300 0) 0)
        |> World.addParticle (Particle.new (Vector2.new -250 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -200 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -150 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -100 0) 1)
        |> World.addParticle (Particle.new (Vector2.new -50 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 0 0) 0)
        |> World.addParticle (Particle.new (Vector2.new 50 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 100 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 150 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 200 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 250 0) 1)
        |> World.addParticle (Particle.new (Vector2.new 300 0) 0)
        |> World.addConstraint 0 1
        |> World.addConstraint 1 2
        |> World.addConstraint 2 3
        |> World.addConstraint 3 4
        |> World.addConstraint 4 5
        |> World.addConstraint 5 6
        |> World.addConstraint 6 7
        |> World.addConstraint 7 8
        |> World.addConstraint 8 9
        |> World.addConstraint 9 10
        |> World.addConstraint 10 11
        |> World.addConstraint 11 12
