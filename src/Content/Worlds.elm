module Content.Worlds exposing (ring, rope)

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
