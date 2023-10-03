module Content.Shapes exposing (nGon)

import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)


nGon : Vector2 -> Int -> Float -> List (Particle a)
nGon center points radius =
    let
        angle : Int -> Float
        angle index =
            ((2 * pi) / toFloat points) * toFloat index

        position : Int -> Vector2
        position index =
            Vector2.new (cos <| angle index) (sin <| angle index)
                |> Vector2.scale radius
                |> Vector2.add center

        newParticle : Int -> Particle a
        newParticle index =
            Particle.new (position index) 1
    in
    List.range 0 (points - 1) |> List.map newParticle
