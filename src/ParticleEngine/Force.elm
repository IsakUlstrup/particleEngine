module ParticleEngine.Force exposing (Force(..))

{-| Relative respects a = f / m

absolute ignores mass, suitable for gravity

-}

import ParticleEngine.Vector2 exposing (Vector2)


type Force
    = Realative Vector2
    | Absolute Vector2
