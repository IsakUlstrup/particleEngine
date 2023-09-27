module ParticleEngine.Spring exposing (Spring, new, setDamping, setLength, setRate)


type alias Spring =
    { length : Float
    , rate : Float
    , damping : Float
    }


new : Float -> Float -> Float -> Spring
new length rate damping =
    Spring (max 0 length) (max 0 rate) (max 0 damping)


setLength : Float -> Spring -> Spring
setLength length spring =
    { spring | length = max 0 length }


setRate : Float -> Spring -> Spring
setRate rate spring =
    { spring | rate = max 0 rate }


setDamping : Float -> Spring -> Spring
setDamping damping spring =
    { spring | damping = max 0 damping }
