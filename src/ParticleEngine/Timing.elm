module ParticleEngine.Timing exposing (Timing, fixedUpdate)


type alias Timing =
    { stepTime : Float
    , timeAccum : Float
    , dtMultiplier : Float
    , dtHistory : List Float
    }


addDtHistory : Float -> Timing -> Timing
addDtHistory dt timing =
    { timing | dtHistory = dt :: timing.dtHistory |> List.take 20 }


fixedUpdate : Float -> Timing -> Timing
fixedUpdate dt timing =
    let
        adjustedDt : Float
        adjustedDt =
            dt * timing.dtMultiplier
    in
    if adjustedDt >= timing.stepTime then
        { timing | timeAccum = adjustedDt - timing.stepTime }
            |> addDtHistory dt
            |> fixedUpdate (adjustedDt - timing.stepTime)

    else
        { timing | timeAccum = timing.timeAccum + adjustedDt }
            |> addDtHistory dt
