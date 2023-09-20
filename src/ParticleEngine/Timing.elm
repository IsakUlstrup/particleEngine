module ParticleEngine.Timing exposing
    ( Timing
    , fixedUpdate
    , new
    , setDtMulti
    )


type alias Timing =
    { stepTime : Float
    , timeAccum : Float
    , dtMultiplier : Float
    , dtHistory : List Float
    }


new : Timing
new =
    Timing 0.02 0 1 []


addDtHistory : Float -> Timing -> Timing
addDtHistory dt timing =
    { timing | dtHistory = dt :: timing.dtHistory |> List.take 20 }


setDtMulti : Float -> Timing -> Timing
setDtMulti multi timing =
    { timing | dtMultiplier = multi }


fixedUpdate : (a -> a) -> a -> Float -> Timing -> ( a, Timing )
fixedUpdate f model dt timing =
    let
        adjustedDt : Float
        adjustedDt =
            (dt * timing.dtMultiplier) / 1000
    in
    if adjustedDt + timing.timeAccum >= timing.stepTime then
        { timing | timeAccum = (adjustedDt + timing.timeAccum) - timing.stepTime }
            |> addDtHistory dt
            |> fixedUpdate f (f model) (adjustedDt - timing.stepTime)

    else
        ( model
        , { timing | timeAccum = timing.timeAccum + adjustedDt }
            |> addDtHistory dt
        )
