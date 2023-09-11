module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import ParticleEngine.Particle as Particle exposing (Particle, Stick(..))
import ParticleEngine.Vector2 as Vector2
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { particles : Dict Int Particle
    , idCounter : Int
    , constraints : Dict ( Int, Int ) Float
    }


addParticle : Particle -> Model -> Model
addParticle particle model =
    { model
        | particles = model.particles |> Dict.insert model.idCounter particle
        , idCounter = model.idCounter + 1
    }


addConstraint : Int -> Int -> Float -> Model -> Model
addConstraint from to length model =
    { model | constraints = model.constraints |> Dict.insert ( from, to ) length }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Dict.empty
        0
        Dict.empty
        |> addParticle (Particle.new (Vector2.new -150 -150) 1)
        |> addParticle (Particle.new (Vector2.new -100 -100) 1)
        |> addParticle (Particle.new (Vector2.new -50 -50) 1)
        |> addParticle (Particle.new (Vector2.new 0 0) 1)
        |> addParticle (Particle.new (Vector2.new 50 50) 1)
        |> addParticle (Particle.new (Vector2.new 100 100) 1)
        |> addParticle (Particle.new (Vector2.new 150 150) 1)
        |> addConstraint 0 1 100
        |> addConstraint 1 5 300
        |> addConstraint 5 0 300
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | particles =
                    model.particles
                        |> Dict.map (\_ p -> Particle.step (Vector2.new 0 500) (dt / 1000) p)
                        |> Dict.map (\_ p -> Particle.constrain 500 500 p)
              }
            , Cmd.none
            )



-- VIEW


viewParticle : ( Int, Particle ) -> Svg msg
viewParticle ( _, particle ) =
    let
        transform =
            Svg.Attributes.transform <|
                "translate("
                    ++ String.fromFloat particle.position.x
                    ++ " "
                    ++ String.fromFloat particle.position.y
                    ++ ")"
    in
    Svg.circle
        [ transform
        , Svg.Attributes.r <| String.fromInt (round Particle.radius)
        , Svg.Attributes.fill "beige"
        ]
        []


viewConstraint : Dict Int Particle -> ( ( Int, Int ), Float ) -> Maybe (Svg msg)
viewConstraint particles ( ( from, to ), length ) =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            Just <|
                Svg.line
                    [ Svg.Attributes.x1 <| String.fromFloat origin.position.x
                    , Svg.Attributes.y1 <| String.fromFloat origin.position.y
                    , Svg.Attributes.x2 <| String.fromFloat target.position.x
                    , Svg.Attributes.y2 <| String.fromFloat target.position.y
                    , Svg.Attributes.stroke "beige"
                    ]
                    []

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.class "game-view"
            ]
            [ Svg.rect
                [ Svg.Attributes.width "1000"
                , Svg.Attributes.height "1000"
                , Svg.Attributes.x "-500"
                , Svg.Attributes.y "-500"
                ]
                []
            , Svg.g [] (Dict.toList model.constraints |> List.filterMap (viewConstraint model.particles))
            , Svg.g [] (Dict.toList model.particles |> List.map viewParticle)
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (min 20 >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
