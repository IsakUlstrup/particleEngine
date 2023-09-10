module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Html, main_)
import Html.Attributes
import ParticleEngine.Particle as Particle exposing (Particle, Stick(..), mapStick)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { particles : List Stick
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Link 100
            (Particle.new (Vector2.new 50 0) 1)
            (Particle.new (Vector2.new 0 -50) 1)
        , None (Particle.new (Vector2.new -50 0) 1)
        ]
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
                        |> List.map (mapStick (Particle.step (Vector2.new 300 2000) (dt / 1000)))
                        |> List.map Particle.updateStick
                        |> List.map (mapStick (Particle.constrain 500 500))
              }
            , Cmd.none
            )



-- VIEW


viewParticle : Particle -> Svg msg
viewParticle particle =
    let
        transform =
            Svg.Attributes.transform <|
                "translate("
                    ++ String.fromInt (round particle.position.x)
                    ++ " "
                    ++ String.fromInt (round particle.position.y)
                    ++ ")"
    in
    Svg.circle
        [ transform
        , Svg.Attributes.r "10"
        , Svg.Attributes.fill "beige"
        ]
        []


viewLink : Vector2 -> Vector2 -> Svg msg
viewLink from to =
    Svg.line
        [ Svg.Attributes.x1 <| String.fromInt (round from.x)
        , Svg.Attributes.y1 <| String.fromInt (round from.y)
        , Svg.Attributes.x2 <| String.fromInt (round to.x)
        , Svg.Attributes.y2 <| String.fromInt (round to.y)
        , Svg.Attributes.stroke "beige"
        ]
        []


viewStick : Stick -> List (Svg msg)
viewStick stick =
    case stick of
        None p ->
            [ viewParticle p ]

        Link _ p1 p2 ->
            [ viewLink p1.position p2.position, viewParticle p1, viewParticle p2 ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.class "game-view"
            ]
            (List.concatMap viewStick model.particles)
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
