module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Html, main_)
import Html.Attributes
import ParticleEngine.Particle as Particle exposing (Particle, Stick(..), mapStick)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)
import Svg exposing (Svg)
import Svg.Attributes


triangle : Float -> Vector2 -> Stick
triangle length position =
    Link (Particle.radius * 2 + length)
        (Particle.new (Vector2.new 50 0 |> Vector2.add position) 1)
        (Particle.new (Vector2.new 0 -50 |> Vector2.add position) 1)
        (Particle.new (Vector2.new -50 0 |> Vector2.add position) 1)



-- MODEL


type alias Model =
    { particles : List Stick
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ triangle 100 Vector2.zero
        , triangle 150 (Vector2.new -200 -200)
        , triangle 40 (Vector2.new 200 -300)
        , triangle 50 (Vector2.new 100 -300)
        , triangle 200 (Vector2.new -400 -300)
        , triangle 200 (Vector2.new 400 -300)
        , triangle 10 (Vector2.new 400 -300)
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
                        |> List.map (mapStick (Particle.step (Vector2.new 0 500) (dt / 1000)))
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

        Link _ p1 p2 p3 ->
            [ viewLink p1.position p2.position, viewLink p2.position p3.position, viewLink p3.position p1.position, viewParticle p1, viewParticle p2, viewParticle p3 ]


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
            , Svg.g [] (List.concatMap viewStick model.particles)
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
