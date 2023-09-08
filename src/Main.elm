module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, main_)
import Html.Attributes
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Vector2 as Vector2
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    List Particle


init : () -> ( Model, Cmd Msg )
init _ =
    ( [ Particle.new (Vector2.new 0 0) 100
      , Particle.new (Vector2.new 100 0) 100
      , Particle.new (Vector2.new -100 -50) 100
      ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.class "game-view"
            ]
            (List.map viewParticle model)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
