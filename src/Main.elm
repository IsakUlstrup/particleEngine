module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task



-- MODEL


type alias Pointer =
    { position : Vector2
    , pressed : Bool
    }


type alias RenderConfig =
    { width : Float
    , height : Float
    }


setWidth : Float -> RenderConfig -> RenderConfig
setWidth width config =
    { config | width = width }


setHeight : Float -> RenderConfig -> RenderConfig
setHeight height config =
    { config | height = height }


type alias Model =
    { particles : Dict Int Particle
    , idCounter : Int
    , constraints : Dict ( Int, Int ) Float
    , forces : List ( Bool, Vector2 )
    , stepTime : Float
    , timeAccum : Float
    , renderConfig : RenderConfig
    , pointer : Pointer
    , dtHistory : List Float
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


addParticleList : List ( Float, Float ) -> Model -> Model
addParticleList positions model =
    let
        particle ( x, y ) =
            Particle.new (Vector2.new x y) 1

        particles =
            List.map particle positions
    in
    List.foldl addParticle model particles


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Dict.empty
        0
        Dict.empty
        [ ( False, Vector2.new 0 500 )
        , ( False, Vector2.new 200 0 )
        ]
        0.02
        0
        (RenderConfig 1000 1000)
        (Pointer Vector2.zero False)
        []
        |> addParticleList
            [ ( -50, -50 )
            , ( 50, -50 )
            , ( 50, 50 )
            , ( -50, 50 )
            ]
        |> addConstraint 0 1 100
        |> addConstraint 1 2 100
        |> addConstraint 2 3 100
        |> addConstraint 3 0 100
        |> addConstraint 0 2 141.42
        |> addConstraint 1 3 141.42
    , gameResize
    )



-- UPDATE


constrainPair : ( ( Int, Int ), Float ) -> Dict Int Particle -> Dict Int Particle
constrainPair ( ( from, to ), length ) particles =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            let
                ( p1, p2 ) =
                    Particle.constrainStick length ( origin, target )
            in
            particles
                |> Dict.insert from p1
                |> Dict.insert to p2

        _ ->
            particles


constrainParticles : Dict ( Int, Int ) Float -> Dict Int Particle -> Dict Int Particle
constrainParticles constraints particles =
    List.foldl constrainPair particles (Dict.toList constraints)


type Msg
    = Tick Float
    | ToggleForce Int
    | SetForce Int Vector2
    | AddForce
    | WindowResize
    | GameViewResized (Result Browser.Dom.Error Browser.Dom.Element)
    | PointerChanged Float Float Bool


fixedUpdate : Float -> Model -> Model
fixedUpdate dt model =
    if dt >= model.stepTime then
        let
            sumForces =
                List.foldl Vector2.add
                    Vector2.zero
                    (List.filterMap
                        (\( e, f ) ->
                            if e then
                                Just f

                            else
                                Nothing
                        )
                        model.forces
                    )
        in
        { model
            | timeAccum = dt - model.stepTime
            , particles =
                model.particles
                    |> Dict.map (\_ p -> Particle.step sumForces model.stepTime p)
                    |> Dict.map (\_ p -> Particle.constrain (model.renderConfig.width / 2) (model.renderConfig.height / 2) p)
                    |> constrainParticles model.constraints
        }
            |> fixedUpdate (dt - model.stepTime)

    else
        { model | timeAccum = model.timeAccum + dt }


addParticleIsPointerPressed : Model -> Model
addParticleIsPointerPressed model =
    if model.pointer.pressed then
        addParticle (Particle.new model.pointer.position 1) model

    else
        model


addToDtHistory : Float -> Model -> Model
addToDtHistory dt model =
    { model | dtHistory = dt :: model.dtHistory |> List.take 20 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> fixedUpdate (model.timeAccum + (dt / 1000))
                |> addParticleIsPointerPressed
                |> addToDtHistory dt
            , Cmd.none
            )

        ToggleForce targetIndex ->
            let
                toggleForce index force =
                    if targetIndex == index then
                        Tuple.mapFirst not force

                    else
                        force
            in
            ( { model | forces = List.indexedMap toggleForce model.forces }, Cmd.none )

        SetForce targetIndex newForce ->
            let
                setForce index force =
                    if index == targetIndex then
                        Tuple.mapSecond (always newForce) force

                    else
                        force
            in
            ( { model | forces = List.indexedMap setForce model.forces }, Cmd.none )

        AddForce ->
            ( { model | forces = ( False, Vector2.zero ) :: model.forces }, Cmd.none )

        WindowResize ->
            ( model, gameResize )

        GameViewResized (Ok element) ->
            ( { model
                | renderConfig =
                    model.renderConfig
                        |> setWidth element.element.width
                        |> setHeight element.element.height
              }
            , Cmd.none
            )

        GameViewResized (Err _) ->
            ( model, Cmd.none )

        PointerChanged x y pressed ->
            ( { model | pointer = Pointer (Vector2.new (x - model.renderConfig.width / 2) (y - model.renderConfig.height / 2)) pressed }, Cmd.none )



-- VIEW


transform : Float -> Float -> Svg.Attribute msg
transform x y =
    Svg.Attributes.transform <|
        "translate("
            ++ String.fromFloat x
            ++ " "
            ++ String.fromFloat y
            ++ ")"


viewParticle : ( Int, Particle ) -> Svg msg
viewParticle ( _, particle ) =
    Svg.circle
        [ transform particle.position.x particle.position.y
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


viewSidebarForces : List ( Bool, Vector2 ) -> Html Msg
viewSidebarForces forces =
    let
        viewForce index ( enabled, force ) =
            Html.li []
                [ Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.checked enabled
                    , Html.Events.onClick (ToggleForce index)
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.value <| String.fromFloat force.x
                    , Html.Attributes.step "0.1"
                    , Html.Events.onInput (\i -> SetForce index (Vector2.mapX (always (Maybe.withDefault 0 (String.toFloat i))) force))
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.value <| String.fromFloat force.y
                    , Html.Attributes.step "0.1"
                    , Html.Events.onInput (\i -> SetForce index (Vector2.mapY (always (Maybe.withDefault 0 (String.toFloat i))) force))
                    ]
                    []
                ]
    in
    Html.details []
        [ Html.summary [] [ Html.text "Forces" ]
        , Html.ul [] (List.indexedMap viewForce forces)
        , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.value "New force"
            , Html.Events.onClick AddForce
            ]
            []
        ]


viewSidebarStats : Model -> Html msg
viewSidebarStats model =
    let
        fpsString : List Float -> String
        fpsString dts =
            let
                averageDelta : Float
                averageDelta =
                    List.sum dts / toFloat (List.length dts)

                averageFps : Float
                averageFps =
                    1000 / averageDelta
            in
            averageFps
                |> String.fromFloat
                |> String.split "."
                |> List.head
                |> Maybe.withDefault "-"

        particleCount =
            Dict.toList model.particles |> List.length
    in
    Html.details []
        [ Html.summary [] [ Html.text "Stats" ]
        , Html.p [] [ Html.text <| "Average FPS: " ++ fpsString model.dtHistory ]
        , Html.p [] [ Html.text <| "Particle count: " ++ String.fromInt particleCount ]
        ]


viewPointer : Pointer -> Svg msg
viewPointer pointer =
    let
        fillColor =
            if pointer.pressed then
                "beige"

            else
                "none"
    in
    Svg.circle
        [ transform pointer.position.x pointer.position.y
        , Svg.Attributes.r "20"
        , Svg.Attributes.stroke "beige"
        , Svg.Attributes.fill fillColor
        ]
        []


viewBox : RenderConfig -> Svg.Attribute msg
viewBox config =
    Svg.Attributes.viewBox <|
        String.fromFloat -(config.width / 2)
            ++ " "
            ++ String.fromFloat -(config.height / 2)
            ++ " "
            ++ String.fromFloat config.width
            ++ " "
            ++ String.fromFloat config.height


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.section [ Html.Attributes.class "sidebar" ]
            [ viewSidebarForces model.forces
            , viewSidebarStats model
            ]
        , Svg.svg
            [ viewBox model.renderConfig
            , Svg.Attributes.id "game-view"
            , Svg.Events.on "pointermove" pointerDecoder
            , Svg.Events.on "pointerdown" pointerDecoder
            , Svg.Events.on "pointerup" pointerDecoder
            , Svg.Events.on "pointercancel" pointerDecoder
            ]
            [ Svg.rect
                [ Svg.Attributes.width <| String.fromFloat model.renderConfig.width
                , Svg.Attributes.height <| String.fromFloat model.renderConfig.height
                , Svg.Attributes.x <| String.fromFloat -(model.renderConfig.width / 2)
                , Svg.Attributes.y <| String.fromFloat -(model.renderConfig.height / 2)
                , Svg.Attributes.class "constraint"
                ]
                []
            , Svg.g [] (Dict.toList model.constraints |> List.filterMap (viewConstraint model.particles))
            , Svg.g [] (Dict.toList model.particles |> List.map viewParticle)
            , viewPointer model.pointer
            ]
        ]



-- SUBSCRIPTIONS


pressedDecoder : Decoder Bool
pressedDecoder =
    Decode.field "buttons" Decode.float
        |> Decode.map (\p -> p > 0)


pointerDecoder : Decoder Msg
pointerDecoder =
    Decode.map3 PointerChanged
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        pressedDecoder


gameResize : Cmd Msg
gameResize =
    Browser.Dom.getElement "game-view" |> Task.attempt GameViewResized


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (min 2000 >> Tick)
        , Browser.Events.onResize (\_ _ -> WindowResize)
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
