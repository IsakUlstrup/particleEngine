module Main exposing
    ( Model
    , Msg
    , RenderConfig
    , main
    )

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)
import SidebarView
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task


nGon : Vector2 -> Int -> Float -> List Particle
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

        newParticle : Int -> Particle
        newParticle index =
            Particle.new (position index) 1
    in
    List.range 0 (points - 1) |> List.map newParticle



-- MODEL


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
    , dtMultiplier : Float
    , renderConfig : RenderConfig
    , dtHistory : List Float
    , selected : Maybe Int
    }


addParticle : Particle -> Model -> Model
addParticle particle model =
    { model
        | particles = model.particles |> Dict.insert model.idCounter particle
        , idCounter = model.idCounter + 1
    }


addParticles : List Particle -> Model -> Model
addParticles particles model =
    List.foldl addParticle model particles


addConstraint : Int -> Int -> Model -> Model
addConstraint from to model =
    case particleDistance from to model.particles of
        Just dist ->
            { model | constraints = model.constraints |> Dict.insert ( from, to ) dist }

        Nothing ->
            model


particleDistance : Int -> Int -> Dict Int Particle -> Maybe Float
particleDistance one two particles =
    case ( Dict.get one particles, Dict.get two particles ) of
        ( Just p1, Just p2 ) ->
            Just <| Vector2.distance p1.position p2.position

        _ ->
            Nothing


addParticleList : List ( Float, Float ) -> Model -> Model
addParticleList positions model =
    let
        particle : ( Float, Float ) -> Particle
        particle ( x, y ) =
            Particle.new (Vector2.new x y) 1

        particles : List Particle
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
        [ ( False, Vector2.new 0 300 )
        , ( False, Vector2.new 200 0 )
        ]
        0.02
        0
        1
        (RenderConfig 1000 1000)
        []
        Nothing
        |> addParticleList
            [ ( -50, -50 )
            , ( 50, -50 )
            , ( 50, 50 )
            , ( -50, 50 )
            ]
        |> addConstraint 0 1
        |> addConstraint 1 2
        |> addConstraint 2 3
        |> addConstraint 3 0
        |> addConstraint 0 2
        |> addConstraint 1 3
        |> addParticles (nGon (Vector2.new -300 0) 20 100)
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
    | ClickedParticle Int
    | ClickedConstraint ( Int, Int )
    | SetDtMultiplier Float


fixedUpdate : Float -> Model -> Model
fixedUpdate dt model =
    let
        adjustedDt : Float
        adjustedDt =
            dt * model.dtMultiplier
    in
    if adjustedDt >= model.stepTime then
        let
            sumForces : Vector2
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
            | timeAccum = adjustedDt - model.stepTime
            , particles =
                model.particles
                    |> Dict.map (\_ p -> Particle.applyForce sumForces p)
                    |> Dict.map (\_ p -> Particle.step model.stepTime p)
                    |> Dict.map (\_ p -> Particle.constrain (model.renderConfig.width / 2) (model.renderConfig.height / 2) p)
                    |> constrainParticles model.constraints
        }
            |> fixedUpdate (adjustedDt - model.stepTime)

    else
        { model | timeAccum = model.timeAccum + adjustedDt }


addToDtHistory : Float -> Model -> Model
addToDtHistory dt model =
    { model | dtHistory = dt :: model.dtHistory |> List.take 20 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> fixedUpdate (model.timeAccum + (dt / 1000))
                |> addToDtHistory dt
            , Cmd.none
            )

        ToggleForce targetIndex ->
            let
                toggleForce : Int -> ( Bool, Vector2 ) -> ( Bool, Vector2 )
                toggleForce index force =
                    if targetIndex == index then
                        Tuple.mapFirst not force

                    else
                        force
            in
            ( { model | forces = List.indexedMap toggleForce model.forces }, Cmd.none )

        SetForce targetIndex newForce ->
            let
                setForce : Int -> ( Bool, Vector2 ) -> ( Bool, Vector2 )
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

        ClickedParticle id ->
            let
                toggle : Model -> Model
                toggle m =
                    case m.selected of
                        Just selected ->
                            if selected == id then
                                { m | selected = Nothing }

                            else
                                m
                                    |> addConstraint id selected
                                    |> (\x -> { x | selected = Nothing })

                        Nothing ->
                            { m | selected = Just id }
            in
            ( toggle model, Cmd.none )

        ClickedConstraint constraintIds ->
            let
                keepConstraint : ( Int, Int ) -> Float -> Bool
                keepConstraint ids _ =
                    ids /= constraintIds
            in
            ( { model | constraints = Dict.filter keepConstraint model.constraints }, Cmd.none )

        SetDtMultiplier multi ->
            ( { model | dtMultiplier = multi }, Cmd.none )



-- VIEW


transform : Float -> Float -> Svg.Attribute msg
transform x y =
    Svg.Attributes.transform <|
        "translate("
            ++ String.fromFloat x
            ++ " "
            ++ String.fromFloat y
            ++ ")"


viewParticle : Maybe Int -> ( Int, Particle ) -> Svg Msg
viewParticle selected ( id, particle ) =
    let
        fillColor : String
        fillColor =
            case selected of
                Just selId ->
                    if selId == id then
                        "magenta"

                    else
                        "beige"

                _ ->
                    "beige"
    in
    Svg.circle
        [ transform particle.position.x particle.position.y
        , Svg.Attributes.r <| String.fromInt (round Particle.radius)
        , Svg.Attributes.fill fillColor
        , Svg.Events.onClick <| ClickedParticle id
        , Svg.Attributes.class "particle"
        ]
        []


viewConstraint : Dict Int Particle -> ( ( Int, Int ), Float ) -> Maybe (Svg Msg)
viewConstraint particles ( ( from, to ), _ ) =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            Just <|
                Svg.line
                    [ Svg.Attributes.x1 <| String.fromFloat origin.position.x
                    , Svg.Attributes.y1 <| String.fromFloat origin.position.y
                    , Svg.Attributes.x2 <| String.fromFloat target.position.x
                    , Svg.Attributes.y2 <| String.fromFloat target.position.y
                    , Svg.Attributes.stroke "beige"
                    , Svg.Attributes.strokeWidth "3"
                    , Svg.Events.onClick <| ClickedConstraint ( from, to )
                    , Svg.Attributes.class "constraint"
                    ]
                    []

        _ ->
            Nothing


viewSidebarForces : List ( Bool, Vector2 ) -> Html Msg
viewSidebarForces forces =
    let
        viewForce : Int -> ( Bool, Vector2 ) -> Html Msg
        viewForce index ( enabled, force ) =
            Html.li []
                [ Html.div []
                    [ Html.input
                        [ Html.Attributes.type_ "checkbox"
                        , Html.Attributes.checked enabled
                        , Html.Events.onClick (ToggleForce index)
                        , Html.Attributes.id <| "force-enabled" ++ String.fromInt index
                        ]
                        []
                    , Html.label [ Html.Attributes.for <| "force-enabled" ++ String.fromInt index ] [ Html.text "Enabled" ]
                    ]
                , Html.div []
                    [ Html.label [ Html.Attributes.for <| "force-x" ++ String.fromInt index ] [ Html.text "x" ]
                    , Html.input
                        [ Html.Attributes.type_ "number"
                        , Html.Attributes.value <| String.fromFloat force.x
                        , Html.Attributes.step "0.1"
                        , Html.Events.onInput (\i -> SetForce index (Vector2.mapX (always (Maybe.withDefault 0 (String.toFloat i))) force))
                        , Html.Attributes.id <| "force-x" ++ String.fromInt index
                        ]
                        []
                    ]
                , Html.div []
                    [ Html.label [ Html.Attributes.for <| "force-y" ++ String.fromInt index ] [ Html.text "y" ]
                    , Html.input
                        [ Html.Attributes.type_ "number"
                        , Html.Attributes.value <| String.fromFloat force.y
                        , Html.Attributes.step "0.1"
                        , Html.Events.onInput (\i -> SetForce index (Vector2.mapY (always (Maybe.withDefault 0 (String.toFloat i))) force))
                        , Html.Attributes.id <| "force-y" ++ String.fromInt index
                        ]
                        []
                    ]
                ]
    in
    SidebarView.viewDetails "Forces"
        [ Html.ul [] (List.indexedMap viewForce forces)
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

        particleCount : Int
        particleCount =
            Dict.toList model.particles |> List.length

        constraintCount : Int
        constraintCount =
            Dict.toList model.constraints |> List.length
    in
    SidebarView.viewDetails "Stats"
        [ Html.p [] [ Html.text <| "Average FPS: " ++ fpsString model.dtHistory ]
        , Html.p [] [ Html.text <| "Particle count: " ++ String.fromInt particleCount ]
        , Html.p [] [ Html.text <| "Constraint count: " ++ String.fromInt constraintCount ]
        ]


viewSidebarTimeControls : Float -> Html Msg
viewSidebarTimeControls dtMulti =
    SidebarView.viewDetails "Stats"
        [ Html.p [] [ Html.text <| String.fromFloat dtMulti ]
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.max "1"
            , Html.Attributes.step "0.1"
            , Html.Attributes.value <| String.fromFloat dtMulti
            , Html.Events.onInput (String.toFloat >> Maybe.withDefault 1 >> SetDtMultiplier)
            ]
            []
        , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.value "Pause"
            , Html.Events.onClick <| SetDtMultiplier 0
            ]
            []
        , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.value "Resume"
            , Html.Events.onClick <| SetDtMultiplier 1
            ]
            []
        ]


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
            , viewSidebarTimeControls model.dtMultiplier
            ]
        , Svg.svg
            [ viewBox model.renderConfig
            , Svg.Attributes.id "game-view"
            ]
            [ Svg.rect
                [ Svg.Attributes.width <| String.fromFloat model.renderConfig.width
                , Svg.Attributes.height <| String.fromFloat model.renderConfig.height
                , Svg.Attributes.x <| String.fromFloat -(model.renderConfig.width / 2)
                , Svg.Attributes.y <| String.fromFloat -(model.renderConfig.height / 2)
                , Svg.Attributes.class "bounds"
                ]
                []
            , Svg.g [] (Dict.toList model.constraints |> List.filterMap (viewConstraint model.particles))
            , Svg.g [] (Dict.toList model.particles |> List.map (viewParticle model.selected))
            ]
        ]



-- SUBSCRIPTIONS


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
