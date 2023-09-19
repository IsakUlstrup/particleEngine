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
import ParticleEngine.Boundary as Boundary exposing (Boundary)
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
    , hoverParticle : Maybe Int
    , particleBoundary : Boundary
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
            Particle.new (Vector2.new x y) 0

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
        Nothing
        (Boundary.new Vector2.zero 1000 1000)
        |> addParticleList
            [ ( 0, -50 )
            , ( 100, -50 )
            , ( 100, 50 )
            , ( 0, 50 )
            ]
        |> addConstraint 0 1
        |> addConstraint 1 2
        |> addConstraint 2 3
        |> addConstraint 3 0
        |> addConstraint 0 2
        |> addConstraint 1 3
        |> addParticles (nGon Vector2.zero 20 400)
        |> addParticles (nGon (Vector2.new -100 0) 6 40)
    , gameResize
    )



-- UPDATE


constrainPair : ( ( Int, Int ), Float ) -> Dict Int Particle -> Dict Int Particle
constrainPair ( ( from, to ), length ) particles =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            let
                ( p1, p2 ) =
                    Particle.enforceConstraint length ( origin, target )
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
    | SetParticlePosition Int Vector2
    | AddForce
    | WindowResize
    | GameViewResized (Result Browser.Dom.Error Browser.Dom.Element)
    | ClickedParticle Int
    | HoverParticle Int
    | ClickedConstraint ( Int, Int )
    | SetDtMultiplier Float
    | HoverExitParticle


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
                    |> Dict.map (\_ p -> Particle.constrain model.particleBoundary p)
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

        SetParticlePosition id position ->
            let
                updatePosition : Particle -> Particle
                updatePosition p =
                    { p | position = position }
            in
            ( { model | particles = Dict.update id (Maybe.map updatePosition) model.particles }, Cmd.none )

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
                , particleBoundary =
                    model.particleBoundary
                        |> Boundary.setWidth element.element.width
                        |> Boundary.setHeight element.element.height
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

        HoverParticle id ->
            ( { model | hoverParticle = Just id }, Cmd.none )

        HoverExitParticle ->
            ( { model | hoverParticle = Nothing }, Cmd.none )

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


svgClassList : List ( String, Bool ) -> Svg.Attribute msg
svgClassList classes =
    classes
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.intersperse " "
        |> String.concat
        |> Svg.Attributes.class


viewParticle : Maybe Int -> Maybe Int -> ( Int, Particle ) -> Svg Msg
viewParticle selected hovered ( id, particle ) =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Just selectedId ->
                    selectedId == id

                Nothing ->
                    False

        isHovered : Bool
        isHovered =
            case hovered of
                Just selectedId ->
                    selectedId == id

                Nothing ->
                    False
    in
    Svg.circle
        [ transform particle.position.x particle.position.y
        , Svg.Attributes.r <| String.fromInt (round Particle.radius)
        , Svg.Events.onClick <| ClickedParticle id
        , Html.Events.onMouseOver <| HoverParticle id
        , Html.Events.onMouseOut <| HoverExitParticle
        , svgClassList
            [ ( "selected", isSelected )
            , ( "hover", isHovered )
            , ( "particle", True )
            ]
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


viewSidebarForces : List ( Bool, Vector2 ) -> ( String, List (Html Msg) )
viewSidebarForces forces =
    let
        viewForce : Int -> ( Bool, Vector2 ) -> Html Msg
        viewForce index ( enabled, force ) =
            Html.li []
                [ Html.div [ Html.Attributes.class "labeled-checkbox" ]
                    [ Html.input
                        [ Html.Attributes.type_ "checkbox"
                        , Html.Attributes.checked enabled
                        , Html.Events.onClick (ToggleForce index)
                        , Html.Attributes.id <| "force-enabled" ++ String.fromInt index
                        ]
                        []
                    , Html.label [ Html.Attributes.for <| "force-enabled" ++ String.fromInt index ] [ Html.text "Enabled" ]
                    ]
                , SidebarView.viewVector2Input force (SetForce index)
                ]
    in
    ( "Forces"
    , [ Html.ul [] (List.indexedMap viewForce forces)
      , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.value "New force"
            , Html.Events.onClick AddForce
            ]
            []
      ]
    )


viewSidebarParticles : Maybe Int -> Maybe Int -> Dict Int Particle -> ( String, List (Html Msg) )
viewSidebarParticles selected hovered particles =
    let
        isSelected : Int -> Bool
        isSelected id =
            case selected of
                Just selectedId ->
                    selectedId == id

                Nothing ->
                    False

        isHovered : Int -> Bool
        isHovered id =
            case hovered of
                Just selectedId ->
                    selectedId == id

                Nothing ->
                    False

        particle : ( Int, Particle ) -> Html Msg
        particle ( id, p ) =
            Html.div
                [ Html.Events.onClick <| ClickedParticle id
                , Html.Events.onMouseOver <| HoverParticle id
                , Html.Events.onMouseOut <| HoverExitParticle
                , Html.Attributes.classList
                    [ ( "selected", isSelected id )
                    , ( "hover", isHovered id )
                    , ( "particle", True )
                    ]
                ]
                [ Html.text <|
                    "id: "
                        ++ String.fromInt id
                , SidebarView.viewVector2Input p.position (SetParticlePosition id)
                ]
    in
    ( "Particles"
    , particles |> Dict.toList |> List.map particle
    )


viewSidebarStats : Model -> ( String, List (Html Msg) )
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
    ( "Stats"
    , [ Html.p [] [ Html.text <| "Average FPS: " ++ fpsString model.dtHistory ]
      , Html.p [] [ Html.text <| "Particle count: " ++ String.fromInt particleCount ]
      , Html.p [] [ Html.text <| "Constraint count: " ++ String.fromInt constraintCount ]
      ]
    )


viewSidebarTimeControls : Float -> ( String, List (Html Msg) )
viewSidebarTimeControls dtMulti =
    let
        timeButton n =
            Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.value <| String.fromFloat n ++ "x"
                , Html.Attributes.classList [ ( "selected", n == dtMulti ) ]
                , Html.Events.onClick <| SetDtMultiplier n
                ]
                []
    in
    ( "Time"
    , [ SidebarView.buttonGroup [ timeButton 0, timeButton 0.5, timeButton 1 ]
      ]
    )


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


viewParticleBounds : Boundary -> Svg msg
viewParticleBounds boundary =
    Svg.rect
        [ Svg.Attributes.width <| String.fromFloat boundary.width
        , Svg.Attributes.height <| String.fromFloat boundary.height
        , Svg.Attributes.x <| String.fromFloat (boundary.center.x - (boundary.width / 2))
        , Svg.Attributes.y <| String.fromFloat (boundary.center.y - (boundary.height / 2))
        , Svg.Attributes.rx "10"
        , Svg.Attributes.class "bounds"
        ]
        []


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ SidebarView.viewSidebar
            [ viewSidebarForces model.forces
            , viewSidebarParticles model.selected model.hoverParticle model.particles
            , viewSidebarStats model
            , viewSidebarTimeControls model.dtMultiplier
            ]
        , Svg.svg
            [ viewBox model.renderConfig
            , Svg.Attributes.id "game-view"
            ]
            [ viewParticleBounds model.particleBoundary
            , Svg.g [] (Dict.toList model.constraints |> List.filterMap (viewConstraint model.particles))
            , Svg.g [] (Dict.toList model.particles |> List.map (viewParticle model.selected model.hoverParticle))
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
