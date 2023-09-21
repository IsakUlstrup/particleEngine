module Main exposing
    ( Model
    , Msg
    , RenderConfig
    , main
    )

import Browser
import Browser.Dom
import Browser.Events
import Content.Worlds
import Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import ParticleEngine.Boundary as Boundary exposing (Boundary)
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Spring exposing (Spring)
import ParticleEngine.Timing exposing (Timing)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)
import ParticleEngine.World as World exposing (World)
import SidebarView
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task



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
    { world : World
    , renderConfig : RenderConfig
    , timing : Timing
    , selected : Maybe Int
    , hoverParticle : Maybe Int
    , particleBoundary : Boundary
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Worlds.bridge
        (RenderConfig 1000 1000)
        ParticleEngine.Timing.new
        Nothing
        Nothing
        (Boundary.new Vector2.zero 1000 1000)
    , gameResize
    )



-- UPDATE


type Msg
    = Tick Float
    | ToggleForce Int
    | SetForce Int Vector2
    | SetParticlePosition Int Vector2
    | SetParticleMass Int Float
    | AddForce
    | WindowResize
    | GameViewResized (Result Browser.Dom.Error Browser.Dom.Element)
    | ClickedParticle Int
    | HoverParticle Int
    | ClickedConstraint ( Int, Int )
    | SetDtMultiplier Float
    | HoverExitParticle


physicsUpdate : Model -> Model
physicsUpdate model =
    { model
        | world =
            model.world
                |> World.updateParticles (\_ p -> Particle.applyForce (World.sumForces model.world) p)
                |> World.updateParticles (\_ p -> Particle.step model.timing.stepTime p)
                |> World.updateParticles (\_ p -> Particle.constrain model.particleBoundary p)
                |> World.constrainParticles
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( newModel, newTiming ) =
                    ParticleEngine.Timing.fixedUpdate physicsUpdate model dt model.timing
            in
            ( { newModel | timing = newTiming }
            , Cmd.none
            )

        ToggleForce targetIndex ->
            ( { model | world = World.toggleForce targetIndex model.world }, Cmd.none )

        SetForce targetIndex newForce ->
            ( { model | world = World.setForce targetIndex newForce model.world }, Cmd.none )

        SetParticlePosition id position ->
            ( { model | world = World.setParticlePosition id position model.world }, Cmd.none )

        SetParticleMass id mass ->
            ( { model | world = World.setParticleMass id mass model.world }, Cmd.none )

        AddForce ->
            ( { model | world = World.addForce Vector2.zero False model.world }, Cmd.none )

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
                                { m
                                    | world = World.addConstraint id selected m.world
                                    , selected = Nothing
                                }

                        Nothing ->
                            { m | selected = Just id }
            in
            ( toggle model, Cmd.none )

        HoverParticle id ->
            ( { model | hoverParticle = Just id }, Cmd.none )

        HoverExitParticle ->
            ( { model | hoverParticle = Nothing }, Cmd.none )

        ClickedConstraint constraintIds ->
            ( { model | world = World.removeConstraint constraintIds model.world }, Cmd.none )

        SetDtMultiplier multi ->
            ( { model | timing = ParticleEngine.Timing.setDtMulti multi model.timing }, Cmd.none )



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
    Svg.circle
        [ transform particle.position.x particle.position.y
        , Svg.Attributes.r <| String.fromInt (round Particle.radius)
        , Svg.Events.onClick <| ClickedParticle id
        , Svg.Events.onMouseOver <| HoverParticle id
        , Svg.Events.onMouseOut <| HoverExitParticle
        , svgClassList
            [ ( "selected", maybeEq id selected )
            , ( "hover", maybeEq id hovered )
            , ( "particle", True )
            ]
        ]
        []


viewConstraint : Dict Int Particle -> ( ( Int, Int ), Spring ) -> Maybe (Svg Msg)
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
    ( "Forces (" ++ (String.fromInt <| List.length forces) ++ ")"
    , [ Html.ul [] (List.indexedMap viewForce forces)
      , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.value "New force"
            , Html.Events.onClick AddForce
            ]
            []
      ]
    )


maybeEq : Int -> Maybe Int -> Bool
maybeEq n mn =
    case mn of
        Just jn ->
            jn == n

        Nothing ->
            False


viewSidebarParticle : Maybe Int -> Maybe Int -> ( Int, Particle ) -> Html Msg
viewSidebarParticle selected hovered ( id, p ) =
    Html.div
        [ Html.Events.onClick <| ClickedParticle id
        , Html.Events.onMouseOver <| HoverParticle id
        , Html.Events.onMouseOut <| HoverExitParticle
        , Html.Attributes.classList
            [ ( "selected", maybeEq id selected )
            , ( "hover", maybeEq id hovered )
            , ( "particle", True )
            ]
        ]
        [ Html.text <|
            "id: "
                ++ String.fromInt id
        , SidebarView.viewVector2Input p.position (SetParticlePosition id)
        , SidebarView.viewLabeledInput "number" (String.fromFloat p.mass) "Mass" (\i -> SetParticleMass id (i |> String.toFloat |> Maybe.withDefault 0))
        ]


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
            Dict.toList model.world.particles |> List.length

        constraintCount : Int
        constraintCount =
            Dict.toList model.world.constraints |> List.length
    in
    ( "Stats"
    , [ Html.p [] [ Html.text <| "Average FPS: " ++ fpsString model.timing.dtHistory ]
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
            [ viewSidebarForces model.world.forces
            , ( "Particles (" ++ (Dict.toList model.world.particles |> List.length |> String.fromInt) ++ ")"
              , model.world.particles |> Dict.toList |> List.map (viewSidebarParticle model.selected model.hoverParticle)
              )
            , viewSidebarStats model
            , viewSidebarTimeControls model.timing.dtMultiplier
            ]
        , Svg.svg
            [ viewBox model.renderConfig
            , Svg.Attributes.id "game-view"
            ]
            [ viewParticleBounds model.particleBoundary
            , Svg.g [] (Dict.toList model.world.constraints |> List.filterMap (viewConstraint model.world.particles))
            , Svg.g [] (Dict.toList model.world.particles |> List.map (viewParticle model.selected model.hoverParticle))
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
