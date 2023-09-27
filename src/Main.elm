module Main exposing
    ( Model
    , Msg
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
import ParticleEngine.Force as Force exposing (Force(..))
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Render as Render exposing (RenderConfig)
import ParticleEngine.Spring exposing (Spring)
import ParticleEngine.Timing as Timing exposing (Timing)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)
import ParticleEngine.World as World exposing (World)
import RenderSystem exposing (RenderSystem(..))
import SidebarView
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task



-- MODEL


type alias Model =
    { world : World RenderSystem
    , worlds : Dict String (World RenderSystem)
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
        (Dict.fromList
            [ ( "Bridge", Content.Worlds.bridge )
            , ( "Rope", Content.Worlds.rope )
            , ( "Ring", Content.Worlds.ring )
            , ( "Gravity", Content.Worlds.gravity )
            , ( "Cloth", Content.Worlds.cloth )
            ]
        )
        (RenderConfig 1000 1000)
        Timing.new
        Nothing
        Nothing
        (Boundary.new Vector2.zero 1000 1000)
    , gameResize
    )



-- UPDATE


physicsUpdate : Model -> Model
physicsUpdate model =
    { model
        | world =
            model.world
                |> World.applyForces
                |> World.updateParticles (\_ p -> Particle.constrain model.particleBoundary p)
                |> World.applySpringForces
                |> World.updateParticles (\_ p -> Particle.step model.timing.stepTime p)
    }


type Msg
    = Tick Float
    | ToggleForce Int
    | SetForce Int Force
    | SetParticlePosition Int Vector2
    | SetParticleMass Int Float
    | AddForce Force
    | WindowResize
    | GameViewResized (Result Browser.Dom.Error Browser.Dom.Element)
    | ClickedParticle Int
    | HoverParticle Int
    | ClickedConstraint ( Int, Int )
    | SetDtMultiplier Float
    | HoverExitParticle
    | SetWorld (World RenderSystem)
    | SetSpring ( Int, Int ) Spring
    | ToggleSystem Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( newModel, newTiming ) =
                    Timing.fixedUpdate physicsUpdate model dt model.timing
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

        AddForce force ->
            ( { model | world = World.addForce force False model.world }, Cmd.none )

        WindowResize ->
            ( model, gameResize )

        GameViewResized (Ok element) ->
            ( { model
                | renderConfig =
                    model.renderConfig
                        |> Render.setWidth element.element.width
                        |> Render.setHeight element.element.height
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
                                    | world = World.addAutoSpring id selected 100 100 m.world
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
            ( { model | world = World.removeSpring constraintIds model.world }, Cmd.none )

        SetDtMultiplier multi ->
            ( { model | timing = Timing.setDtMulti multi model.timing }, Cmd.none )

        SetWorld world ->
            ( { model | world = world }, Cmd.none )

        SetSpring connections spring ->
            ( { model | world = World.updateSpring connections (always spring) model.world }, Cmd.none )

        ToggleSystem index ->
            ( { model | world = World.toggleSystem index model.world }, Cmd.none )



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


viewConstraint : Float -> Dict Int Particle -> ( ( Int, Int ), Spring ) -> Maybe (Svg Msg)
viewConstraint strokeWidth particles ( ( from, to ), _ ) =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            Just <|
                Svg.line
                    [ Svg.Attributes.x1 <| String.fromFloat origin.position.x
                    , Svg.Attributes.y1 <| String.fromFloat origin.position.y
                    , Svg.Attributes.x2 <| String.fromFloat target.position.x
                    , Svg.Attributes.y2 <| String.fromFloat target.position.y
                    , Svg.Attributes.stroke "beige"
                    , Svg.Attributes.strokeWidth <| String.fromFloat strokeWidth
                    , Svg.Attributes.strokeLinecap "round"
                    , Svg.Events.onClick <| ClickedConstraint ( from, to )
                    , Svg.Attributes.class "constraint"
                    ]
                    []

        _ ->
            Nothing


viewSidebarForces : List ( Bool, Force ) -> ( String, List (Html Msg) )
viewSidebarForces forces =
    let
        viewForce : Int -> ( Bool, Force ) -> Html Msg
        viewForce index ( enabled, force ) =
            let
                forceType : String
                forceType =
                    case force of
                        Realative _ ->
                            "Relative"

                        Absolute _ ->
                            "Absolute"
            in
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
                , Html.p [] [ Html.text forceType ]
                , case force of
                    Force.Realative f ->
                        SidebarView.viewVector2Input f (\x -> SetForce index (Force.Realative x))

                    Force.Absolute f ->
                        SidebarView.viewVector2Input f (\x -> SetForce index (Force.Absolute x))
                ]
    in
    ( "Forces (" ++ (String.fromInt <| List.length forces) ++ ")"
    , [ Html.ul [] (List.indexedMap viewForce forces)
      , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.value "New relative force"
            , Html.Events.onClick (AddForce (Force.Realative Vector2.zero))
            ]
            []
      , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.value "New absolute force"
            , Html.Events.onClick (AddForce (Force.Absolute Vector2.zero))
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
        fpsString : Timing -> String
        fpsString timing =
            Timing.averageFps timing
                |> String.fromFloat
                |> String.split "."
                |> List.head
                |> Maybe.withDefault "-"
    in
    ( "Stats"
    , [ Html.p [] [ Html.text <| "Average FPS: " ++ fpsString model.timing ]
      ]
    )


viewSidebarTimeControls : Float -> ( String, List (Html Msg) )
viewSidebarTimeControls dtMulti =
    let
        timeButton : Float -> Html Msg
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


viewSidebarSprings : Dict ( Int, Int ) Spring -> ( String, List (Html Msg) )
viewSidebarSprings springs =
    let
        springList : List ( ( Int, Int ), Spring )
        springList =
            Dict.toList springs

        viewSpring : ( ( Int, Int ), Spring ) -> Html Msg
        viewSpring ( ( from, to ), spring ) =
            Html.div []
                [ Html.p [] [ Html.text <| String.fromInt from ++ ", " ++ String.fromInt to ]
                , SidebarView.viewLabeledInput "number" (String.fromFloat spring.length) "Length" (\i -> SetSpring ( from, to ) { spring | length = String.toFloat i |> Maybe.withDefault spring.length })
                , SidebarView.viewLabeledInput "number" (String.fromFloat spring.rate) "Rate" (\i -> SetSpring ( from, to ) { spring | rate = String.toFloat i |> Maybe.withDefault spring.rate })
                , SidebarView.viewLabeledInput "number" (String.fromFloat spring.damping) "Damping" (\i -> SetSpring ( from, to ) { spring | damping = String.toFloat i |> Maybe.withDefault spring.damping })
                ]
    in
    ( "Springs (" ++ (String.fromInt <| List.length springList) ++ ")", springList |> List.map viewSpring )


viewSidebarWorld : ( String, World RenderSystem ) -> Html Msg
viewSidebarWorld ( name, world ) =
    Html.input
        [ Html.Attributes.type_ "button"
        , Html.Attributes.value name
        , Html.Events.onClick <| SetWorld world
        ]
        []


viewParticleBounds : Boundary -> Svg msg
viewParticleBounds boundary =
    Svg.rect
        [ Svg.Attributes.width <| String.fromFloat boundary.width
        , Svg.Attributes.height <| String.fromFloat boundary.height
        , Svg.Attributes.x <| String.fromFloat (boundary.center.x - (boundary.width / 2))
        , Svg.Attributes.y <| String.fromFloat (boundary.center.y - (boundary.height / 2))
        , Svg.Attributes.rx "5"
        , Svg.Attributes.class "bounds"
        ]
        []


viewSidebarRenderSystem : Int -> ( Bool, RenderSystem ) -> Html Msg
viewSidebarRenderSystem index ( enabled, system ) =
    Html.div [ Html.Attributes.class "labeled-checkbox" ]
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked enabled
            , Html.Events.onClick (ToggleSystem index)
            , Html.Attributes.id <| "system-enabled" ++ String.fromInt index
            ]
            []
        , Html.label [ Html.Attributes.for <| "system-enabled" ++ String.fromInt index ] [ Html.text <| RenderSystem.toString system ]
        ]


runRenderSystem : Boundary -> Maybe Int -> Maybe Int -> World RenderSystem -> RenderSystem -> Svg Msg
runRenderSystem boundary selected hovered world system =
    case system of
        RenderParticles ->
            Svg.g [] (Dict.toList world.particles |> List.map (viewParticle selected hovered))

        RenderSprings width ->
            Svg.g [] (Dict.toList world.springs |> List.filterMap (viewConstraint width world.particles))

        RenderBoundary ->
            viewParticleBounds boundary


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ SidebarView.viewSidebar
            [ viewSidebarForces model.world.forces
            , ( "Particles (" ++ (Dict.toList model.world.particles |> List.length |> String.fromInt) ++ ")"
              , model.world.particles |> Dict.toList |> List.map (viewSidebarParticle model.selected model.hoverParticle)
              )
            , viewSidebarSprings model.world.springs
            , ( "Render systems (" ++ (model.world.renderSystems |> List.length |> String.fromInt) ++ ")"
              , model.world.renderSystems |> List.indexedMap viewSidebarRenderSystem
              )
            , viewSidebarStats model
            , viewSidebarTimeControls model.timing.dtMultiplier
            , ( "Worlds"
              , model.worlds |> Dict.toList |> List.map viewSidebarWorld
              )
            ]
        , Render.viewWorld (runRenderSystem model.particleBoundary model.selected model.hoverParticle) model.renderConfig model.world
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
