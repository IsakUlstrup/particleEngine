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
import ParticleEngine.Boundary exposing (Boundary)
import ParticleEngine.Particle as Particle exposing (Particle)
import ParticleEngine.Render as Render exposing (RenderConfig)
import ParticleEngine.Spring as Spring exposing (Spring)
import ParticleEngine.Vector2 as Vector2 exposing (Vector2)
import ParticleEngine.World as World exposing (World)
import SidebarView
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Keyed
import System exposing (System(..))
import Task



-- MODEL


type alias Model =
    { world : World System
    , worlds : Dict String (World System)
    , renderConfig : RenderConfig
    , selected : Maybe Int
    , hoverParticle : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Worlds.bridge
        (Dict.fromList
            [ ( "Bridge", Content.Worlds.bridge )
            , ( "Rope", Content.Worlds.rope )
            , ( "Ball", Content.Worlds.ball )
            , ( "Gravity", Content.Worlds.gravity )
            , ( "Cloth", Content.Worlds.cloth )
            , ( "Spawn", Content.Worlds.spawn )
            ]
        )
        (RenderConfig 1000 1000 0.7 Vector2.zero)
        Nothing
        Nothing
    , gameResize
    )



-- UPDATE


type Msg
    = Tick Float
    | SetParticlePosition Int Vector2
    | SetParticleMass Int Float
    | WindowResize
    | GameViewResized (Result Browser.Dom.Error Browser.Dom.Element)
    | ClickedParticle Int
    | HoverParticle Int
    | ClickedConstraint ( Int, Int )
    | SetDtMultiplier Float
    | HoverExitParticle
    | SetWorld (World System)
    | SetSpring ( Int, Int ) Spring
    | ToggleSystem Int
    | SetCameraZoom Float
    | SetCameraPosition Vector2
    | SetStepTime Float


runSystem : System -> World System -> World System
runSystem system world =
    case system of
        ConstrainParticles b ->
            World.updateParticles (Particle.constrain b) world

        RenderParticles ->
            world

        RenderParticleVelocity ->
            world

        RenderSprings _ ->
            world

        RenderSpringStress ->
            world

        Force f ->
            World.updateParticles (Particle.applyForce f) world

        Gravity f ->
            World.updateParticles (Particle.applyGravity f) world

        BreakSprings ->
            let
                deltaLength : Int -> Int -> Spring -> Maybe Float
                deltaLength from to spring =
                    world
                        |> World.particleDistance from to
                        |> Maybe.map (\d -> abs (d - spring.length))

                fiddlesticks ( from, to ) spring =
                    case deltaLength from to spring of
                        Just length ->
                            length <= spring.length * 1.02

                        Nothing ->
                            False
            in
            World.filterSprings fiddlesticks world

        SpawnParticle ( cd, _ ) particle ->
            if cd <= 0 then
                World.addParticle particle world

            else
                world


updateSystem : Float -> Bool -> System -> System
updateSystem dt enabled system =
    case system of
        ConstrainParticles _ ->
            system

        RenderParticles ->
            system

        RenderParticleVelocity ->
            system

        RenderSprings _ ->
            system

        RenderSpringStress ->
            system

        Force _ ->
            system

        Gravity _ ->
            system

        BreakSprings ->
            system

        SpawnParticle ( cd, maxCd ) particle ->
            if enabled then
                if cd <= 0 then
                    SpawnParticle ( maxCd, maxCd ) particle

                else
                    SpawnParticle ( max 0 (cd - dt), maxCd ) particle

            else
                system


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | world =
                    model.world
                        |> World.mapSystems (updateSystem dt)
                        |> World.tick dt runSystem
              }
            , Cmd.none
            )

        SetParticlePosition id position ->
            ( { model | world = World.setParticlePosition id position model.world }, Cmd.none )

        SetParticleMass id mass ->
            ( { model | world = World.setParticleMass id mass model.world }, Cmd.none )

        WindowResize ->
            ( model, gameResize )

        GameViewResized (Ok element) ->
            ( { model
                | renderConfig =
                    model.renderConfig
                        |> Render.setWidth element.element.width
                        |> Render.setHeight element.element.height
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
            ( { model | world = World.setDtMulti multi model.world }, Cmd.none )

        SetWorld world ->
            ( { model | world = world }, Cmd.none )

        SetSpring connections spring ->
            ( { model | world = World.updateSpring connections (always spring) model.world }, Cmd.none )

        ToggleSystem index ->
            ( { model | world = World.toggleSystem index model.world }, Cmd.none )

        SetCameraZoom zoom ->
            ( { model | renderConfig = Render.setZoom zoom model.renderConfig }, Cmd.none )

        SetCameraPosition position ->
            ( { model | renderConfig = Render.setPosition position model.renderConfig }, Cmd.none )

        SetStepTime stepTime ->
            ( { model | world = World.setStepTime stepTime model.world }, Cmd.none )



-- VIEW


transform : Float -> Float -> Svg.Attribute msg
transform x y =
    Svg.Attributes.transform <|
        "translate("
            ++ String.fromInt (round x)
            ++ " "
            ++ String.fromInt (round y)
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

        -- , Svg.Events.onClick <| ClickedParticle id
        -- , Svg.Events.onMouseOver <| HoverParticle id
        -- , Svg.Events.onMouseOut <| HoverExitParticle
        , svgClassList
            [ ( "selected", maybeEq id selected )
            , ( "hover", maybeEq id hovered )
            , ( "particle", True )
            ]
        ]
        []


viewKeyedParticle : Maybe Int -> Maybe Int -> ( Int, Particle ) -> ( String, Svg Msg )
viewKeyedParticle selected hovered ( id, particle ) =
    ( String.fromInt id, viewParticle selected hovered ( id, particle ) )


viewConstraint : Float -> Dict Int Particle -> ( ( Int, Int ), Spring ) -> Maybe (Svg Msg)
viewConstraint strokeWidth particles ( ( from, to ), _ ) =
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just origin, Just target ) ->
            Just <|
                Svg.line
                    [ Svg.Attributes.x1 <| String.fromInt (round origin.position.x)
                    , Svg.Attributes.y1 <| String.fromInt (round origin.position.y)
                    , Svg.Attributes.x2 <| String.fromInt (round target.position.x)
                    , Svg.Attributes.y2 <| String.fromInt (round target.position.y)
                    , Svg.Attributes.stroke "beige"
                    , Svg.Attributes.strokeWidth <| String.fromFloat strokeWidth
                    , Svg.Attributes.strokeLinecap "round"

                    -- , Svg.Events.onClick <| ClickedConstraint ( from, to )
                    , Svg.Attributes.class "constraint"
                    ]
                    []

        _ ->
            Nothing


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
        [ Html.Events.onClick (ClickedParticle id)
        , Html.Events.onMouseOver (HoverParticle id)
        , Html.Events.onMouseOut HoverExitParticle
        , Html.Attributes.classList
            [ ( "selected", maybeEq id selected )
            , ( "hover", maybeEq id hovered )
            , ( "particle", True )
            ]
        ]
        [ Html.text ("id: " ++ String.fromInt id)
        , SidebarView.viewVector2Input p.position (SetParticlePosition id)
        , SidebarView.viewLabeledNumberInput 1 p.mass "Mass" (SetParticleMass id)
        ]


viewSidebarStats : Model -> ( String, List (Html Msg) )
viewSidebarStats model =
    let
        fpsString : World a -> String
        fpsString world =
            World.averageFps world
                |> String.fromFloat
                |> String.split "."
                |> List.head
                |> Maybe.withDefault "-"
    in
    ( "Stats"
    , [ Html.p [] [ Html.text ("Average FPS: " ++ fpsString model.world) ] ]
    )


viewSidebarTimeControls : Float -> Float -> ( String, List (Html Msg) )
viewSidebarTimeControls stepTime dtMulti =
    let
        timeButton : Float -> Html Msg
        timeButton n =
            Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.value (String.fromFloat n ++ "x")
                , Html.Attributes.classList [ ( "selected", n == dtMulti ) ]
                , Html.Events.onClick (SetDtMultiplier n)
                ]
                []
    in
    ( "Time"
    , [ SidebarView.buttonGroup [ timeButton 0, timeButton 0.5, timeButton 1 ]
      , SidebarView.viewLabeledNumberInput 0.01 stepTime "Step Time" SetStepTime
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
                , SidebarView.viewLabeledNumberInput 1 spring.length "Length" (\i -> SetSpring ( from, to ) (Spring.setLength i spring))
                , SidebarView.viewLabeledNumberInput 1 spring.rate "Rate" (\i -> SetSpring ( from, to ) (Spring.setRate i spring))
                , SidebarView.viewLabeledNumberInput 1 spring.damping "Damping" (\i -> SetSpring ( from, to ) (Spring.setDamping i spring))
                ]
    in
    ( "Springs (" ++ (springList |> List.length |> String.fromInt) ++ ")", List.map viewSpring springList )


viewSidebarWorld : ( String, World System ) -> Html Msg
viewSidebarWorld ( name, world ) =
    Html.input
        [ Html.Attributes.type_ "button"
        , Html.Attributes.value name
        , Html.Events.onClick (SetWorld world)
        ]
        []


viewParticleBounds : Boundary -> Svg msg
viewParticleBounds boundary =
    Svg.rect
        [ Svg.Attributes.width <| String.fromFloat boundary.width
        , Svg.Attributes.height <| String.fromFloat boundary.height
        , Svg.Attributes.x <| String.fromFloat (boundary.center.x - (boundary.width / 2))
        , Svg.Attributes.y <| String.fromFloat (boundary.center.y - (boundary.height / 2))
        , Svg.Attributes.class "bounds"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "black"
        ]
        []


viewSidebarSystem : Int -> ( Bool, System ) -> Html Msg
viewSidebarSystem index ( enabled, system ) =
    Html.div [ Html.Attributes.class "labeled-checkbox" ]
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked enabled
            , Html.Events.onClick (ToggleSystem index)
            , Html.Attributes.id <| "system-enabled" ++ String.fromInt index
            ]
            []
        , Html.label [ Html.Attributes.for <| "system-enabled" ++ String.fromInt index ] [ Html.text <| System.toString system ]
        ]


viewParticleVelocity : ( Int, Particle ) -> Svg msg
viewParticleVelocity ( _, particle ) =
    let
        scaledVelocity : Vector2
        scaledVelocity =
            particle
                |> Particle.velocity
                |> Vector2.scale 50
                |> Vector2.add particle.position
    in
    Svg.line
        [ Svg.Attributes.x1 <| String.fromFloat particle.position.x
        , Svg.Attributes.y1 <| String.fromFloat particle.position.y
        , Svg.Attributes.x2 <| String.fromFloat scaledVelocity.x
        , Svg.Attributes.y2 <| String.fromFloat scaledVelocity.y
        , Svg.Attributes.stroke "beige"
        ]
        []


viewSpringStress : Dict Int Particle -> ( ( Int, Int ), Spring ) -> Maybe (Svg Msg)
viewSpringStress particles ( ( from, to ), spring ) =
    let
        color : Float -> String
        color d =
            "hsl(" ++ String.fromFloat (300 + (min 50 <| d * 5)) ++ ", 50%, " ++ String.fromFloat (97 - (min 50 <| abs d * 10)) ++ "%)"
    in
    case ( Dict.get from particles, Dict.get to particles ) of
        ( Just p1, Just p2 ) ->
            Just <|
                Svg.line
                    [ Svg.Attributes.x1 <| String.fromFloat p1.position.x
                    , Svg.Attributes.y1 <| String.fromFloat p1.position.y
                    , Svg.Attributes.x2 <| String.fromFloat p2.position.x
                    , Svg.Attributes.y2 <| String.fromFloat p2.position.y
                    , Svg.Attributes.stroke <| color (Vector2.distance p1.position p2.position - spring.length)
                    , Svg.Attributes.strokeWidth "5"
                    , Svg.Attributes.strokeLinecap "round"
                    , Svg.Events.onClick <| ClickedConstraint ( from, to )
                    ]
                    []

        _ ->
            Nothing


runRenderSystem : Maybe Int -> Maybe Int -> World System -> System -> Maybe (Svg Msg)
runRenderSystem selected hovered world system =
    case system of
        RenderParticles ->
            Svg.Keyed.node "g" [] (Dict.toList world.particles |> List.map (viewKeyedParticle selected hovered)) |> Just

        RenderParticleVelocity ->
            Svg.g [] (Dict.toList world.particles |> List.map viewParticleVelocity) |> Just

        RenderSprings width ->
            Svg.g [] (Dict.toList world.springs |> List.filterMap (viewConstraint width world.particles)) |> Just

        RenderSpringStress ->
            Svg.g [] (Dict.toList world.springs |> List.filterMap (viewSpringStress world.particles)) |> Just

        ConstrainParticles b ->
            viewParticleBounds b |> Just

        Force _ ->
            Nothing

        Gravity _ ->
            Nothing

        BreakSprings ->
            Nothing

        SpawnParticle _ _ ->
            Nothing


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ SidebarView.viewSidebar
            [ ( "Particles (" ++ (Dict.toList model.world.particles |> List.length |> String.fromInt) ++ ")"
              , model.world.particles |> Dict.toList |> List.map (viewSidebarParticle model.selected model.hoverParticle)
              )
            , viewSidebarSprings model.world.springs
            , ( "Systems (" ++ (model.world.systems |> List.length |> String.fromInt) ++ ")"
              , model.world.systems |> List.indexedMap viewSidebarSystem
              )
            , ( "Camera", viewSidebarCamera model.renderConfig )
            , viewSidebarStats model
            , viewSidebarTimeControls model.world.stepTime model.world.dtMultiplier
            , ( "Worlds"
              , model.worlds |> Dict.toList |> List.map viewSidebarWorld
              )
            ]
        , Render.viewWorld (runRenderSystem model.selected model.hoverParticle) model.renderConfig model.world
        ]


viewSidebarCamera : RenderConfig -> List (Html Msg)
viewSidebarCamera config =
    [ SidebarView.viewLabeledNumberInput 0.1 config.cameraZoom "Zoom" SetCameraZoom
    , SidebarView.viewVector2Input config.cameraPosition SetCameraPosition
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
