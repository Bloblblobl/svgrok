module Main exposing (..)

import Browser
import Browser.Events as BrowserE
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Path exposing (Index2, Point, PointPair)
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias AnnotatedPath =
    { commands : List Path.Command
    , selected : Set Index2
    , hovered : Maybe Index2
    }


type alias Colors =
    { pointColor : String
    , strokeColor : String
    }


type alias OverlayConfig =
    { pointColor : String
    , strokeColor : String
    , hovered : Colors
    , selected : Colors
    }


type alias Config =
    { fillColor : String
    , strokeColor : String
    , strokeWidth : String
    , overlay : OverlayConfig
    , viewBox : String
    , editorHeight : String
    , editorWidth : String
    }


type alias Model =
    { config : Config
    , parseErrorString : String
    , path : AnnotatedPath
    , pathCommandsString : String
    , mouseOverOverlay : Bool
    , mouseOffset : Point
    , dragging : Maybe Index2
    }


initAnnotatedPath : List Path.Command -> AnnotatedPath
initAnnotatedPath commands =
    { commands = commands
    , selected = Set.empty
    , hovered = Nothing
    }


initOverlayConfig : OverlayConfig
initOverlayConfig =
    { pointColor = "red"
    , strokeColor = "red"
    , hovered =
        { pointColor = "blue"
        , strokeColor = "blue"
        }
    , selected =
        { pointColor = "green"
        , strokeColor = "green"
        }
    }


initConfig : Config
initConfig =
    { fillColor = "none"
    , strokeColor = "black"
    , strokeWidth = "1"
    , overlay = initOverlayConfig
    , viewBox = "0 0 100 100"
    , editorHeight = "400"
    , editorWidth = "400"
    }


initModel : Model
initModel =
    { config = initConfig
    , parseErrorString = ""
    , path = initAnnotatedPath []
    , pathCommandsString = ""
    , mouseOverOverlay = False
    , mouseOffset = { x = 0, y = 0 }
    , dragging = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- UPDATE


type ConfigChange
    = FillColor String
    | StrokeColor String
    | StrokeWidth String
    | ViewBox String


type Msg
    = PathStringChanged String
    | ConfigChanged ConfigChange
      -- Overlay messages
    | MouseOverOverlay
    | MouseOutOverlay
    | MouseMoveOverlay Point
    | MouseDownOverlay
    | MouseUpOverlay
    | HoverOverlay Index2
    | UnhoverOverlay
    | SelectOverlay Index2
    | UnselectOverlay Index2


updateConfig : ConfigChange -> Config -> Config
updateConfig configChange config =
    case configChange of
        FillColor newValue ->
            { config | fillColor = newValue }

        StrokeColor newValue ->
            { config | strokeColor = newValue }

        StrokeWidth newValue ->
            { config | strokeWidth = newValue }

        ViewBox newValue ->
            { config | viewBox = newValue }


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        PathStringChanged newPathString ->
            let
                ( commands, errorString ) =
                    Path.fromString newPathString
            in
            { model
                | path = initAnnotatedPath commands
                , pathCommandsString = newPathString
                , parseErrorString = errorString
            }

        ConfigChanged configChange ->
            { model | config = updateConfig configChange model.config }

        MouseOverOverlay ->
            { model | mouseOverOverlay = True }

        MouseOutOverlay ->
            { model | mouseOverOverlay = False }

        MouseMoveOverlay offset ->
            let
                path : AnnotatedPath
                path =
                    model.path

                transformedOffset : Point
                transformedOffset =
                    elementToViewBoxPoint model.config offset

                updatedCommands : List Path.Command
                updatedCommands =
                    case model.dragging of
                        Just index ->
                            Path.updateCommands
                                index
                                transformedOffset
                                model.path.commands

                        Nothing ->
                            model.path.commands

                updatedPathString : String
                updatedPathString =
                    Path.commandsToString updatedCommands
            in
            { model
                | mouseOffset = offset
                , path = { path | commands = updatedCommands }
                , pathCommandsString = updatedPathString
            }

        MouseDownOverlay ->
            { model | dragging = model.path.hovered }

        MouseUpOverlay ->
            { model | dragging = Nothing }

        HoverOverlay index ->
            let
                ( primary, _ ) =
                    index

                path : AnnotatedPath
                path =
                    model.path

                newModel : Model
                newModel =
                    if primary + 1 > List.length path.commands then
                        model

                    else
                        { model | path = { path | hovered = Just index } }
            in
            newModel

        UnhoverOverlay ->
            { model
                | path =
                    { commands = model.path.commands
                    , selected = model.path.selected
                    , hovered = Nothing
                    }
            }

        SelectOverlay index ->
            let
                ( primary, _ ) =
                    index

                path : AnnotatedPath
                path =
                    model.path

                newSelected : Set Index2
                newSelected =
                    Set.insert index path.selected

                newModel : Model
                newModel =
                    if primary + 1 > List.length path.commands then
                        model

                    else
                        { model | path = { path | selected = newSelected } }
            in
            newModel

        UnselectOverlay index ->
            { model
                | path =
                    { commands = model.path.commands
                    , selected = Set.remove index model.path.selected
                    , hovered = Just index
                    }
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )



-- SUBSCRIPTIONS


decodeMouseOffset : JsonD.Decoder Point
decodeMouseOffset =
    JsonD.map2 Point
        (JsonD.field "offsetX" JsonD.float)
        (JsonD.field "offsetY" JsonD.float)


subscriptions : Model -> Sub Msg
subscriptions { mouseOverOverlay, dragging } =
    if mouseOverOverlay then
        [ BrowserE.onMouseMove (JsonD.map MouseMoveOverlay decodeMouseOffset)
        , BrowserE.onMouseDown (JsonD.succeed MouseDownOverlay)
        , BrowserE.onMouseUp (JsonD.succeed MouseUpOverlay)
        ]
            |> Sub.batch

    else if dragging /= Nothing then
        BrowserE.onMouseUp (JsonD.succeed MouseUpOverlay)

    else
        Sub.none



-- VIEW


type alias OverlayBuilder =
    { pathSegments : List (Html Msg)
    , pathControls : List (Html Msg)
    , currentPoint : Point
    , index : Index2
    , config : Config
    , colorsResolver : Index2 -> Colors
    }


initOverlayBuilder : Config -> (Index2 -> Colors) -> OverlayBuilder
initOverlayBuilder config colorsResolver =
    { pathSegments = []
    , pathControls = []
    , currentPoint = Path.origin
    , index = ( 0, 0 )
    , config = config
    , colorsResolver = colorsResolver
    }


addToPrimaryIndex : Int -> Index2 -> Index2
addToPrimaryIndex amount ( primary, _ ) =
    ( primary + amount, 0 )


addToSecondaryIndex : Int -> Index2 -> Index2
addToSecondaryIndex amount ( primary, secondary ) =
    ( primary, secondary + amount )


indexToString : Index2 -> String
indexToString ( primary, secondary ) =
    String.join "," [ String.fromInt primary, String.fromInt secondary ]


preserveAspectRatio : Svg.Attribute msg
preserveAspectRatio =
    -- See preserveAspectRatio on MDN
    SvgA.preserveAspectRatio "xMinYMin slice"


elementToViewBoxPoint : Config -> Point -> Point
elementToViewBoxPoint config point =
    -- See https://www.w3.org/TR/SVG2/coords.html#ComputingAViewportsTransform
    let
        editorHeight : Float
        editorHeight =
            Maybe.withDefault 0 (String.toFloat config.editorHeight)

        editorWidth : Float
        editorWidth =
            Maybe.withDefault 0 (String.toFloat config.editorWidth)

        viewBox : Path.Rect
        viewBox =
            Path.viewBoxRectFromString config.viewBox

        scale : Float
        scale =
            max (viewBox.height / editorHeight) (viewBox.width / editorWidth)
    in
    { x = (point.x * scale) + viewBox.x, y = (point.y * scale) + viewBox.y }


viewPathConfig : Config -> Html Msg
viewPathConfig { fillColor, strokeColor, strokeWidth, viewBox } =
    Html.div []
        [ Html.input
            [ HtmlA.value fillColor
            , HtmlE.onInput (FillColor >> ConfigChanged)
            ]
            []
        , Html.input
            [ HtmlA.value strokeColor
            , HtmlE.onInput (StrokeColor >> ConfigChanged)
            ]
            []
        , Html.input
            [ HtmlA.value strokeWidth
            , HtmlE.onInput (StrokeWidth >> ConfigChanged)
            ]
            []
        , Html.input
            [ HtmlA.value viewBox
            , HtmlE.onInput (ViewBox >> ConfigChanged)
            ]
            []
        ]


viewPath : Model -> Html Msg
viewPath { config, pathCommandsString } =
    Svg.svg
        [ SvgA.height config.editorHeight
        , SvgA.width config.editorWidth
        , SvgA.fill config.fillColor
        , SvgA.stroke config.strokeColor
        , SvgA.strokeWidth config.strokeWidth
        , SvgA.viewBox config.viewBox
        , preserveAspectRatio
        ]
        [ Svg.path [ SvgA.d pathCommandsString ] [] ]


viewOverlaySegment : Config -> String -> Index2 -> String -> Html Msg
viewOverlaySegment config color index pathString =
    Svg.path
        [ SvgA.d pathString
        , SvgA.fill "none"
        , SvgA.stroke color
        , SvgA.strokeWidth config.strokeWidth
        , SvgE.onMouseOver (HoverOverlay index)
        , SvgE.onMouseOut UnhoverOverlay
        ]
        []


viewOverlayPoint : Config -> String -> Index2 -> Point -> Html Msg
viewOverlayPoint config color index point =
    Svg.circle
        [ SvgA.cx (String.fromFloat point.x)
        , SvgA.cy (String.fromFloat point.y)
        , SvgA.r config.strokeWidth
        , SvgA.fill color
        , SvgA.stroke "none"
        , SvgE.onMouseOver (HoverOverlay index)
        , SvgE.onMouseOut UnhoverOverlay
        ]
        []


viewMove : OverlayBuilder -> Point -> OverlayBuilder
viewMove overlayBuilder endPoint =
    let
        pointIndex : Index2
        pointIndex =
            addToSecondaryIndex 1 overlayBuilder.index

        pointColor : String
        pointColor =
            .pointColor (overlayBuilder.colorsResolver pointIndex)

        overlayPoint : Html Msg
        overlayPoint =
            viewOverlayPoint
                overlayBuilder.config
                pointColor
                pointIndex
                endPoint
    in
    { overlayBuilder
        | pathControls = overlayPoint :: overlayBuilder.pathControls
        , currentPoint = endPoint
        , index = addToPrimaryIndex 1 overlayBuilder.index
    }


viewLine : OverlayBuilder -> Point -> OverlayBuilder
viewLine overlayBuilder endPoint =
    let
        pointIndex : Index2
        pointIndex =
            addToSecondaryIndex 1 overlayBuilder.index

        strokeColor : String
        strokeColor =
            .strokeColor (overlayBuilder.colorsResolver overlayBuilder.index)

        pointColor : String
        pointColor =
            .pointColor (overlayBuilder.colorsResolver pointIndex)

        pathString : String
        pathString =
            Path.absoluteCommandOverlayString
                overlayBuilder.currentPoint
                (Path.AbsoluteLine endPoint)

        overlaySegment : Html Msg
        overlaySegment =
            viewOverlaySegment
                overlayBuilder.config
                strokeColor
                overlayBuilder.index
                pathString

        overlayPoint : Html Msg
        overlayPoint =
            viewOverlayPoint
                overlayBuilder.config
                pointColor
                pointIndex
                endPoint
    in
    { overlayBuilder
        | pathSegments = overlaySegment :: overlayBuilder.pathSegments
        , pathControls = overlayPoint :: overlayBuilder.pathControls
        , currentPoint = endPoint
        , index = addToPrimaryIndex 1 overlayBuilder.index
    }


viewCubicCurve : OverlayBuilder -> PointPair -> Point -> OverlayBuilder
viewCubicCurve overlayBuilder controls endPoint =
    let
        controlStartIndex : Index2
        controlStartIndex =
            addToSecondaryIndex 1 overlayBuilder.index

        controlEndIndex : Index2
        controlEndIndex =
            addToSecondaryIndex 2 overlayBuilder.index

        pointIndex : Index2
        pointIndex =
            addToSecondaryIndex 3 overlayBuilder.index

        strokeColor : String
        strokeColor =
            .strokeColor (overlayBuilder.colorsResolver overlayBuilder.index)

        controlStartColor : String
        controlStartColor =
            .pointColor (overlayBuilder.colorsResolver controlStartIndex)

        controlEndColor : String
        controlEndColor =
            .pointColor (overlayBuilder.colorsResolver controlEndIndex)

        pointColor : String
        pointColor =
            .pointColor (overlayBuilder.colorsResolver pointIndex)

        pathString : String
        pathString =
            Path.absoluteCommandOverlayString
                overlayBuilder.currentPoint
                (Path.AbsoluteCubicCurve controls endPoint)

        overlaySegment : Html Msg
        overlaySegment =
            viewOverlaySegment
                overlayBuilder.config
                strokeColor
                overlayBuilder.index
                pathString

        overlayControlStart : Html Msg
        overlayControlStart =
            viewOverlayPoint
                overlayBuilder.config
                controlStartColor
                controlStartIndex
                controls.start

        overlayControlEnd : Html Msg
        overlayControlEnd =
            viewOverlayPoint
                overlayBuilder.config
                controlEndColor
                controlEndIndex
                controls.end

        overlayPoint : Html Msg
        overlayPoint =
            viewOverlayPoint
                overlayBuilder.config
                pointColor
                pointIndex
                endPoint

        newPathControls =
            [ overlayPoint
            , overlayControlEnd
            , overlayControlStart
            ]
    in
    { overlayBuilder
        | pathSegments = overlaySegment :: overlayBuilder.pathSegments
        , pathControls = newPathControls ++ overlayBuilder.pathControls
        , currentPoint = endPoint
        , index = addToPrimaryIndex 1 overlayBuilder.index
    }


viewQuadraticCurve : OverlayBuilder -> Point -> Point -> OverlayBuilder
viewQuadraticCurve overlayBuilder control endPoint =
    let
        controlIndex : Index2
        controlIndex =
            addToSecondaryIndex 1 overlayBuilder.index

        pointIndex : Index2
        pointIndex =
            addToSecondaryIndex 2 overlayBuilder.index

        strokeColor : String
        strokeColor =
            .strokeColor (overlayBuilder.colorsResolver overlayBuilder.index)

        controlColor : String
        controlColor =
            .pointColor (overlayBuilder.colorsResolver controlIndex)

        pointColor : String
        pointColor =
            .pointColor (overlayBuilder.colorsResolver pointIndex)

        pathString : String
        pathString =
            Path.absoluteCommandOverlayString
                overlayBuilder.currentPoint
                (Path.AbsoluteQuadraticCurve control endPoint)

        overlaySegment : Html Msg
        overlaySegment =
            viewOverlaySegment
                overlayBuilder.config
                strokeColor
                overlayBuilder.index
                pathString

        overlayControl : Html Msg
        overlayControl =
            viewOverlayPoint
                overlayBuilder.config
                controlColor
                controlIndex
                control

        overlayPoint : Html Msg
        overlayPoint =
            viewOverlayPoint
                overlayBuilder.config
                pointColor
                pointIndex
                endPoint

        newPathControls =
            [ overlayPoint
            , overlayControl
            ]
    in
    { overlayBuilder
        | pathSegments = overlaySegment :: overlayBuilder.pathSegments
        , pathControls = newPathControls ++ overlayBuilder.pathControls
        , currentPoint = endPoint
        , index = addToPrimaryIndex 1 overlayBuilder.index
    }


viewArc : OverlayBuilder -> Path.ArcParameters -> Point -> OverlayBuilder
viewArc overlayBuilder params endPoint =
    -- TODO: Draw arc controls
    let
        pointIndex : Index2
        pointIndex =
            addToSecondaryIndex 1 overlayBuilder.index

        strokeColor : String
        strokeColor =
            .strokeColor (overlayBuilder.colorsResolver overlayBuilder.index)

        pointColor : String
        pointColor =
            .pointColor (overlayBuilder.colorsResolver pointIndex)

        pathString : String
        pathString =
            Path.absoluteCommandOverlayString
                overlayBuilder.currentPoint
                (Path.AbsoluteArc params endPoint)

        overlaySegment : Html Msg
        overlaySegment =
            viewOverlaySegment
                overlayBuilder.config
                strokeColor
                overlayBuilder.index
                pathString

        overlayPoint : Html Msg
        overlayPoint =
            viewOverlayPoint
                overlayBuilder.config
                pointColor
                pointIndex
                endPoint
    in
    { overlayBuilder
        | pathSegments = overlaySegment :: overlayBuilder.pathSegments
        , pathControls = overlayPoint :: overlayBuilder.pathControls
        , currentPoint = endPoint
        , index = addToPrimaryIndex 1 overlayBuilder.index
    }


viewOverlayStep : Path.AbsoluteCommand -> OverlayBuilder -> OverlayBuilder
viewOverlayStep absoluteCommand overlayBuilder =
    case absoluteCommand of
        Path.AbsoluteMove endPoint ->
            viewMove overlayBuilder endPoint

        Path.AbsoluteLine endPoint ->
            viewLine overlayBuilder endPoint

        Path.AbsoluteCubicCurve controls endPoint ->
            viewCubicCurve overlayBuilder controls endPoint

        Path.AbsoluteQuadraticCurve control endPoint ->
            viewQuadraticCurve overlayBuilder control endPoint

        Path.AbsoluteArc parameters endPoint ->
            viewArc overlayBuilder parameters endPoint


getColorsResolver : Config -> Set Index2 -> Maybe Index2 -> Index2 -> Colors
getColorsResolver config selected hovered index =
    let
        strokeColor : String
        strokeColor =
            if hovered == Just index then
                config.overlay.hovered.strokeColor

            else if Set.member index selected then
                config.overlay.selected.strokeColor

            else
                config.overlay.strokeColor

        pointColor : String
        pointColor =
            if hovered == Just index then
                config.overlay.hovered.pointColor

            else if Set.member index selected then
                config.overlay.selected.pointColor

            else
                config.overlay.pointColor
    in
    { strokeColor = strokeColor, pointColor = pointColor }


viewOverlay : Config -> AnnotatedPath -> Html Msg
viewOverlay config path =
    let
        absoluteCommands : List Path.AbsoluteCommand
        absoluteCommands =
            Path.resolveRawCommands path.commands

        colorsResolver : Index2 -> Colors
        colorsResolver =
            getColorsResolver config path.selected path.hovered

        overlayBuilder : OverlayBuilder
        overlayBuilder =
            initOverlayBuilder config colorsResolver

        builtOverlay : OverlayBuilder
        builtOverlay =
            List.foldl viewOverlayStep overlayBuilder absoluteCommands

        sortedOverlay : List (Html Msg)
        sortedOverlay =
            List.append
                (List.reverse builtOverlay.pathSegments)
                (List.reverse builtOverlay.pathControls)
    in
    Svg.svg
        [ SvgA.height config.editorHeight
        , SvgA.width config.editorWidth
        , SvgA.viewBox config.viewBox
        , SvgA.strokeWidth config.strokeWidth
        , SvgE.onMouseOver MouseOverOverlay
        , SvgE.onMouseOut MouseOutOverlay
        , preserveAspectRatio
        ]
        sortedOverlay


viewCommandStrings : List Path.Command -> Html Msg
viewCommandStrings commands =
    let
        commandListItems : List (Html Msg)
        commandListItems =
            List.map Path.commandToString commands
                |> List.map Html.text
                |> List.map List.singleton
                |> List.map (Html.li [])
    in
    Html.ul [] commandListItems


viewMouseOffset : Model -> Html Msg
viewMouseOffset { config, mouseOffset } =
    let
        transformedOffset : Point
        transformedOffset =
            elementToViewBoxPoint config mouseOffset
    in
    Html.text <|
        String.join ""
            [ "Mouse Offset: "
            , String.fromFloat transformedOffset.x
            , ","
            , String.fromFloat transformedOffset.y
            ]


viewHovered : Maybe Index2 -> Html Msg
viewHovered hovered =
    case hovered of
        Just index ->
            Html.text ("Hovered: " ++ indexToString index)

        Nothing ->
            Html.text "Nothing Hovered"


viewDragging : Maybe Index2 -> Html Msg
viewDragging dragging =
    case dragging of
        Just index ->
            Html.text ("Dragging: " ++ indexToString index)

        Nothing ->
            Html.text "Not dragging"


viewSelected : Set Index2 -> Html Msg
viewSelected selected =
    let
        selectedString : String
        selectedString =
            Set.toList selected
                |> List.map indexToString
                |> String.join ", "
    in
    if Set.isEmpty selected then
        Html.text "Nothing Selected"

    else
        Html.text ("Selected: " ++ selectedString)


viewInfo : Model -> Html Msg
viewInfo model =
    Html.ul []
        [ Html.li [] [ viewMouseOffset model ]
        , Html.li [] [ viewHovered model.path.hovered ]
        , Html.li [] [ viewDragging model.dragging ]
        , Html.li [] [ viewSelected model.path.selected ]
        , Html.li [] [ Html.text model.parseErrorString ]
        ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.input
            [ HtmlA.value model.pathCommandsString
            , HtmlE.onInput PathStringChanged
            ]
            []
        , viewPathConfig model.config
        , viewPath model
        , viewOverlay model.config model.path
        , viewInfo model
        , viewCommandStrings model.path.commands
        ]
