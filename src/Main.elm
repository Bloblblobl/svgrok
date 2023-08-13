module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events as BrowserE
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key exposing (Key(..))
import Path exposing (Path)
import Path.Parser
import Point exposing (Point)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Task
import Time
import ViewBox exposing (ViewBox)



-----------
-- MODEL --
-----------


type alias OverlayConfig =
    { default : List (Attribute Msg)
    , hovered : List (Attribute Msg)
    , selected : List (Attribute Msg)
    }


type DrawingCubicCurveState
    = DrawingCubicCurveTo
    | DrawingStartControl { to : Point }
    | DrawingEndControl { to : Point, startControl : Point }


type DrawingOnePointCurveState
    = DrawingOnePointCurveTo
    | DrawingControl { to : Point }


type DrawingArcState
    = DrawingArcTo
    | DrawingArcRadiusX { to : Point }
    | DrawingArcRadiusY { to : Point, radiusX : Float }
    | DrawingArcAngle { to : Point, radii : Point }
    | DrawingArcSize { to : Point, radii : Point, angle : Float }
    | DrawingArcRotation
        { to : Point
        , radii : Point
        , angle : Float
        , size : Path.ArcSize
        }


type DrawingState
    = DrawingMove
    | DrawingLine
    | DrawingHorizontalLine
    | DrawingVerticalLine
    | DrawingCubicCurve DrawingCubicCurveState
    | DrawingSmoothCubicCurve DrawingOnePointCurveState
    | DrawingQuadraticCurve DrawingOnePointCurveState
    | DrawingSmoothQuadraticCurve
    | DrawingArc DrawingArcState


type State
    = Neutral
    | Clicking
        { position : Point
        , temporarySelection : Path.Selection
        , canDrag : Bool
        }
    | Dragging { dragStart : Point, temporarySelection : Path.Selection }
    | Selecting Point
    | Drawing DrawingState


type alias SavedModel =
    { pathString : String
    , path : Path
    }


type alias Model =
    { pathString : String
    , path : Path
    , overlayConfig : OverlayConfig
    , viewBox : ViewBox
    , mouseOffset : Point
    , state : State
    , activeKeys : List Key
    , zoomFactor : Float

    -- special flag for when the meta key is pressed,
    -- as it messes with key event handling
    , metaPressed : Bool
    , undoStack : List SavedModel
    , redoStack : List SavedModel
    }



----------
-- INIT --
----------


initPath : Path
initPath =
    { components = []
    , hovered = Nothing
    , selected = []
    }


initOverlayConfig : OverlayConfig
initOverlayConfig =
    { default =
        [ SvgA.stroke "black"
        , SvgA.fill "none"
        , SvgA.cursor "pointer"
        ]
    , hovered =
        [ SvgA.stroke "yellow"
        , SvgA.fill "none"
        , SvgA.cursor "pointer"
        ]
    , selected =
        [ SvgA.stroke "blue"
        , SvgA.fill "none"
        , SvgA.cursor "grab"
        ]
    }


draggingOverlayConfig : OverlayConfig
draggingOverlayConfig =
    { default =
        [ SvgA.stroke "black"
        , SvgA.fill "none"
        , SvgA.cursor "pointer"
        , SvgA.opacity "0.5"
        ]
    , hovered =
        [ SvgA.stroke "black"
        , SvgA.fill "none"
        , SvgA.cursor "pointer"
        , SvgA.opacity "0.5"
        ]
    , selected =
        [ SvgA.stroke "black"
        , SvgA.fill "none"
        , SvgA.cursor "grab"
        , SvgA.opacity "0.5"
        ]
    }


initModel : Model
initModel =
    { pathString = ""
    , path = Path.init
    , overlayConfig = initOverlayConfig
    , viewBox = ViewBox.init
    , mouseOffset = Point.zero
    , state = Neutral
    , activeKeys = []
    , zoomFactor = 1
    , metaPressed = False
    , undoStack = []
    , redoStack = []
    }


getInitialViewBoxFromViewport : Float -> Cmd Msg
getInitialViewBoxFromViewport zoomFactor =
    Task.perform
        (SetViewBox << ViewBox.zoom zoomFactor << ViewBox.fromViewport)
        Browser.Dom.getViewport


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, getInitialViewBoxFromViewport initModel.zoomFactor )



------------
-- UPDATE --
------------


type Msg
    = SetViewBox ViewBox
    | PathStringChanged String
    | WindowResized Int Int
    | SetHoveredElement (Maybe Path.Selection)
    | MouseMove Point
    | MouseDownElement Path.Selection
    | MouseDownCanvas
    | MouseUp
    | SetCanDrag
    | KeyDown KeyboardEvent
    | KeyUp KeyboardEvent
    | Undo
    | Redo


trackedKey : Key -> Bool
trackedKey key =
    case key of
        Shift _ ->
            True

        _ ->
            False


setActiveKey : Key -> Model -> Model
setActiveKey key model =
    if List.member key model.activeKeys || not (trackedKey key) then
        model

    else
        { model | activeKeys = key :: model.activeKeys }


unsetActiveKey : Key -> Model -> Model
unsetActiveKey key model =
    { model | activeKeys = List.filter ((/=) key) model.activeKeys }


shiftPressed : Model -> Bool
shiftPressed model =
    List.any
        (\key ->
            case key of
                Shift _ ->
                    True

                _ ->
                    False
        )
        model.activeKeys


handleKeyDown : KeyboardEvent -> Model -> Model
handleKeyDown { keyCode } model =
    case keyCode of
        Ambiguous _ ->
            { model | metaPressed = True }

        U ->
            undo model

        R ->
            redo model

        -- ZOOM
        E ->
            let
                newZoomFactor : Float
                newZoomFactor =
                    model.zoomFactor * 0.5
            in
            { model
                | viewBox = ViewBox.zoom newZoomFactor model.viewBox
                , zoomFactor = newZoomFactor
            }

        W ->
            let
                newZoomFactor : Float
                newZoomFactor =
                    model.zoomFactor * 2
            in
            { model
                | viewBox = ViewBox.zoom newZoomFactor model.viewBox
                , zoomFactor = newZoomFactor
            }

        -- PAN
        Left ->
            { model | viewBox = ViewBox.pan -5 0 model.viewBox }

        Up ->
            { model | viewBox = ViewBox.pan 0 -5 model.viewBox }

        Down ->
            { model | viewBox = ViewBox.pan 0 5 model.viewBox }

        Right ->
            { model | viewBox = ViewBox.pan 5 0 model.viewBox }

        -- DRAWING
        M ->
            { model | state = Drawing DrawingMove }

        L ->
            { model | state = Drawing DrawingLine }

        H ->
            { model | state = Drawing DrawingHorizontalLine }

        V ->
            { model | state = Drawing DrawingVerticalLine }

        C ->
            { model | state = Drawing (DrawingCubicCurve DrawingCubicCurveTo) }

        S ->
            { model
                | state =
                    Drawing (DrawingSmoothCubicCurve DrawingOnePointCurveTo)
            }

        Q ->
            { model
                | state =
                    Drawing (DrawingQuadraticCurve DrawingOnePointCurveTo)
            }

        T ->
            { model | state = Drawing DrawingSmoothQuadraticCurve }

        X ->
            case model.state of
                Drawing _ ->
                    { model | state = Neutral }

                _ ->
                    model

        _ ->
            if not model.metaPressed then
                setActiveKey keyCode model

            else
                model


handleDraw : Model -> DrawingState -> Model
handleDraw model drawingState =
    case drawingState of
        DrawingMove ->
            let
                moveCommand : Path.Command
                moveCommand =
                    Path.preFormattedMove { to = model.mouseOffset }

                newPath : Path
                newPath =
                    Path.appendCommand model.path moveCommand
            in
            { model
                | path = newPath
                , pathString = Path.toString newPath
            }

        DrawingLine ->
            let
                lineCommand : Path.Command
                lineCommand =
                    Path.preFormattedLine { to = model.mouseOffset }

                newPath : Path
                newPath =
                    Path.appendCommand model.path lineCommand
            in
            { model
                | path = newPath
                , pathString = Path.toString newPath
            }

        DrawingHorizontalLine ->
            let
                horizontalLineCommand : Path.Command
                horizontalLineCommand =
                    Path.preFormattedHorizontalLine
                        { toX = model.mouseOffset.x }

                newPath : Path
                newPath =
                    Path.appendCommand model.path horizontalLineCommand
            in
            { model
                | path = newPath
                , pathString = Path.toString newPath
            }

        DrawingVerticalLine ->
            let
                verticalLineCommand : Path.Command
                verticalLineCommand =
                    Path.preFormattedVerticalLine
                        { toY = model.mouseOffset.y }

                newPath : Path
                newPath =
                    Path.appendCommand model.path verticalLineCommand
            in
            { model
                | path = newPath
                , pathString = Path.toString newPath
            }

        DrawingCubicCurve drawingCubicCurveState ->
            case drawingCubicCurveState of
                DrawingCubicCurveTo ->
                    { model
                        | state =
                            Drawing
                                (DrawingCubicCurve
                                    (DrawingStartControl
                                        { to = model.mouseOffset }
                                    )
                                )
                    }

                DrawingStartControl { to } ->
                    { model
                        | state =
                            Drawing
                                (DrawingCubicCurve
                                    (DrawingEndControl
                                        { to = to
                                        , startControl = model.mouseOffset
                                        }
                                    )
                                )
                    }

                DrawingEndControl { to, startControl } ->
                    let
                        cubicCurveCommand : Path.Command
                        cubicCurveCommand =
                            Path.preFormattedCubicCurve
                                { to = to
                                , startControl = startControl
                                , endControl = model.mouseOffset
                                }

                        newPath : Path
                        newPath =
                            Path.appendCommand model.path cubicCurveCommand

                        newState : State
                        newState =
                            Drawing (DrawingCubicCurve DrawingCubicCurveTo)
                    in
                    { model
                        | path = newPath
                        , pathString = Path.toString newPath
                        , state = newState
                    }

        DrawingSmoothCubicCurve drawingOnePointCurveState ->
            case drawingOnePointCurveState of
                DrawingOnePointCurveTo ->
                    { model
                        | state =
                            Drawing
                                (DrawingSmoothCubicCurve
                                    (DrawingControl
                                        { to = model.mouseOffset }
                                    )
                                )
                    }

                DrawingControl { to } ->
                    let
                        smoothCubicCurveCommand : Path.Command
                        smoothCubicCurveCommand =
                            Path.preFormattedSmoothCubicCurve
                                { to = to
                                , endControl = model.mouseOffset
                                }

                        newPath : Path
                        newPath =
                            Path.appendCommand
                                model.path
                                smoothCubicCurveCommand

                        newState : State
                        newState =
                            Drawing
                                (DrawingSmoothCubicCurve DrawingOnePointCurveTo)
                    in
                    { model
                        | path = newPath
                        , pathString = Path.toString newPath
                        , state = newState
                    }

        DrawingQuadraticCurve drawingOnePointCurveState ->
            case drawingOnePointCurveState of
                DrawingOnePointCurveTo ->
                    { model
                        | state =
                            Drawing
                                (DrawingQuadraticCurve
                                    (DrawingControl
                                        { to = model.mouseOffset }
                                    )
                                )
                    }

                DrawingControl { to } ->
                    let
                        quadraticCurveCommand : Path.Command
                        quadraticCurveCommand =
                            Path.preFormattedQuadraticCurve
                                { to = to
                                , control = model.mouseOffset
                                }

                        newPath : Path
                        newPath =
                            Path.appendCommand
                                model.path
                                quadraticCurveCommand

                        newState : State
                        newState =
                            Drawing
                                (DrawingQuadraticCurve DrawingOnePointCurveTo)
                    in
                    { model
                        | path = newPath
                        , pathString = Path.toString newPath
                        , state = newState
                    }

        DrawingSmoothQuadraticCurve ->
            let
                smoothQuadraticCurveCommand : Path.Command
                smoothQuadraticCurveCommand =
                    Path.preFormattedSmoothQuadraticCurve
                        { to = model.mouseOffset }

                newPath : Path
                newPath =
                    Path.appendCommand model.path smoothQuadraticCurveCommand
            in
            { model
                | path = newPath
                , pathString = Path.toString newPath
            }

        _ ->
            model


shouldSave : SavedModel -> SavedModel -> Bool
shouldSave oldSavedModel newSavedModel =
    let
        oldSegments : List Path.Segment
        oldSegments =
            List.map .segment oldSavedModel.path.components

        newSegments : List Path.Segment
        newSegments =
            List.map .segment newSavedModel.path.components
    in
    oldSegments /= newSegments


savePath : Path -> Path
savePath path =
    { path
        | hovered = Nothing
        , selected = []
    }


saveModel : Model -> Model -> Model
saveModel oldModel newModel =
    let
        oldSavedModel : SavedModel
        oldSavedModel =
            { pathString = oldModel.pathString
            , path = savePath oldModel.path
            }

        newSavedModel : SavedModel
        newSavedModel =
            { pathString = newModel.pathString
            , path = savePath newModel.path
            }
    in
    case List.head newModel.undoStack of
        Just _ ->
            if shouldSave oldSavedModel newSavedModel then
                { newModel
                    | undoStack = oldSavedModel :: newModel.undoStack
                    , redoStack = []
                }

            else
                newModel

        Nothing ->
            if shouldSave oldSavedModel newSavedModel then
                { newModel
                    | undoStack = [ oldSavedModel ]
                    , redoStack = []
                }

            else
                newModel


undo : Model -> Model
undo model =
    let
        currentSavedModel : SavedModel
        currentSavedModel =
            { pathString = model.pathString
            , path = savePath model.path
            }
    in
    case List.head model.undoStack of
        Just savedModel ->
            { model
                | pathString = savedModel.pathString
                , path =
                    { components = savedModel.path.components
                    , hovered = Nothing
                    , selected = savedModel.path.selected
                    }
                , undoStack = List.drop 1 model.undoStack
                , redoStack = currentSavedModel :: model.redoStack
            }

        Nothing ->
            model


redo : Model -> Model
redo model =
    let
        currentSavedModel : SavedModel
        currentSavedModel =
            { pathString = model.pathString
            , path = savePath model.path
            }
    in
    case List.head model.redoStack of
        Just savedModel ->
            { model
                | pathString = savedModel.pathString
                , path =
                    { components = savedModel.path.components
                    , hovered = Nothing
                    , selected = savedModel.path.selected
                    }
                , undoStack = currentSavedModel :: model.undoStack
                , redoStack = List.drop 1 model.redoStack
            }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        save : Model -> Model
        save =
            saveModel model
    in
    case msg of
        SetViewBox newViewBox ->
            ( { model | viewBox = newViewBox }, Cmd.none )

        PathStringChanged newPathString ->
            ( save
                { model
                    | pathString = newPathString
                    , path = Path.Parser.parse newPathString
                }
            , Cmd.none
            )

        WindowResized newWidth newHeight ->
            let
                newViewBox : ViewBox
                newViewBox =
                    { minX = model.viewBox.minX
                    , minY = model.viewBox.minY
                    , width = toFloat newWidth
                    , height = toFloat newHeight
                    , actualWidth = toFloat newWidth
                    , actualHeight = toFloat newHeight
                    }
            in
            ( { model | viewBox = ViewBox.zoom model.zoomFactor newViewBox }
            , Cmd.none
            )

        SetHoveredElement selection ->
            let
                { path } =
                    model
            in
            ( { model | path = { path | hovered = selection } }
            , Cmd.none
            )

        MouseMove newOffset ->
            let
                newState : State
                newState =
                    case model.state of
                        Neutral ->
                            model.state

                        Clicking { position, temporarySelection, canDrag } ->
                            if canDrag then
                                Dragging
                                    { dragStart = position
                                    , temporarySelection = temporarySelection
                                    }

                            else
                                model.state

                        Dragging _ ->
                            model.state

                        Selecting _ ->
                            model.state

                        Drawing _ ->
                            model.state
            in
            ( { model
                | mouseOffset =
                    Point.add
                        (ViewBox.scalePoint model.viewBox newOffset)
                        { x = model.viewBox.minX, y = model.viewBox.minY }
                , state = newState
              }
            , Cmd.none
            )

        MouseDownElement selection ->
            case model.state of
                Neutral ->
                    ( { model
                        | state =
                            Clicking
                                { position = model.mouseOffset
                                , temporarySelection = selection
                                , canDrag = False
                                }
                      }
                    , Cmd.none
                    )

                Clicking _ ->
                    ( model, Cmd.none )

                Dragging _ ->
                    ( model, Cmd.none )

                Selecting _ ->
                    ( model, Cmd.none )

                Drawing _ ->
                    ( model, Cmd.none )

        MouseDownCanvas ->
            case model.state of
                Neutral ->
                    if shiftPressed model then
                        ( { model | state = Selecting model.mouseOffset }
                        , Cmd.none
                        )

                    else
                        ( save
                            { model
                                | path =
                                    { components = model.path.components
                                    , hovered = model.path.hovered
                                    , selected = []
                                    }
                            }
                        , Cmd.none
                        )

                Clicking _ ->
                    ( model, Cmd.none )

                Dragging _ ->
                    ( model, Cmd.none )

                Selecting _ ->
                    ( model, Cmd.none )

                Drawing _ ->
                    ( model, Cmd.none )

        MouseUp ->
            case model.state of
                Neutral ->
                    ( model, Cmd.none )

                Clicking { temporarySelection } ->
                    let
                        newPath : Path
                        newPath =
                            if shiftPressed model then
                                Path.toggleSelection
                                    model.path
                                    temporarySelection

                            else
                                { components = model.path.components
                                , hovered = model.path.hovered
                                , selected =
                                    if
                                        List.member temporarySelection
                                            model.path.selected
                                    then
                                        []

                                    else
                                        [ temporarySelection ]
                                }
                    in
                    ( save
                        { model | path = newPath, state = Neutral }
                    , Cmd.none
                    )

                Dragging { dragStart, temporarySelection } ->
                    let
                        dragOffset : Point
                        dragOffset =
                            Point.subtract model.mouseOffset dragStart

                        alreadySelected : Bool
                        alreadySelected =
                            List.member temporarySelection model.path.selected

                        newPath : Path
                        newPath =
                            if alreadySelected then
                                Path.update model.path dragOffset

                            else
                                Path.updateWithSelection
                                    model.path
                                    dragOffset
                                    temporarySelection

                        newPathString : String
                        newPathString =
                            Path.toString newPath
                    in
                    ( save
                        { model
                            | path = newPath
                            , pathString = newPathString
                            , state = Neutral
                        }
                    , Cmd.none
                    )

                Selecting selectStart ->
                    let
                        viewBoxOffset : Point
                        viewBoxOffset =
                            { x = model.viewBox.minX, y = model.viewBox.minY }

                        actualSelectStart : Point
                        actualSelectStart =
                            Point.add selectStart viewBoxOffset

                        actualMouseOffset : Point
                        actualMouseOffset =
                            Point.add model.mouseOffset viewBoxOffset
                    in
                    ( save
                        { model
                            | path =
                                { components = model.path.components
                                , hovered = model.path.hovered
                                , selected =
                                    Path.selectionsWithin
                                        actualSelectStart
                                        actualMouseOffset
                                        model.path
                                }
                            , state = Neutral
                        }
                    , Cmd.none
                    )

                Drawing drawingState ->
                    ( save (handleDraw model drawingState), Cmd.none )

        SetCanDrag ->
            case model.state of
                Neutral ->
                    ( model, Cmd.none )

                Clicking clickingParams ->
                    ( { model
                        | state =
                            Clicking
                                { clickingParams | canDrag = True }
                      }
                    , Cmd.none
                    )

                Dragging _ ->
                    ( model, Cmd.none )

                Selecting _ ->
                    ( model, Cmd.none )

                Drawing _ ->
                    ( model, Cmd.none )

        KeyDown keyboardEvent ->
            ( handleKeyDown keyboardEvent model, Cmd.none )

        KeyUp { keyCode } ->
            case keyCode of
                Ambiguous _ ->
                    ( { model | metaPressed = False }, Cmd.none )

                _ ->
                    ( unsetActiveKey keyCode model, Cmd.none )

        Undo ->
            ( undo model, Cmd.none )

        Redo ->
            ( redo model, Cmd.none )



-------------------
-- SUBSCRIPTIONS --
-------------------


decodeMouseOffset : JsonD.Decoder Point
decodeMouseOffset =
    JsonD.map2 Point
        (JsonD.field "clientX" JsonD.float)
        (JsonD.field "clientY" JsonD.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        baseSubscriptions : List (Sub Msg)
        baseSubscriptions =
            [ BrowserE.onMouseMove (JsonD.map MouseMove decodeMouseOffset)
            , BrowserE.onMouseUp (JsonD.succeed MouseUp)
            , BrowserE.onKeyDown (JsonD.map KeyDown decodeKeyboardEvent)
            , BrowserE.onKeyUp (JsonD.map KeyUp decodeKeyboardEvent)
            , BrowserE.onResize WindowResized
            ]
    in
    case model.state of
        Neutral ->
            baseSubscriptions
                |> Sub.batch

        Clicking { canDrag } ->
            if canDrag then
                baseSubscriptions
                    |> Sub.batch

            else
                Time.every 100 (\_ -> SetCanDrag)
                    :: baseSubscriptions
                    |> Sub.batch

        Dragging _ ->
            baseSubscriptions
                |> Sub.batch

        Selecting _ ->
            baseSubscriptions
                |> Sub.batch

        Drawing _ ->
            baseSubscriptions
                |> Sub.batch



----------
-- VIEW --
----------


type alias OverlayBuilder =
    { config : OverlayConfig
    , hovered : Maybe Path.Selection
    , selected : List Path.Selection
    , points : List (Svg Msg)
    , segments : List (Svg Msg)
    }


initOverlayBuilder : OverlayConfig -> Path -> OverlayBuilder
initOverlayBuilder config { hovered, selected } =
    { config = config
    , hovered = hovered
    , selected = selected
    , points = []
    , segments = []
    }


{-| Returns a list of mouse event Attributes for a Selection
-}
selectionMouseEvents : Path.Selection -> List (Attribute Msg)
selectionMouseEvents selection =
    [ SvgE.onMouseOver (SetHoveredElement <| Just selection)
    , SvgE.onMouseOut (SetHoveredElement Nothing)
    , SvgE.onMouseDown (MouseDownElement selection)
    ]


{-| Returns a list of Attributes for a Selection from an OverlayBuilder.
-}
selectionAttributes : OverlayBuilder -> Path.Selection -> List (Attribute Msg)
selectionAttributes { config, hovered, selected } selection =
    let
        isHovered : Bool
        isHovered =
            case hovered of
                Just hoveredSelection ->
                    selection == hoveredSelection

                Nothing ->
                    False

        isSelected : Bool
        isSelected =
            List.member selection selected
    in
    List.append (selectionMouseEvents selection) <|
        if isSelected then
            config.selected

        else if isHovered then
            config.hovered

        else
            config.default


{-| Renders a Path String as a single SVG <path> element.
-}
viewPath : List (Attribute Msg) -> String -> Svg Msg
viewPath attributes pathString =
    Svg.path (SvgA.d pathString :: attributes) []


{-| Renders a Point as an SVG <circle> element.
-}
viewPoint : List (Attribute Msg) -> Point -> Svg Msg
viewPoint attributes { x, y } =
    let
        pointAttributes : List (Attribute Msg)
        pointAttributes =
            [ SvgA.cx (String.fromFloat x)
            , SvgA.cy (String.fromFloat y)
            , SvgA.r "0.5"
            ]
    in
    Svg.circle (pointAttributes ++ attributes) []


{-| Builds a Segment onto an OverlayBuilder by adding all of the relevant Points
on the Segment as well as the Segment itself to the builder.
-}
buildSegment : ( Int, Path.Component ) -> OverlayBuilder -> OverlayBuilder
buildSegment ( index, component ) builder =
    case component.segment of
        Path.MoveSegment { to } ->
            let
                selection : Path.Selection
                selection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    viewPoint (selectionAttributes builder selection) to
            in
            { builder | points = endPoint :: builder.points }

        Path.LineSegment { to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                lineSegment : Svg Msg
                lineSegment =
                    viewPath
                        (selectionAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    viewPoint (selectionAttributes builder endSelection) to
            in
            { builder
                | points = endPoint :: builder.points
                , segments = lineSegment :: builder.segments
            }

        Path.CubicCurveSegment { startControl, endControl, to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                curveSegment : Svg Msg
                curveSegment =
                    viewPath
                        (selectionAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                startSelection : Path.Selection
                startSelection =
                    { index = index, element = Path.StartControl }

                startControlPoint : Svg Msg
                startControlPoint =
                    viewPoint (selectionAttributes builder startSelection) startControl

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndControl }

                endControlPoint : Svg Msg
                endControlPoint =
                    viewPoint (selectionAttributes builder endSelection) endControl

                endPointSelection : Path.Selection
                endPointSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    viewPoint (selectionAttributes builder endPointSelection) to
            in
            { builder
                | points =
                    List.append
                        [ startControlPoint, endControlPoint, endPoint ]
                        builder.points
                , segments = curveSegment :: builder.segments
            }

        Path.QuadraticCurveSegment { control, to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                curveSegment : Svg Msg
                curveSegment =
                    viewPath
                        (selectionAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                controlSelection : Path.Selection
                controlSelection =
                    { index = index, element = Path.Control }

                controlPoint : Svg Msg
                controlPoint =
                    viewPoint (selectionAttributes builder controlSelection) control

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    viewPoint (selectionAttributes builder endSelection) to
            in
            { builder
                | points = controlPoint :: endPoint :: builder.points
                , segments = curveSegment :: builder.segments
            }

        Path.ArcSegment { to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                arcSegment : Svg Msg
                arcSegment =
                    viewPath
                        (selectionAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    viewPoint (selectionAttributes builder endSelection) to
            in
            { builder
                | points = endPoint :: builder.points
                , segments = arcSegment :: builder.segments
            }

        Path.CloseSegment _ ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                closeSegment : Svg Msg
                closeSegment =
                    viewPath
                        (selectionAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)
            in
            { builder | segments = closeSegment :: builder.segments }


{-| Renders an overlay of a Path, with separate SVG elements for each Point and
Segment of the Path.
-}
viewOverlay : OverlayConfig -> Path -> List (Svg Msg)
viewOverlay config path =
    let
        initialBuilder : OverlayBuilder
        initialBuilder =
            initOverlayBuilder config path

        indexedComponents : List ( Int, Path.Component )
        indexedComponents =
            List.indexedMap Tuple.pair path.components
    in
    List.foldl buildSegment initialBuilder indexedComponents
        |> (\builder -> builder.segments ++ builder.points)


viewSelectedPoints : Path -> List (Svg Msg)
viewSelectedPoints path =
    let
        indexedComponents : List ( Int, Path.Component )
        indexedComponents =
            List.indexedMap Tuple.pair path.components

        makeSelection : Int -> Path.Element -> Path.Selection
        makeSelection index element =
            { index = index, element = element }

        possibleSelections : Int -> List Path.Selection
        possibleSelections index =
            List.map
                (makeSelection index)
                [ Path.EndPoint
                , Path.StartControl
                , Path.EndControl
                , Path.Control
                ]

        isSelected : Int -> Bool
        isSelected index =
            List.any
                (\selection -> List.member selection path.selected)
                (possibleSelections index)

        selectedComponents : List Path.Component
        selectedComponents =
            List.filterMap
                (\( index, component ) ->
                    if isSelected index then
                        Just component

                    else
                        Nothing
                )
                indexedComponents

        selectedPoints : List Point
        selectedPoints =
            List.concatMap Path.componentPoints selectedComponents

        viewSelectedPoint : Point -> Svg Msg
        viewSelectedPoint =
            viewPoint [ SvgA.fill "black", SvgA.stroke "none", SvgA.r "1" ]
    in
    List.map viewSelectedPoint selectedPoints


viewDraggingPreview : Model -> Point -> Path.Selection -> Svg Msg
viewDraggingPreview model dragStart temporarySelection =
    let
        previewPath : Path
        previewPath =
            Path.update
                (Path.addSelection model.path temporarySelection)
                (Point.subtract model.mouseOffset dragStart)

        viewPreviewPath : Svg Msg
        viewPreviewPath =
            viewPath [] (Path.toString previewPath)
    in
    Svg.g
        [ SvgA.fill "none"
        , SvgA.stroke "black"
        , SvgA.strokeWidth "0.5"
        , SvgA.strokeDasharray "0.5 0.5"
        , SvgA.opacity "0.5"
        , SvgA.cursor "grab"
        ]
        (viewPreviewPath :: viewSelectedPoints previewPath)


viewSelectionBox : Model -> Point -> Svg Msg
viewSelectionBox model selectionStart =
    let
        minX : Float
        minX =
            min selectionStart.x model.mouseOffset.x

        width : Float
        width =
            max selectionStart.x model.mouseOffset.x - minX

        minY : Float
        minY =
            min selectionStart.y model.mouseOffset.y

        height : Float
        height =
            max selectionStart.y model.mouseOffset.y - minY
    in
    Svg.rect
        [ SvgA.fill "black"
        , SvgA.stroke "black"
        , SvgA.strokeWidth "0.5"
        , SvgA.strokeDasharray "0.5 0.5"
        , SvgA.fillOpacity "0.05"
        , SvgA.strokeOpacity "0.5"
        , SvgA.x (String.fromFloat <| minX + model.viewBox.minX)
        , SvgA.y (String.fromFloat <| minY + model.viewBox.minY)
        , SvgA.width (String.fromFloat width)
        , SvgA.height (String.fromFloat height)
        ]
        []


viewDrawingPreview : Model -> DrawingState -> Svg Msg
viewDrawingPreview model drawingState =
    let
        endState : Path.EndState
        endState =
            Path.toEndState model.path

        pathEndPointString : String
        pathEndPointString =
            Path.commandToString
                (Path.preFormattedMove { to = endState.endPoint })

        previewPoint : Point -> Svg Msg
        previewPoint point =
            viewPoint
                [ SvgA.fill "black", SvgA.stroke "none", SvgA.r "1" ]
                point

        previewAttributes : List (Svg.Attribute Msg)
        previewAttributes =
            [ SvgA.fill "none"
            , SvgA.stroke "black"
            , SvgA.strokeWidth "0.5"
            , SvgA.opacity "0.5"
            ]
    in
    case drawingState of
        DrawingMove ->
            let
                previewMoveString : String
                previewMoveString =
                    Path.commandToString
                        (Path.preFormattedLine { to = model.mouseOffset })
            in
            Svg.g previewAttributes
                [ viewPath
                    [ SvgA.strokeDasharray "2 2" ]
                    (pathEndPointString ++ previewMoveString)
                , previewPoint model.mouseOffset
                ]

        DrawingLine ->
            let
                previewLineString : String
                previewLineString =
                    Path.commandToString
                        (Path.preFormattedLine { to = model.mouseOffset })
            in
            Svg.g previewAttributes
                [ viewPath [] (pathEndPointString ++ previewLineString)
                , previewPoint model.mouseOffset
                ]

        DrawingHorizontalLine ->
            let
                previewHorizontalLineString : String
                previewHorizontalLineString =
                    Path.commandToString
                        (Path.preFormattedHorizontalLine
                            { toX = model.mouseOffset.x }
                        )
            in
            Svg.g previewAttributes
                [ viewPath
                    []
                    (pathEndPointString ++ previewHorizontalLineString)
                , previewPoint
                    { x = model.mouseOffset.x, y = endState.endPoint.y }
                ]

        DrawingVerticalLine ->
            let
                previewVerticalLineString : String
                previewVerticalLineString =
                    Path.commandToString
                        (Path.preFormattedVerticalLine
                            { toY = model.mouseOffset.y }
                        )
            in
            Svg.g previewAttributes
                [ viewPath [] (pathEndPointString ++ previewVerticalLineString)
                , previewPoint
                    { x = endState.endPoint.x, y = model.mouseOffset.y }
                ]

        DrawingCubicCurve drawingCubicCurveState ->
            case drawingCubicCurveState of
                DrawingCubicCurveTo ->
                    let
                        previewCubicCurveString : String
                        previewCubicCurveString =
                            Path.commandToString
                                (Path.preFormattedCubicCurve
                                    { to = model.mouseOffset
                                    , startControl = model.mouseOffset
                                    , endControl = model.mouseOffset
                                    }
                                )
                    in
                    Svg.g previewAttributes
                        [ viewPath
                            []
                            (pathEndPointString ++ previewCubicCurveString)
                        , previewPoint model.mouseOffset
                        ]

                DrawingStartControl { to } ->
                    let
                        previewCubicCurveString : String
                        previewCubicCurveString =
                            Path.commandToString
                                (Path.preFormattedCubicCurve
                                    { to = to
                                    , startControl = model.mouseOffset
                                    , endControl = to
                                    }
                                )

                        previewStartControlString : String
                        previewStartControlString =
                            Path.commandToString
                                (Path.preFormattedLine
                                    { to = model.mouseOffset }
                                )
                    in
                    Svg.g previewAttributes
                        [ viewPath
                            []
                            (pathEndPointString ++ previewCubicCurveString)
                        , viewPath
                            []
                            (pathEndPointString ++ previewStartControlString)
                        , previewPoint to
                        , previewPoint model.mouseOffset
                        ]

                DrawingEndControl { to, startControl } ->
                    let
                        previewCubicCurveString : String
                        previewCubicCurveString =
                            Path.commandToString
                                (Path.preFormattedCubicCurve
                                    { to = to
                                    , startControl = startControl
                                    , endControl = model.mouseOffset
                                    }
                                )

                        previewEndPointString : String
                        previewEndPointString =
                            Path.commandToString
                                (Path.preFormattedMove { to = to })

                        previewStartControlString : String
                        previewStartControlString =
                            Path.commandToString
                                (Path.preFormattedLine
                                    { to = startControl }
                                )

                        previewEndControlString : String
                        previewEndControlString =
                            Path.commandToString
                                (Path.preFormattedLine
                                    { to = model.mouseOffset }
                                )
                    in
                    Svg.g previewAttributes
                        [ viewPath
                            []
                            (pathEndPointString ++ previewCubicCurveString)
                        , viewPath
                            []
                            (pathEndPointString ++ previewStartControlString)
                        , viewPath
                            []
                            (previewEndPointString ++ previewEndControlString)
                        , previewPoint to
                        , previewPoint startControl
                        , previewPoint model.mouseOffset
                        ]

        DrawingSmoothCubicCurve drawingSmoothCubicCurveState ->
            let
                smoothStartControl : Point
                smoothStartControl =
                    Point.reflectOver
                        endState.endPoint
                        endState.endControlPoint
            in
            case drawingSmoothCubicCurveState of
                DrawingOnePointCurveTo ->
                    let
                        previewCubicCurveString : String
                        previewCubicCurveString =
                            Path.commandToString
                                (Path.preFormattedCubicCurve
                                    { to = model.mouseOffset
                                    , startControl = smoothStartControl
                                    , endControl = model.mouseOffset
                                    }
                                )

                        previewStartControlString : String
                        previewStartControlString =
                            Path.commandToString
                                (Path.preFormattedLine
                                    { to = smoothStartControl }
                                )
                    in
                    Svg.g previewAttributes
                        [ viewPath
                            []
                            (pathEndPointString ++ previewCubicCurveString)
                        , viewPath
                            []
                            (pathEndPointString ++ previewStartControlString)
                        , previewPoint model.mouseOffset
                        , previewPoint smoothStartControl
                        ]

                DrawingControl { to } ->
                    let
                        previewCubicCurveString : String
                        previewCubicCurveString =
                            Path.commandToString
                                (Path.preFormattedCubicCurve
                                    { to = to
                                    , startControl = smoothStartControl
                                    , endControl = model.mouseOffset
                                    }
                                )

                        previewEndPointString : String
                        previewEndPointString =
                            Path.commandToString
                                (Path.preFormattedMove { to = to })

                        previewStartControlString : String
                        previewStartControlString =
                            Path.commandToString
                                (Path.preFormattedLine
                                    { to = smoothStartControl }
                                )

                        previewEndControlString : String
                        previewEndControlString =
                            Path.commandToString
                                (Path.preFormattedLine
                                    { to = model.mouseOffset }
                                )
                    in
                    Svg.g previewAttributes
                        [ viewPath
                            []
                            (pathEndPointString ++ previewCubicCurveString)
                        , viewPath
                            []
                            (pathEndPointString ++ previewStartControlString)
                        , viewPath
                            []
                            (previewEndPointString ++ previewEndControlString)
                        , previewPoint to
                        , previewPoint smoothStartControl
                        , previewPoint model.mouseOffset
                        ]

        DrawingQuadraticCurve drawingQuadraticCurve ->
            case drawingQuadraticCurve of
                DrawingOnePointCurveTo ->
                    let
                        previewQuadraticCurveString : String
                        previewQuadraticCurveString =
                            Path.commandToString
                                (Path.preFormattedQuadraticCurve
                                    { to = model.mouseOffset
                                    , control = model.mouseOffset
                                    }
                                )
                    in
                    Svg.g previewAttributes
                        [ viewPath
                            []
                            (pathEndPointString ++ previewQuadraticCurveString)
                        , previewPoint model.mouseOffset
                        ]

                DrawingControl { to } ->
                    let
                        previewQuadraticCurveString : String
                        previewQuadraticCurveString =
                            Path.commandToString
                                (Path.preFormattedQuadraticCurve
                                    { to = to
                                    , control = model.mouseOffset
                                    }
                                )

                        previewEndPointString : String
                        previewEndPointString =
                            Path.commandToString
                                (Path.preFormattedMove { to = to })

                        previewControlString : String
                        previewControlString =
                            Path.commandToString
                                (Path.preFormattedLine
                                    { to = model.mouseOffset }
                                )
                    in
                    Svg.g previewAttributes
                        [ viewPath
                            []
                            (pathEndPointString ++ previewQuadraticCurveString)
                        , viewPath
                            []
                            (pathEndPointString ++ previewControlString)
                        , viewPath
                            []
                            (previewEndPointString ++ previewControlString)
                        , previewPoint to
                        , previewPoint model.mouseOffset
                        ]

        DrawingSmoothQuadraticCurve ->
            let
                smoothControl : Point
                smoothControl =
                    Point.reflectOver
                        endState.endPoint
                        endState.controlPoint

                previewQuadraticCurveString : String
                previewQuadraticCurveString =
                    Path.commandToString
                        (Path.preFormattedQuadraticCurve
                            { to = model.mouseOffset
                            , control = smoothControl
                            }
                        )

                previewEndPointString : String
                previewEndPointString =
                    Path.commandToString
                        (Path.preFormattedMove { to = model.mouseOffset })

                previewControlString : String
                previewControlString =
                    Path.commandToString
                        (Path.preFormattedLine
                            { to = smoothControl }
                        )
            in
            Svg.g previewAttributes
                [ viewPath
                    []
                    (pathEndPointString ++ previewQuadraticCurveString)
                , viewPath
                    []
                    (pathEndPointString ++ previewControlString)
                , viewPath
                    []
                    (previewEndPointString ++ previewControlString)
                , previewPoint model.mouseOffset
                , previewPoint smoothControl
                ]

        _ ->
            Svg.g [] []


viewDefs : Svg Msg
viewDefs =
    let
        gridSize : Int
        gridSize =
            10

        gridSizeString : String
        gridSizeString =
            String.fromInt gridSize

        viewBoxString : String
        viewBoxString =
            String.join "," [ "0", "0", gridSizeString, gridSizeString ]
    in
    Svg.defs []
        [ Svg.pattern
            [ SvgA.id "grid"
            , SvgA.viewBox viewBoxString
            , SvgA.width gridSizeString
            , SvgA.height gridSizeString
            , SvgA.patternUnits "userSpaceOnUse"
            , SvgA.x "-0.25"
            , SvgA.y "-0.25"
            ]
            [ Svg.line
                [ SvgA.x1 "0"
                , SvgA.y1 "0"
                , SvgA.x2 gridSizeString
                , SvgA.y2 "0"
                , SvgA.stroke "black"
                ]
                []
            , Svg.line
                [ SvgA.x1 "0"
                , SvgA.y1 "0"
                , SvgA.x2 "0"
                , SvgA.y2 gridSizeString
                , SvgA.stroke "black"
                ]
                []
            ]
        ]


viewAxes : Model -> Svg Msg
viewAxes { viewBox } =
    Svg.g [ SvgA.opacity "50%", SvgA.strokeWidth "0.5" ]
        [ Svg.line
            [ SvgA.x1 (String.fromFloat viewBox.minX)
            , SvgA.y1 "0"
            , SvgA.x2 (String.fromFloat (viewBox.minX + viewBox.width))
            , SvgA.y2 "0"
            , SvgA.stroke "black"
            ]
            []
        , Svg.line
            [ SvgA.x1 "0"
            , SvgA.y1 (String.fromFloat viewBox.minY)
            , SvgA.x2 "0"
            , SvgA.y2 (String.fromFloat (viewBox.minY + viewBox.height))
            , SvgA.stroke "black"
            ]
            []
        ]


viewBackground : Model -> Svg Msg
viewBackground model =
    Svg.g []
        [ Svg.rect
            [ SvgA.x (String.fromFloat model.viewBox.minX)
            , SvgA.y (String.fromFloat model.viewBox.minY)
            , SvgA.width "100%"
            , SvgA.height "100%"
            , SvgA.fill "url(#grid)"
            , SvgE.onMouseDown MouseDownCanvas
            , SvgA.opacity "10%"
            ]
            []
        , viewAxes model
        ]


{-| Renders a Path as a single SVG element as well as an overlay above it to
interact with the Path.
-}
viewCanvas : Model -> Svg Msg
viewCanvas model =
    let
        baseOverlay : List (Svg Msg)
        baseOverlay =
            viewOverlay model.overlayConfig model.path

        overlay : List (Svg Msg)
        overlay =
            case model.state of
                Neutral ->
                    baseOverlay

                Clicking _ ->
                    baseOverlay

                Dragging { dragStart, temporarySelection } ->
                    viewDraggingPreview model dragStart temporarySelection
                        :: baseOverlay

                Selecting selectionStart ->
                    viewSelectionBox model selectionStart :: baseOverlay

                Drawing drawingState ->
                    viewDrawingPreview model drawingState :: baseOverlay
    in
    Svg.svg
        [ SvgA.viewBox (ViewBox.toString model.viewBox)
        , SvgA.width "100vw"
        , SvgA.height "100vh"
        , SvgA.display "block"
        ]
        [ viewDefs
        , viewBackground model
        , Svg.g [] overlay
        ]


boolToString : Bool -> String
boolToString value =
    if value then
        "True"

    else
        "False"


stateToString : State -> String
stateToString state =
    case state of
        Neutral ->
            "Neutral: "

        Clicking { position, temporarySelection } ->
            String.concat
                [ "Clicking: "
                , Point.toString position
                , " -> "
                , Path.selectionToString temporarySelection
                , " | "
                ]

        Dragging { dragStart, temporarySelection } ->
            String.concat
                [ "Dragging: "
                , Point.toString dragStart
                , " -> "
                , Path.selectionToString temporarySelection
                , " | "
                ]

        Selecting selectionStart ->
            String.concat
                [ "Selecting: "
                , Point.toString selectionStart
                , " | "
                ]

        Drawing drawingState ->
            case drawingState of
                DrawingMove ->
                    "Drawing Move: "

                DrawingLine ->
                    "Drawing Line: "

                DrawingHorizontalLine ->
                    "Drawing Horizontal Line: "

                DrawingVerticalLine ->
                    "Drawing Vertical Line: "

                DrawingCubicCurve drawingCubicCurveState ->
                    case drawingCubicCurveState of
                        DrawingCubicCurveTo ->
                            "Drawing Cubic Curve - EndPoint: "

                        DrawingStartControl _ ->
                            "Drawing Cubic Curve - Start Control: "

                        DrawingEndControl _ ->
                            "Drawing Cubic Curve - End Control: "

                DrawingSmoothCubicCurve drawingOnePointCurveState ->
                    case drawingOnePointCurveState of
                        DrawingOnePointCurveTo ->
                            "Drawing Smooth Cubic Curve - EndPoint: "

                        DrawingControl _ ->
                            "Drawing Smooth Cubic Curve - End Control: "

                DrawingQuadraticCurve drawingOnePointCurveState ->
                    case drawingOnePointCurveState of
                        DrawingOnePointCurveTo ->
                            "Drawing Quadratic Curve - EndPoint: "

                        DrawingControl _ ->
                            "Drawing Quadratic Curve - End Control: "

                DrawingSmoothQuadraticCurve ->
                    "Drawing Smooth Quadratic Curve: "

                DrawingArc _ ->
                    "Drawing Arc: "


viewViewBoxSize : ViewBox -> Float -> Html Msg
viewViewBoxSize viewBox zoomFactor =
    Html.p
        [ HtmlA.style "margin" "0"
        , HtmlA.style "padding-left" "10px"
        ]
        [ Html.text "Width: "
        , Html.text (String.fromInt (round viewBox.width))
        , Html.text ", Height: "
        , Html.text (String.fromInt (round viewBox.height))
        , Html.text ", Zoom: "
        , Html.text (String.fromFloat zoomFactor)
        ]


viewUndoRedo : Model -> Html Msg
viewUndoRedo { undoStack, redoStack } =
    let
        undoCount : Int
        undoCount =
            List.length undoStack

        redoCount : Int
        redoCount =
            List.length redoStack
    in
    Html.div
        [ HtmlA.style "padding-left" "10px" ]
        [ Html.button
            [ HtmlE.onClick Undo, HtmlA.disabled (undoCount == 0) ]
            [ Html.text
                (String.concat [ "Undo (", String.fromInt undoCount, ")" ])
            ]
        , Html.button
            [ HtmlE.onClick Redo, HtmlA.disabled (redoCount == 0) ]
            [ Html.text
                (String.concat [ "Redo (", String.fromInt redoCount, ")" ])
            ]
        ]


viewState : State -> Point -> Html Msg
viewState state offset =
    Html.p
        [ HtmlA.style "padding-left" "10px" ]
        [ Html.text <| stateToString state ++ Point.toString offset ]


viewPathStringInput : String -> Html Msg
viewPathStringInput pathString =
    Html.div
        [ HtmlA.style "display" "flex"
        , HtmlA.style "padding" "10px"
        ]
        [ Html.input
            [ HtmlA.value pathString
            , HtmlE.onInput PathStringChanged
            , HtmlA.style "width" "100%"
            , HtmlA.style "font-size" "64px"
            ]
            [ Html.text pathString ]
        ]


viewUI : Model -> Html Msg
viewUI model =
    Html.div
        [ HtmlA.style "position" "fixed"
        , HtmlA.style "display" "flex"
        , HtmlA.style "flex-direction" "column"
        , HtmlA.style "width" "100%"
        , HtmlA.style "bottom" "0"
        ]
        [ viewViewBoxSize model.viewBox model.zoomFactor
        , viewUndoRedo model
        , viewState model.state model.mouseOffset
        , viewPathStringInput model.pathString
        ]


view : Model -> Html Msg
view model =
    Html.div [] [ viewCanvas model, viewUI model ]



----------
-- Main --
----------


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
