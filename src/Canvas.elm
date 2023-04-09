module Canvas exposing (..)

import Json.Decode exposing (index)
import Path exposing (Element(..), Path)
import Point exposing (Point)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import ViewBox exposing (ViewBox)


type alias State r =
    { r
        | pathString : String
        , path : Path
        , mouseOffset : Maybe Point
        , mouseOverOverlay : Bool
        , mouseDown : Maybe Point
        , viewBox : ViewBox
    }


type Msg
    = SetHoveredElement (Maybe Path.Selection)
    | ToggleSelection Path.Selection
    | MouseMove Point
    | MouseEnter
    | MouseLeave
    | MouseDown Point
    | MouseUp


update : Msg -> State r -> State r
update msg state =
    case msg of
        SetHoveredElement selection ->
            let
                { path } =
                    state
            in
            { state | path = { path | hovered = selection } }

        ToggleSelection selection ->
            let
                { path } =
                    state

                removedFromSelected : List Path.Selection
                removedFromSelected =
                    List.filter (\s -> s /= selection) path.selected
            in
            if List.member selection path.selected then
                { state | path = { path | selected = removedFromSelected } }

            else
                { state
                    | path =
                        { path | selected = selection :: path.selected }
                }

        MouseMove newOffset ->
            { state
                | mouseOffset =
                    Just (ViewBox.scalePoint state.viewBox newOffset)
            }

        MouseEnter ->
            { state | mouseOverOverlay = True }

        MouseLeave ->
            { state | mouseOverOverlay = False }

        MouseDown mouseOffset ->
            { state
                | mouseDown =
                    Just (ViewBox.scalePoint state.viewBox mouseOffset)
            }

        MouseUp ->
            let
                newPath : Path
                newPath =
                    case ( state.mouseDown, state.mouseOffset ) of
                        ( Just mouseDown, Just mouseOffset ) ->
                            Path.update
                                state.path
                                (Point.subtract mouseOffset mouseDown)

                        _ ->
                            state.path
            in
            { state
                | path = newPath
                , mouseDown = Nothing
                , pathString = Path.toString newPath
            }


type alias OverlayConfig =
    { default : List (Attribute Msg)
    , hovered : List (Attribute Msg)
    , selected : List (Attribute Msg)
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
    , SvgE.onClick (ToggleSelection selection)
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
    Svg.circle (attributes ++ pointAttributes) []


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


{-| Renders a Path as a single SVG element as well as an overlay above it to
interact with the Path.
-}
view : ViewBox -> OverlayConfig -> Path -> Svg Msg
view viewBox config path =
    Svg.svg
        [ SvgA.viewBox (ViewBox.toString viewBox)
        , SvgA.width "100vw"
        , SvgA.height "100vh"
        , SvgA.display "block"
        , SvgE.onMouseOver MouseEnter
        , SvgE.onMouseOut MouseLeave
        ]
        [ Svg.g
            []
            (viewOverlay config path)
        ]
