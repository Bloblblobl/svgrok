module Path.View exposing (..)

import Path exposing (Msg, Path)
import Point exposing (Point)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE


type alias Size a =
    { a | width : Float, height : Float }


type alias ViewBox =
    { minX : Float
    , minY : Float
    , width : Float
    , height : Float
    }


viewBoxString : ViewBox -> String
viewBoxString { minX, minY, width, height } =
    String.join " "
        [ String.fromFloat minX
        , String.fromFloat minY
        , String.fromFloat width
        , String.fromFloat height
        ]


type alias OverlayConfig =
    { default : List (Attribute Msg)
    , hovered : List (Attribute Msg)
    , selected : List (Attribute Msg)
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


{-| Adds relevant mouse events for a Selection to a list of Attributes.
-}
mouseEvents : Path.Selection -> List (Attribute Msg)
mouseEvents selection =
    [ SvgE.onMouseOver (Path.SetHoveredElement <| Just selection)
    , SvgE.onMouseOut (Path.SetHoveredElement Nothing)
    , SvgE.onClick (Path.ToggleSelection selection)
    ]


{-| Get a list of Attributes from a Builder for a Selection.
-}
getAttributes : OverlayBuilder -> Path.Selection -> List (Attribute Msg)
getAttributes { config, hovered, selected } selection =
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
    List.append (mouseEvents selection) <|
        if isHovered then
            config.hovered

        else if isSelected then
            config.selected

        else
            config.default


{-| Renders a Path String as a single continuous SVG <path> element.
-}
path : List (Attribute Msg) -> String -> Svg Msg
path attributes pathString =
    Svg.path (SvgA.d pathString :: attributes) []


{-| Renders a Point as a SVG <circle> element.
-}
point : List (Attribute Msg) -> Point -> Svg Msg
point attributes { x, y } =
    let
        pointAttributes : List (Attribute Msg)
        pointAttributes =
            [ SvgA.cx (String.fromFloat x)
            , SvgA.cy (String.fromFloat y)
            ]
    in
    Svg.circle (attributes ++ pointAttributes) []


segment : ( Int, Path.Component ) -> OverlayBuilder -> OverlayBuilder
segment ( index, component ) builder =
    case component.segment of
        Path.MoveSegment { to } ->
            let
                selection : Path.Selection
                selection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    point (getAttributes builder selection) to
            in
            { builder | points = endPoint :: builder.points }

        Path.LineSegment { to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                lineSegment : Svg Msg
                lineSegment =
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    point (getAttributes builder endSelection) to
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
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                startSelection : Path.Selection
                startSelection =
                    { index = index, element = Path.StartControl }

                startControlPoint : Svg Msg
                startControlPoint =
                    point (getAttributes builder startSelection) startControl

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndControl }

                endControlPoint : Svg Msg
                endControlPoint =
                    point (getAttributes builder endSelection) endControl

                endPointSelection : Path.Selection
                endPointSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    point (getAttributes builder endPointSelection) to
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
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                controlSelection : Path.Selection
                controlSelection =
                    { index = index, element = Path.Control }

                controlPoint : Svg Msg
                controlPoint =
                    point (getAttributes builder controlSelection) control

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    point (getAttributes builder endSelection) to
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
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Msg
                endPoint =
                    point (getAttributes builder endSelection) to
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
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)
            in
            { builder | segments = closeSegment :: builder.segments }


{-| Renders an overlay of a Path, with separate SVG elements for each Point and
Segment of the Path.
-}
overlay : ViewBox -> OverlayConfig -> Path -> Svg Msg
overlay viewBox config p =
    let
        initialBuilder : OverlayBuilder
        initialBuilder =
            initOverlayBuilder config p

        indexedComponents : List ( Int, Path.Component )
        indexedComponents =
            List.indexedMap Tuple.pair p.components
    in
    List.foldl segment initialBuilder indexedComponents
        |> (\builder -> builder.points ++ builder.segments)
        |> Svg.svg [ SvgA.viewBox (viewBoxString viewBox) ]
