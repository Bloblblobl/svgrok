module Path.View exposing (..)

import Path exposing (Path)
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
    { default : List (Attribute msg)
    , hovered : List (Attribute msg)
    , selected : List (Attribute msg)
    }


type alias OverlayBuilder =
    { config : OverlayConfig
    , hovered : Maybe Path.Selection
    , selected : List Path.Selection
    , points : List (Svg msg)
    , segments : List (Svg msg)
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
mouseEvents : Path.Selection -> List (Attribute Path.Msg)
mouseEvents selection =
    [ SvgE.onMouseOver (Path.OverlayHover selection)
    , SvgE.onMouseOut Path.OverlayUnhover
    , SvgE.onClick (Path.OverlayToggleSelection selection)
    ]


{-| Get a list of Attributes from a Builder for a Selection.
-}
getAttributes : OverlayBuilder -> Path.Selection -> List (Attribute Path.Msg)
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
path : List (Attribute msg) -> String -> Svg msg
path attributes pathString =
    Svg.path (SvgA.d pathString :: attributes) []


{-| Renders a Point as a SVG <circle> element.
-}
point : List (Attribute msg) -> Point -> Svg msg
point attributes { x, y } =
    let
        pointAttributes : List (Attribute msg)
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

                endPoint : Svg Path.Msg
                endPoint =
                    point (getAttributes builder selection) to
            in
            { builder | points = endPoint :: builder.points }

        Path.LineSegment { to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                segment : Svg Path.Msg
                segment =
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Path.Msg
                endPoint =
                    point (getAttributes builder endSelection) to
            in
            { builder
                | points = endPoint :: builder.points
                , segments = segment :: builder.segments
            }

        Path.CubicCurveSegment { startControl, endControl, to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                segment : Svg Path.Msg
                segment =
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                startSelection : Path.Selection
                startSelection =
                    { index = index, element = Path.StartControl }

                startControlPoint : Svg Path.Msg
                startControlPoint =
                    point (getAttributes builder startSelection) startControl

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndControl }

                endControlPoint : Svg Path.Msg
                endControlPoint =
                    point (getAttributes builder endSelection) endControl

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Path.Msg
                endPoint =
                    point (getAttributes builder endSelection) to
            in
            { builder
                | points =
                    List.append
                        [ startControlPoint, endControlPoint, endPoint ]
                        builder.points
                , segments = segment :: builder.segments
            }

        Path.QuadraticCurveSegment { control, to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                segment : Svg Path.Msg
                segment =
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                controlSelection : Path.Selection
                controlSelection =
                    { index = index, element = Path.Control }

                controlPoint : Svg Path.Msg
                controlPoint =
                    point (getAttributes builder controlSelection) control

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Path.Msg
                endPoint =
                    point (getAttributes builder endSelection) to
            in
            { builder
                | points = controlPoint :: endPoint :: builder.points
                , segments = segment :: builder.segments
            }

        Path.ArcSegment { to } ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                segment : Svg Path.Msg
                segment =
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)

                endSelection : Path.Selection
                endSelection =
                    { index = index, element = Path.EndPoint }

                endPoint : Svg Path.Msg
                endPoint =
                    point (getAttributes builder endSelection) to
            in
            { builder
                | points = endPoint :: builder.points
                , segments = segment :: builder.segments
            }

        Path.CloseSegment _ ->
            let
                segmentSelection : Path.Selection
                segmentSelection =
                    { index = index, element = Path.Segment }

                segment : Svg Path.Msg
                segment =
                    path
                        (getAttributes builder segmentSelection)
                        (Path.segmentToString component.segment)
            in
            { builder | segments = segment :: builder.segments }


{-| Renders an overlay of a Path, with separate SVG elements for each Point and
Segment of the Path.
-}
overlay : ViewBox -> OverlayConfig -> Path -> Svg msg
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
