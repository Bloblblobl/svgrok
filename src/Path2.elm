module Path2 exposing (..)

import Point exposing (Point)


{-| The relation of a Command to the implicit current Point. If Absolute, the
parameters of the Command are absolute values. Otherwise, the values are
relative to the current Point.
-}
type Relation
    = Absolute
    | Relative


{-| The size of an arc Command. If Large, the arc will be drawn along the
longest valid path between the start and end Points. Otherwise, it will be drawn
along the shortest valid path. The paths first depend on the ArcRotation.
-}
type ArcSize
    = Large
    | Small


{-| The rotation of an arc Command (also known as sweep). If Clockwise, the arc
will sweep clockwise from the start to the end Points. Otherwise, it will sweep
counterclockwise.
-}
type ArcRotation
    = Clockwise
    | CounterClockwise


{-| The separator between 2 arbitrary semantic tokens in a Command string.
-}
type Separator
    = NoSpace
    | Spaces Int
    | Comma { spacesBefore : Int, spacesAfter : Int }


{-| Separators for an X-Y coordinate pair.
-}
type alias CoordinateSeparator =
    { x : Separator, y : Separator }



------------------------
-- COMMAND PARAMETERS --
------------------------


type alias BaseParameters =
    -- For Move, Line, and SmoothQuadraticCurve CommandTypes
    { to : Point }


type alias HorizontalLineParameters =
    { toX : Float }


type alias VerticalLineParameters =
    { toY : Float }


type alias CubicCurveParameters =
    { startControl : Point, endControl : Point, to : Point }


type alias SmoothCubicCurveParameters =
    { endControl : Point, to : Point }


type alias QuadraticCurveParameters =
    { control : Point, to : Point }


type alias ArcParameters =
    { radii : Point
    , angle : Float
    , size : ArcSize
    , rotation : ArcRotation
    , to : Point
    }



---------------------
-- COMMAND FORMATS --
---------------------


type alias BaseFormat =
    -- For Move, Line, and SmoothQuadraticCurve CommandTypes
    { afterLetter : Separator, afterTo : CoordinateSeparator }


type alias HorizontalLineFormat =
    { afterLetter : Separator, afterToX : Separator }


type alias VerticalLineFormat =
    { afterLetter : Separator, afterToY : Separator }


type alias CubicCurveFormat =
    { afterLetter : Separator
    , afterStartControl : CoordinateSeparator
    , afterEndControl : CoordinateSeparator
    , afterTo : CoordinateSeparator
    }


type alias SmoothCubicCurveFormat =
    { afterLetter : Separator
    , afterEndControl : CoordinateSeparator
    , afterTo : CoordinateSeparator
    }


type alias QuadratricCurveFormat =
    { afterLetter : Separator
    , afterControl : CoordinateSeparator
    , afterTo : CoordinateSeparator
    }


type alias ArcFormat =
    { afterLetter : Separator
    , afterRadii : CoordinateSeparator
    , afterAngle : Separator
    , afterSize : Separator
    , afterRotation : Separator
    , afterTo : CoordinateSeparator
    }


type alias CloseFormat =
    { afterLetter : Separator }


{-| The type of a Command, describing its relevant parameters and format.
-}
type CommandType
    = MoveCommand BaseParameters BaseFormat
    | LineCommand BaseParameters BaseFormat
    | HorizontalLineCommand HorizontalLineParameters HorizontalLineFormat
    | VerticalLineCommand VerticalLineParameters VerticalLineFormat
    | CubicCurveCommand CubicCurveParameters CubicCurveFormat
    | SmoothCubicCurveCommand SmoothCubicCurveParameters SmoothCubicCurveFormat
    | QuadraticCurveCommand QuadraticCurveParameters QuadratricCurveFormat
    | SmoothQuadraticCurveCommand BaseParameters BaseFormat
    | ArcCommand ArcParameters ArcFormat
    | CloseCommand CloseFormat


{-| An instruction that defines the next Segment of the Path to be drawn.
-}
type alias Command =
    { relation : Relation
    , commandType : CommandType
    }


{-| A Segment of a Path that includes the start and end Points of the Segment as
well as the relevant parameters (for Segments like curves and arcs), all with
absolute values. Always maps to a corresponding Command.
-}
type Segment
    = MoveSegment { from : Point, to : Point }
    | LineSegment { from : Point, to : Point }
    | CubicCurveSegment
        { startControl : Point
        , endControl : Point
        , from : Point
        , to : Point
        }
    | QuadraticCurveSegment { control : Point, from : Point, to : Point }
    | ArcSegment
        { radii : Point
        , angle : Float
        , size : ArcSize
        , rotation : ArcRotation
        , from : Point
        , to : Point
        }
    | CloseSegment { from : Point, to : Point }


{-| Combines a Command and the corresponding Segment relative to the previous
Command into a single structure.
-}
type alias Component =
    { command : Command
    , segment : Segment
    , string : String
    }


{-| The smallest relevant division of a Component.
-}
type Element
    = End
    | StartControl
    | EndControl
    | Control
    | Segment


{-| A selection representing a Component at the index in a list of Components
with an optional Element specified.
-}
type alias Selection =
    { index : Int
    , element : Maybe Element
    }


{-| A list of Components with some optional selections on the list.
-}
type alias Path =
    { components : List Component
    , hovered : Maybe Selection
    , selected : List Selection
    }


{-| The implicit state of a Path, used when building up a list of Components
from Commands.
-}
type alias ComponentBuilder =
    { components : List Component
    , currentPoint : Point
    , firstConnectedPoint : Point
    }


initComponentBuilder : ComponentBuilder
initComponentBuilder =
    { components = []
    , currentPoint = Point.zero
    , firstConnectedPoint = Point.zero
    }


{-| Create a Path from a list of Components.
-}
fromComponents : List Component -> Path
fromComponents components =
    { components = components
    , hovered = Nothing
    , selected = []
    }


{-| Return the absolute previous control based on the current Command being
processed and the previous Segment. Used when resolving the implicit control
points of smooth curve Commands into their absolute values.
-}
absolutePreviousControl : Segment -> Command -> Point -> Point
absolutePreviousControl previousSegment command defaultPoint =
    case ( previousSegment, command.commandType ) of
        ( CubicCurveSegment params, SmoothCubicCurveCommand _ _ ) ->
            params.endControl

        ( QuadraticCurveSegment params, SmoothQuadraticCurveCommand _ _ ) ->
            params.control

        _ ->
            defaultPoint


{-| Build a Component from a tuple of Command and String and a ComponentBuilder.
-}
buildComponent : ( Command, String ) -> ComponentBuilder -> ComponentBuilder
buildComponent ( command, string ) componentBuilder =
    let
        { relation, commandType } =
            command

        { components, currentPoint, firstConnectedPoint } =
            componentBuilder

        absolute : Point -> Point
        absolute ambiguousPoint =
            case relation of
                Absolute ->
                    ambiguousPoint

                Relative ->
                    Point.add currentPoint ambiguousPoint

        previousControl : Point
        previousControl =
            case List.head components of
                Just { segment } ->
                    absolutePreviousControl segment command currentPoint

                Nothing ->
                    currentPoint

        newSegment : Segment
        newSegment =
            case commandType of
                MoveCommand { to } _ ->
                    MoveSegment { from = currentPoint, to = absolute to }

                LineCommand { to } _ ->
                    LineSegment { from = currentPoint, to = absolute to }

                HorizontalLineCommand { toX } _ ->
                    case relation of
                        Absolute ->
                            LineSegment
                                { from = currentPoint
                                , to = { x = toX, y = currentPoint.y }
                                }

                        Relative ->
                            LineSegment
                                { from = currentPoint
                                , to =
                                    { x = currentPoint.x + toX
                                    , y = currentPoint.y
                                    }
                                }

                VerticalLineCommand { toY } _ ->
                    case relation of
                        Absolute ->
                            LineSegment
                                { from = currentPoint
                                , to = { x = currentPoint.x, y = toY }
                                }

                        Relative ->
                            LineSegment
                                { from = currentPoint
                                , to =
                                    { x = currentPoint.x
                                    , y = currentPoint.y + toY
                                    }
                                }

                CubicCurveCommand { startControl, endControl, to } _ ->
                    CubicCurveSegment
                        { startControl = absolute startControl
                        , endControl = absolute endControl
                        , from = currentPoint
                        , to = absolute to
                        }

                SmoothCubicCurveCommand { endControl, to } _ ->
                    CubicCurveSegment
                        { startControl = previousControl
                        , endControl = absolute endControl
                        , from = currentPoint
                        , to = absolute to
                        }

                QuadraticCurveCommand { control, to } _ ->
                    QuadraticCurveSegment
                        { control = absolute control
                        , from = currentPoint
                        , to = absolute to
                        }

                SmoothQuadraticCurveCommand { to } _ ->
                    QuadraticCurveSegment
                        { control = previousControl
                        , from = currentPoint
                        , to = absolute to
                        }

                ArcCommand { radii, angle, size, rotation, to } _ ->
                    ArcSegment
                        { radii = radii
                        , angle = angle
                        , size = size
                        , rotation = rotation
                        , from = currentPoint
                        , to = absolute to
                        }

                CloseCommand _ ->
                    CloseSegment
                        { from = currentPoint
                        , to = firstConnectedPoint
                        }

        newComponent : Component
        newComponent =
            { command = command
            , segment = newSegment
            , string = string
            }

        nextCurrentPoint : Point
        nextCurrentPoint =
            case newSegment of
                MoveSegment { to } ->
                    to

                LineSegment { to } ->
                    to

                CubicCurveSegment { to } ->
                    to

                QuadraticCurveSegment { to } ->
                    to

                ArcSegment { to } ->
                    to

                CloseSegment { to } ->
                    to

        nextFirstConnectedPoint : Point
        nextFirstConnectedPoint =
            case newSegment of
                MoveSegment { to } ->
                    to

                _ ->
                    firstConnectedPoint
    in
    { components = newComponent :: components
    , currentPoint = nextCurrentPoint
    , firstConnectedPoint = nextFirstConnectedPoint
    }


{-| Build a list of Components from a list of Commands and Strings.
-}
buildComponents : List ( Command, String ) -> List Component
buildComponents commandsAndStrings =
    List.foldl buildComponent initComponentBuilder commandsAndStrings
        |> .components
        |> List.reverse
