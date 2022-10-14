module Path exposing (..)

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


{-| The Separator between 2 arbitrary semantic tokens in a Command string.
NoLetter is a special variant that indicates that the Command was parsed as a
subsequent set of parameters.
-}
type Separator
    = Spaces Int
    | Comma { spacesBefore : Int, spacesAfter : Int }
    | NoLetter


{-| Grouped Separators for a Point (that follow the respective X/Y value).
-}
type alias PointSeparator =
    { x : Separator, y : Separator }



------------------------
-- COMMAND PARAMETERS --
------------------------


{-| For Move, Line, and SmoothQuadraticCurve CommandTypes.
-}
type alias BaseParameters =
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


{-| For Move, Line, and SmoothQuadraticCurve CommandTypes.
-}
type alias BaseFormat =
    { afterLetter : Separator, afterTo : PointSeparator }


type alias HorizontalLineFormat =
    { afterLetter : Separator, afterToX : Separator }


type alias VerticalLineFormat =
    { afterLetter : Separator, afterToY : Separator }


type alias CubicCurveFormat =
    { afterLetter : Separator
    , afterStartControl : PointSeparator
    , afterEndControl : PointSeparator
    , afterTo : PointSeparator
    }


type alias SmoothCubicCurveFormat =
    { afterLetter : Separator
    , afterEndControl : PointSeparator
    , afterTo : PointSeparator
    }


type alias QuadratricCurveFormat =
    { afterLetter : Separator
    , afterControl : PointSeparator
    , afterTo : PointSeparator
    }


type alias ArcFormat =
    { afterLetter : Separator
    , afterRadii : PointSeparator
    , afterAngle : Separator
    , afterSize : Separator
    , afterRotation : Separator
    , afterTo : PointSeparator
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
absolute values.
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
    }


{-| The smallest division of a Component that can be interacted with/modified.
-}
type Element
    = EndPoint
    | StartControl
    | EndControl
    | Control
    | Segment


{-| A selection representing a Component at the index in a list of Components
with an optional Element specified.
-}
type alias Selection =
    { index : Int
    , element : Element
    }


{-| A list of Components with contextual interaction information.
-}
type alias Path =
    { components : List Component
    , hovered : Maybe Selection
    , selected : List Selection
    }


type Msg
    = SetHoveredElement (Maybe Selection)
    | ToggleSelection Selection
    | MouseMove Point
    | SetMouseOver Bool
    | SetMouseDown Bool



---------------------
-- BUILD FUNCTIONS --
---------------------


{-| The implicit state of a Path, used when building up a list of Components
from Commands. The firstConnectedPoint is the Point where the current sequence
of consecutive Commands started, a.k.a. the target Point of the last Move
Command.
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
buildComponent : Command -> ComponentBuilder -> ComponentBuilder
buildComponent command componentBuilder =
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


{-| Build a list of Components from a list of Commands.
-}
buildComponents : List Command -> List Component
buildComponents commands =
    List.foldl buildComponent initComponentBuilder commands
        |> .components
        |> List.reverse



-------------------------
-- TO STRING FUNCTIONS --
-------------------------


arcSizeToString : ArcSize -> String
arcSizeToString size =
    case size of
        Large ->
            "1"

        Small ->
            "0"


arcRotationToString : ArcRotation -> String
arcRotationToString rotation =
    case rotation of
        Clockwise ->
            "1"

        CounterClockwise ->
            "0"


separatorToString : Separator -> String
separatorToString separator =
    case separator of
        Spaces count ->
            String.repeat count " "

        Comma { spacesBefore, spacesAfter } ->
            String.concat
                [ String.repeat spacesBefore " "
                , ","
                , String.repeat spacesAfter " "
                ]

        NoLetter ->
            ""


pointToString : Point -> PointSeparator -> String
pointToString point separator =
    String.concat
        [ String.fromFloat point.x
        , separatorToString separator.x
        , String.fromFloat point.y
        , separatorToString separator.y
        ]


{-| Converts a Point to a String with default separators of one space between
the values and no spaces at the end.
-}
pointToStringNoSpace : Point -> String
pointToStringNoSpace point =
    String.join " "
        [ String.fromFloat point.x
        , String.fromFloat point.y
        ]


{-| Converts a Point to a String with default separators of one space between
the values and at the end.
-}
pointToStringOneSpace : Point -> String
pointToStringOneSpace point =
    String.join " "
        [ String.fromFloat point.x
        , String.fromFloat point.y
        , ""
        ]


commandToString : Command -> String
commandToString { relation, commandType } =
    let
        letter : Separator -> String -> String
        letter separator upper =
            case separator of
                NoLetter ->
                    ""

                _ ->
                    case relation of
                        Absolute ->
                            upper ++ separatorToString separator

                        Relative ->
                            String.toLower upper ++ separatorToString separator
    in
    case commandType of
        MoveCommand { to } { afterLetter, afterTo } ->
            letter afterLetter "M" ++ pointToString to afterTo

        LineCommand { to } { afterLetter, afterTo } ->
            letter afterLetter "L" ++ pointToString to afterTo

        HorizontalLineCommand { toX } { afterLetter, afterToX } ->
            String.concat
                [ letter afterLetter "H"
                , String.fromFloat toX
                , separatorToString afterToX
                ]

        VerticalLineCommand { toY } { afterLetter, afterToY } ->
            String.concat
                [ letter afterLetter "V"
                , String.fromFloat toY
                , separatorToString afterToY
                ]

        CubicCurveCommand parameters format ->
            let
                { startControl, endControl, to } =
                    parameters

                { afterLetter, afterStartControl, afterEndControl, afterTo } =
                    format
            in
            String.concat
                [ letter afterLetter "C"
                , pointToString startControl afterStartControl
                , pointToString endControl afterEndControl
                , pointToString to afterTo
                ]

        SmoothCubicCurveCommand parameters format ->
            let
                { endControl, to } =
                    parameters

                { afterLetter, afterEndControl, afterTo } =
                    format
            in
            String.concat
                [ letter afterLetter "S"
                , pointToString endControl afterEndControl
                , pointToString to afterTo
                ]

        QuadraticCurveCommand parameters format ->
            let
                { control, to } =
                    parameters

                { afterLetter, afterControl, afterTo } =
                    format
            in
            String.concat
                [ letter afterLetter "Q"
                , pointToString control afterControl
                , pointToString to afterTo
                ]

        SmoothQuadraticCurveCommand { to } { afterLetter, afterTo } ->
            letter afterLetter "T" ++ pointToString to afterTo

        ArcCommand parameters format ->
            let
                { radii, angle, size, rotation, to } =
                    parameters

                { afterRadii, afterAngle, afterSize, afterRotation, afterTo } =
                    format
            in
            String.concat
                [ letter format.afterLetter "A"
                , pointToString radii afterRadii
                , String.fromFloat angle
                , separatorToString afterAngle
                , arcSizeToString size
                , separatorToString afterSize
                , arcRotationToString rotation
                , separatorToString afterRotation
                , pointToString to afterTo
                ]

        CloseCommand { afterLetter } ->
            letter afterLetter "Z"


segmentToString : Segment -> String
segmentToString segment =
    let
        moveString : Point -> String
        moveString from =
            "M" ++ pointToStringNoSpace from
    in
    case segment of
        MoveSegment { from, to } ->
            moveString from ++ moveString to

        LineSegment { from, to } ->
            moveString from ++ "L" ++ pointToStringNoSpace to

        CubicCurveSegment { startControl, endControl, from, to } ->
            String.concat
                [ moveString from
                , "C"
                , pointToStringOneSpace startControl
                , pointToStringOneSpace endControl
                , pointToStringNoSpace to
                ]

        QuadraticCurveSegment { control, from, to } ->
            String.concat
                [ moveString from
                , "Q"
                , pointToStringOneSpace control
                , pointToStringNoSpace to
                ]

        ArcSegment { radii, angle, size, rotation, from, to } ->
            String.concat
                [ moveString from
                , "A"
                , pointToStringOneSpace radii
                , String.fromFloat angle ++ " "
                , arcSizeToString size ++ " "
                , arcRotationToString rotation ++ " "
                , pointToStringNoSpace to
                ]

        CloseSegment { from, to } ->
            moveString from ++ "L" ++ pointToStringNoSpace to


toString : Path -> String
toString { components } =
    let
        step : Component -> String -> String
        step { command } commandString =
            commandString ++ commandToString command
    in
    List.foldl step "" components
