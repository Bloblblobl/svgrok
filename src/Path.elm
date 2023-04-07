module Path exposing (..)

import Old.Path exposing (CommandType(..))
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
NoLetter is a special variant that inicdates that the Command was parsed as a
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


init : Path
init =
    { components = []
    , hovered = Nothing
    , selected = []
    }



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
points of smooth curve Commands to their absolute values.
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



----------------------
-- UPDATE FUNCTIONS --
----------------------


updateFromCommand : Command -> Point -> Point -> Command
updateFromCommand command oldFrom offset =
    case ( command.relation, command.commandType ) of
        ( Relative, MoveCommand { to } format ) ->
            { relation = Relative
            , commandType =
                MoveCommand { to = Point.subtract to offset } format
            }

        ( Relative, LineCommand { to } format ) ->
            { relation = Relative
            , commandType =
                LineCommand { to = Point.subtract to offset } format
            }

        -- convert HorizontalLine to normal Line if there is a Y offset
        ( relation, HorizontalLineCommand { toX } { afterLetter, afterToX } ) ->
            if offset.y /= 0 then
                let
                    newTo : Point
                    newTo =
                        case relation of
                            Absolute ->
                                { x = toX, y = oldFrom.y }

                            Relative ->
                                { x = toX - offset.x, y = offset.y }

                    newFormat : BaseFormat
                    newFormat =
                        { afterLetter = afterLetter
                        , afterTo = { x = afterToX, y = afterToX }
                        }
                in
                { relation = relation
                , commandType =
                    LineCommand { to = newTo } newFormat
                }

            else if relation == Relative then
                { relation = Relative
                , commandType =
                    HorizontalLineCommand
                        { toX = toX - offset.x }
                        { afterLetter = afterLetter, afterToX = afterToX }
                }

            else
                command

        -- convert VerticalLine to normal Line if there is an X offset
        ( relation, VerticalLineCommand { toY } { afterLetter, afterToY } ) ->
            if offset.x /= 0 then
                let
                    newTo : Point
                    newTo =
                        case relation of
                            Absolute ->
                                { x = oldFrom.x, y = toY }

                            Relative ->
                                { x = offset.x, y = toY - offset.y }

                    newFormat : BaseFormat
                    newFormat =
                        { afterLetter = afterLetter
                        , afterTo = { x = afterToY, y = afterToY }
                        }
                in
                { relation = relation
                , commandType =
                    LineCommand { to = newTo } newFormat
                }

            else if relation == Relative then
                { relation = Relative
                , commandType =
                    VerticalLineCommand
                        { toY = toY - offset.y }
                        { afterLetter = afterLetter, afterToY = afterToY }
                }

            else
                command

        ( Relative, CubicCurveCommand params format ) ->
            { relation = Relative
            , commandType =
                CubicCurveCommand
                    { params | to = Point.subtract params.to offset }
                    format
            }

        ( Relative, SmoothCubicCurveCommand params format ) ->
            { relation = Relative
            , commandType =
                SmoothCubicCurveCommand
                    { params | to = Point.subtract params.to offset }
                    format
            }

        ( Relative, QuadraticCurveCommand params format ) ->
            { relation = Relative
            , commandType =
                QuadraticCurveCommand
                    { params | to = Point.subtract params.to offset }
                    format
            }

        ( Relative, SmoothQuadraticCurveCommand params format ) ->
            { relation = Relative
            , commandType =
                SmoothQuadraticCurveCommand
                    { params | to = Point.subtract params.to offset }
                    format
            }

        ( Relative, ArcCommand params format ) ->
            { relation = Relative
            , commandType =
                ArcCommand
                    { params | to = Point.subtract params.to offset }
                    format
            }

        _ ->
            command


{-| Updates the from parameter of a Component to a new absolute point. Updates
the Segment's from value directly and calls updateFromCommand to handle updating
the Command.
-}
updateFrom : Component -> Point -> Component
updateFrom { command, segment } newFrom =
    case segment of
        MoveSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newFrom params.from
            in
            { command = updateFromCommand command params.from offset
            , segment =
                MoveSegment { params | from = Point.add offset params.from }
            }

        LineSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newFrom params.from
            in
            { command = updateFromCommand command params.from offset
            , segment =
                LineSegment { params | from = Point.add offset params.from }
            }

        QuadraticCurveSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newFrom params.from
            in
            { command = updateFromCommand command params.from offset
            , segment =
                QuadraticCurveSegment
                    { params
                        | from = Point.add offset params.from
                    }
            }

        CubicCurveSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newFrom params.from
            in
            { command = updateFromCommand command params.from offset
            , segment =
                CubicCurveSegment
                    { params
                        | from = Point.add offset params.from
                    }
            }

        ArcSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newFrom params.from
            in
            { command = updateFromCommand command params.from offset
            , segment =
                ArcSegment { params | from = Point.add offset params.from }
            }

        CloseSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newFrom params.from
            in
            -- The CloseCommand never needs to be updated
            { command = command
            , segment =
                CloseSegment { params | from = Point.add offset params.from }
            }


updateToCommand : Command -> Point -> Point -> Command
updateToCommand command oldTo offset =
    case command.commandType of
        MoveCommand { to } format ->
            { command
                | commandType = MoveCommand { to = Point.add to offset } format
            }

        LineCommand { to } format ->
            { command
                | commandType = LineCommand { to = Point.add to offset } format
            }

        -- convert HorizontalLine to normal Line if there is a Y offset
        HorizontalLineCommand { toX } { afterLetter, afterToX } ->
            if offset.y /= 0 then
                let
                    newTo : Point
                    newTo =
                        case command.relation of
                            Absolute ->
                                { x = toX + offset.x, y = oldTo.y + offset.y }

                            Relative ->
                                { x = toX + offset.x, y = offset.y }

                    newFormat : BaseFormat
                    newFormat =
                        { afterLetter = afterLetter
                        , afterTo = { x = afterToX, y = afterToX }
                        }
                in
                { command
                    | commandType =
                        LineCommand { to = newTo } newFormat
                }

            else
                { command
                    | commandType =
                        HorizontalLineCommand
                            { toX = toX + offset.x }
                            { afterLetter = afterLetter, afterToX = afterToX }
                }

        -- convert VerticalLine to normal Line if there is an X offset
        VerticalLineCommand { toY } { afterLetter, afterToY } ->
            if offset.x /= 0 then
                let
                    newTo : Point
                    newTo =
                        case command.relation of
                            Absolute ->
                                { x = oldTo.x + offset.x, y = toY + offset.y }

                            Relative ->
                                { x = offset.x, y = toY + offset.y }

                    newFormat : BaseFormat
                    newFormat =
                        { afterLetter = afterLetter
                        , afterTo = { x = afterToY, y = afterToY }
                        }
                in
                { command
                    | commandType =
                        LineCommand { to = newTo } newFormat
                }

            else
                { command
                    | commandType =
                        VerticalLineCommand
                            { toY = toY + offset.y }
                            { afterLetter = afterLetter, afterToY = afterToY }
                }

        CubicCurveCommand params format ->
            { command
                | commandType =
                    CubicCurveCommand
                        { params | to = Point.add params.to offset }
                        format
            }

        SmoothCubicCurveCommand params format ->
            { command
                | commandType =
                    SmoothCubicCurveCommand
                        { params | to = Point.add params.to offset }
                        format
            }

        QuadraticCurveCommand params format ->
            { command
                | commandType =
                    QuadraticCurveCommand
                        { params | to = Point.add params.to offset }
                        format
            }

        SmoothQuadraticCurveCommand params format ->
            { command
                | commandType =
                    SmoothQuadraticCurveCommand
                        { params | to = Point.add params.to offset }
                        format
            }

        ArcCommand params format ->
            { command
                | commandType =
                    ArcCommand
                        { params | to = Point.add params.to offset }
                        format
            }

        _ ->
            command


{-| Updates the to parameter of a Component to a new absolute point. Updates
the Segment's to value directly and calls updateToCommand to handle updating
the Command.
-}
updateTo : Component -> Point -> Component
updateTo { command, segment } newTo =
    case segment of
        MoveSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newTo params.to
            in
            { command = updateToCommand command params.to offset
            , segment = MoveSegment { params | to = Point.add offset params.to }
            }

        LineSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newTo params.to
            in
            { command = updateToCommand command params.to offset
            , segment = LineSegment { params | to = Point.add offset params.to }
            }

        QuadraticCurveSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newTo params.to
            in
            { command = updateToCommand command params.to offset
            , segment =
                QuadraticCurveSegment
                    { params | to = Point.add offset params.to }
            }

        CubicCurveSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newTo params.to
            in
            { command = updateToCommand command params.to offset
            , segment =
                CubicCurveSegment { params | to = Point.add offset params.to }
            }

        ArcSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newTo params.to
            in
            { command = updateToCommand command params.to offset
            , segment = ArcSegment { params | to = Point.add offset params.to }
            }

        CloseSegment params ->
            let
                offset : Point
                offset =
                    Point.subtract newTo params.to
            in
            -- The CloseCommand never needs to be updated
            { command = command
            , segment =
                CloseSegment { params | to = Point.add offset params.to }
            }



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
