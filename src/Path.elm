module Path exposing (..)

import Old.Path exposing (CommandType(..))
import Point exposing (Point)



---------------------
-- COMMAND FORMATS --
---------------------


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


type alias QuadraticCurveFormat =
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


expandHorizontalFormat : HorizontalLineFormat -> BaseFormat
expandHorizontalFormat { afterLetter, afterToX } =
    if afterToX == Spaces 0 then
        { afterLetter = afterLetter
        , afterTo = { x = Spaces 1, y = Spaces 0 }
        }

    else
        { afterLetter = afterLetter
        , afterTo = { x = afterToX, y = afterToX }
        }


expandVerticalFormat : VerticalLineFormat -> BaseFormat
expandVerticalFormat { afterLetter, afterToY } =
    if afterToY == Spaces 0 then
        { afterLetter = afterLetter
        , afterTo = { x = Spaces 1, y = Spaces 0 }
        }

    else
        { afterLetter = afterLetter
        , afterTo = { x = afterToY, y = afterToY }
        }


expandSmoothCubicFormat : SmoothCubicCurveFormat -> CubicCurveFormat
expandSmoothCubicFormat { afterLetter, afterEndControl, afterTo } =
    { afterLetter = afterLetter
    , afterStartControl = afterEndControl
    , afterEndControl = afterEndControl
    , afterTo = afterTo
    }


expandSmoothQuadraticFormat : BaseFormat -> QuadraticCurveFormat
expandSmoothQuadraticFormat { afterLetter, afterTo } =
    if afterTo.y == Spaces 0 then
        { afterLetter = afterLetter
        , afterControl = { x = afterTo.x, y = Spaces 1 }
        , afterTo = afterTo
        }

    else
        { afterLetter = afterLetter
        , afterControl = afterTo
        , afterTo = afterTo
        }



------------------------
-- COMMAND PARAMETERS --
------------------------


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


{-| The type of a Command, describing its relevant parameters and format.
-}
type CommandType
    = MoveCommand BaseParameters BaseFormat
    | LineCommand BaseParameters BaseFormat
    | HorizontalLineCommand HorizontalLineParameters HorizontalLineFormat
    | VerticalLineCommand VerticalLineParameters VerticalLineFormat
    | CubicCurveCommand CubicCurveParameters CubicCurveFormat
    | SmoothCubicCurveCommand SmoothCubicCurveParameters SmoothCubicCurveFormat
    | QuadraticCurveCommand QuadraticCurveParameters QuadraticCurveFormat
    | SmoothQuadraticCurveCommand BaseParameters BaseFormat
    | ArcCommand ArcParameters ArcFormat
    | CloseCommand CloseFormat


{-| An instruction that defines the next Segment of the Path to be drawn.
-}
type alias Command =
    { relation : Relation
    , commandType : CommandType
    }



------------------------
-- SEGMENT PARAMETERS --
------------------------


type alias BaseSegmentParameters =
    { from : Point
    , to : Point
    }


type alias CubicCurveSegmentParameters =
    { startControl : Point
    , endControl : Point
    , from : Point
    , to : Point
    }


type alias QuadraticCurveSegmentParameters =
    { control : Point
    , from : Point
    , to : Point
    }


type alias ArcSegmentParameters =
    { radii : Point
    , angle : Float
    , size : ArcSize
    , rotation : ArcRotation
    , from : Point
    , to : Point
    }


{-| A Segment of a Path that includes the start and end Points of the Segment as
well as the relevant parameters (for Segments like curves and arcs), all with
absolute values.
-}
type Segment
    = MoveSegment BaseSegmentParameters
    | LineSegment BaseSegmentParameters
    | CubicCurveSegment CubicCurveSegmentParameters
    | QuadraticCurveSegment QuadraticCurveSegmentParameters
    | ArcSegment ArcSegmentParameters
    | CloseSegment BaseSegmentParameters


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
with an Element specified.
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


type alias UpdateBuilder =
    { selected : List Selection
    , offset : Point
    , lastEndPoint : Point
    , lastMovePoint : Point
    , lastControlPoint : Point
    , updatedComponents : List Component
    }


initUpdateBuilder : List Selection -> Point -> UpdateBuilder
initUpdateBuilder selected offset =
    { selected = selected
    , offset = offset
    , lastEndPoint = Point.zero
    , lastMovePoint = Point.zero
    , lastControlPoint = Point.zero
    , updatedComponents = []
    }


updateMove : Component -> BaseSegmentParameters -> Component
updateMove { command } params =
    let
        newCommand : Command
        newCommand =
            case ( command.relation, command.commandType ) of
                ( Absolute, MoveCommand _ format ) ->
                    { command
                        | commandType =
                            MoveCommand
                                { to = params.to }
                                format
                    }

                ( Relative, MoveCommand _ format ) ->
                    { command
                        | commandType =
                            MoveCommand
                                { to = Point.subtract params.to params.from }
                                format
                    }

                _ ->
                    command
    in
    { command = newCommand
    , segment = MoveSegment params
    }


updateLine : Component -> BaseSegmentParameters -> Component
updateLine { command } params =
    let
        newOffset : Point
        newOffset =
            Point.subtract params.to params.from

        newCommand : Command
        newCommand =
            case ( command.relation, command.commandType ) of
                ( Absolute, LineCommand _ format ) ->
                    { command
                        | commandType =
                            LineCommand
                                { to = params.to }
                                format
                    }

                ( Relative, LineCommand _ format ) ->
                    { command
                        | commandType =
                            LineCommand
                                { to = newOffset }
                                format
                    }

                ( Absolute, HorizontalLineCommand _ format ) ->
                    if newOffset.y == 0 then
                        { command
                            | commandType =
                                HorizontalLineCommand
                                    { toX = params.to.x }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = params.to }
                                    (expandHorizontalFormat format)
                        }

                ( Relative, HorizontalLineCommand _ format ) ->
                    if newOffset.y == 0 then
                        { command
                            | commandType =
                                HorizontalLineCommand
                                    { toX = newOffset.x }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = newOffset }
                                    (expandHorizontalFormat format)
                        }

                ( Absolute, VerticalLineCommand _ format ) ->
                    if newOffset.x == 0 then
                        { command
                            | commandType =
                                VerticalLineCommand
                                    { toY = params.to.y }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = params.to }
                                    (expandVerticalFormat format)
                        }

                ( Relative, VerticalLineCommand _ format ) ->
                    if newOffset.x == 0 then
                        { command
                            | commandType =
                                VerticalLineCommand
                                    { toY = newOffset.y }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = newOffset }
                                    (expandVerticalFormat format)
                        }

                _ ->
                    command
    in
    { command = newCommand
    , segment = LineSegment params
    }


type alias UpdateCubicCurveParameters =
    { from : Point
    , startControl : Point
    , endControl : Point
    , to : Point
    , isSmooth : Bool
    }


updateCubicCurve : Component -> UpdateCubicCurveParameters -> Component
updateCubicCurve { command } params =
    let
        newStartControlOffset : Point
        newStartControlOffset =
            Point.subtract params.startControl params.from

        newEndControlOffset : Point
        newEndControlOffset =
            Point.subtract params.endControl params.from

        newOffset : Point
        newOffset =
            Point.subtract params.to params.from

        newCommand : Command
        newCommand =
            case ( command.relation, command.commandType ) of
                ( Absolute, CubicCurveCommand _ format ) ->
                    { command
                        | commandType =
                            CubicCurveCommand
                                { startControl = params.startControl
                                , endControl = params.endControl
                                , to = params.to
                                }
                                format
                    }

                ( Relative, CubicCurveCommand _ format ) ->
                    { command
                        | commandType =
                            CubicCurveCommand
                                { startControl = newStartControlOffset
                                , endControl = newEndControlOffset
                                , to = newOffset
                                }
                                format
                    }

                ( Absolute, SmoothCubicCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothCubicCurveCommand
                                    { endControl = params.endControl
                                    , to = params.to
                                    }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                CubicCurveCommand
                                    { startControl = params.startControl
                                    , endControl = params.endControl
                                    , to = params.to
                                    }
                                    (expandSmoothCubicFormat format)
                        }

                ( Relative, SmoothCubicCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothCubicCurveCommand
                                    { endControl = newEndControlOffset
                                    , to = newOffset
                                    }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                CubicCurveCommand
                                    { startControl = newStartControlOffset
                                    , endControl = newEndControlOffset
                                    , to = newOffset
                                    }
                                    (expandSmoothCubicFormat format)
                        }

                _ ->
                    command
    in
    { command = newCommand
    , segment =
        CubicCurveSegment
            { startControl = params.startControl
            , endControl = params.endControl
            , from = params.from
            , to = params.to
            }
    }


type alias UpdateQuadraticCurveParameters =
    { control : Point
    , from : Point
    , to : Point
    , isSmooth : Bool
    }


updateQuadraticCurve : Component -> UpdateQuadraticCurveParameters -> Component
updateQuadraticCurve { command } params =
    let
        newControlOffset : Point
        newControlOffset =
            Point.subtract params.control params.from

        newOffset : Point
        newOffset =
            Point.subtract params.to params.from

        newCommand : Command
        newCommand =
            case ( command.relation, command.commandType ) of
                ( Absolute, QuadraticCurveCommand _ format ) ->
                    { command
                        | commandType =
                            QuadraticCurveCommand
                                { control = params.control
                                , to = params.to
                                }
                                format
                    }

                ( Relative, QuadraticCurveCommand _ format ) ->
                    { command
                        | commandType =
                            QuadraticCurveCommand
                                { control = newControlOffset
                                , to = newOffset
                                }
                                format
                    }

                ( Absolute, SmoothQuadraticCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothQuadraticCurveCommand
                                    { to = params.to }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                QuadraticCurveCommand
                                    { control = params.control
                                    , to = params.to
                                    }
                                    (expandSmoothQuadraticFormat format)
                        }

                ( Relative, SmoothQuadraticCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothQuadraticCurveCommand
                                    { to = newOffset }
                                    format
                        }

                    else
                        { command
                            | commandType =
                                QuadraticCurveCommand
                                    { control = newControlOffset
                                    , to = newOffset
                                    }
                                    (expandSmoothQuadraticFormat format)
                        }

                _ ->
                    command
    in
    { command = newCommand
    , segment =
        QuadraticCurveSegment
            { control = params.control
            , from = params.from
            , to = params.to
            }
    }


updateArc : Component -> ArcSegmentParameters -> Component
updateArc { command } params =
    let
        newCommand : Command
        newCommand =
            case ( command.relation, command.commandType ) of
                ( Absolute, ArcCommand _ format ) ->
                    { command
                        | commandType =
                            ArcCommand
                                { radii = params.radii
                                , angle = params.angle
                                , size = params.size
                                , rotation = params.rotation
                                , to = params.to
                                }
                                format
                    }

                ( Relative, ArcCommand _ format ) ->
                    { command
                        | commandType =
                            ArcCommand
                                { radii = params.radii
                                , angle = params.angle
                                , size = params.size
                                , rotation = params.rotation
                                , to = Point.subtract params.to params.from
                                }
                                format
                    }

                _ ->
                    command
    in
    { command = newCommand
    , segment = ArcSegment params
    }


updateComponent : ( Int, Component ) -> UpdateBuilder -> UpdateBuilder
updateComponent ( index, component ) builder =
    let
        updatingEndPoint : Bool
        updatingEndPoint =
            List.member
                { index = index, element = EndPoint }
                builder.selected

        updatingStartControl : Bool
        updatingStartControl =
            List.member
                { index = index, element = StartControl }
                builder.selected

        updatingEndControl : Bool
        updatingEndControl =
            List.member
                { index = index, element = EndControl }
                builder.selected

        updatingControl : Bool
        updatingControl =
            List.member
                { index = index, element = Control }
                builder.selected
    in
    case component.segment of
        MoveSegment params ->
            let
                newParams : BaseSegmentParameters
                newParams =
                    { from = builder.lastEndPoint
                    , to =
                        if updatingEndPoint then
                            Point.add params.to builder.offset

                        else
                            params.to
                    }

                updatedComponent : Component
                updatedComponent =
                    updateMove component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastMovePoint = newParams.to
                , lastControlPoint = newParams.to
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }

        LineSegment params ->
            let
                newParams : BaseSegmentParameters
                newParams =
                    { from = builder.lastEndPoint
                    , to =
                        if updatingEndPoint then
                            Point.add params.to builder.offset

                        else
                            params.to
                    }

                updatedComponent : Component
                updatedComponent =
                    updateLine component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.to
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }

        CubicCurveSegment params ->
            let
                newParams : UpdateCubicCurveParameters
                newParams =
                    { startControl =
                        if updatingStartControl then
                            Point.add params.startControl builder.offset

                        else
                            params.startControl
                    , endControl =
                        if updatingEndControl then
                            Point.add params.endControl builder.offset

                        else
                            params.endControl
                    , from = builder.lastEndPoint
                    , to =
                        if updatingEndPoint then
                            Point.add params.to builder.offset

                        else
                            params.to
                    , isSmooth =
                        Point.isReflectionOver
                            params.from
                            params.startControl
                            builder.lastControlPoint
                    }

                updatedComponent : Component
                updatedComponent =
                    updateCubicCurve component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.endControl
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }

        QuadraticCurveSegment params ->
            let
                newParams : UpdateQuadraticCurveParameters
                newParams =
                    { control =
                        if updatingControl then
                            Point.add params.control builder.offset

                        else
                            params.control
                    , from = builder.lastEndPoint
                    , to =
                        if updatingEndPoint then
                            Point.add params.to builder.offset

                        else
                            params.to
                    , isSmooth =
                        Point.isReflectionOver
                            params.from
                            params.control
                            builder.lastControlPoint
                    }

                updatedComponent : Component
                updatedComponent =
                    updateQuadraticCurve component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.control
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }

        ArcSegment params ->
            let
                newParams : ArcSegmentParameters
                newParams =
                    { radii = params.radii
                    , angle = params.angle
                    , size = params.size
                    , rotation = params.rotation
                    , from = builder.lastEndPoint
                    , to =
                        if updatingEndPoint then
                            Point.add params.to builder.offset

                        else
                            params.to
                    }

                updatedComponent : Component
                updatedComponent =
                    updateArc component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.to
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }

        CloseSegment _ ->
            let
                updatedComponent : Component
                updatedComponent =
                    { component
                        | segment =
                            CloseSegment
                                { from = builder.lastEndPoint
                                , to = builder.lastMovePoint
                                }
                    }
            in
            { builder
                | lastEndPoint = builder.lastMovePoint
                , lastControlPoint = builder.lastMovePoint
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }


update : Path -> Point -> Path
update path offset =
    let
        updatedBuilder : UpdateBuilder
        updatedBuilder =
            List.foldl
                updateComponent
                (initUpdateBuilder path.selected offset)
                (List.indexedMap Tuple.pair path.components)

        updatedComponents : List Component
        updatedComponents =
            List.reverse updatedBuilder.updatedComponents
    in
    { path | components = updatedComponents }



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
