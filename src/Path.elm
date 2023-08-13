module Path exposing (..)

import Point exposing (Point, withinBounds)



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


defaultSeparator : Separator
defaultSeparator =
    Spaces 1


defaultPointSeparator : PointSeparator
defaultPointSeparator =
    { x = defaultSeparator, y = defaultSeparator }


expandHorizontalFormat : HorizontalLineFormat -> BaseFormat
expandHorizontalFormat { afterLetter, afterToX } =
    { afterLetter = afterLetter
    , afterTo =
        if afterToX == Spaces 0 then
            { x = defaultSeparator, y = Spaces 0 }

        else
            { x = afterToX, y = afterToX }
    }


expandVerticalFormat : VerticalLineFormat -> BaseFormat
expandVerticalFormat { afterLetter, afterToY } =
    { afterLetter = afterLetter
    , afterTo =
        if afterToY == Spaces 0 then
            { x = defaultSeparator, y = Spaces 0 }

        else
            { x = afterToY, y = afterToY }
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
    { afterLetter = afterLetter
    , afterControl =
        if afterTo.y == Spaces 0 then
            { x = afterTo.x, y = defaultSeparator }

        else
            afterTo
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


preFormattedMove : BaseParameters -> Command
preFormattedMove params =
    { relation = Absolute
    , commandType =
        MoveCommand
            params
            { afterLetter = defaultSeparator
            , afterTo = defaultPointSeparator
            }
    }


preFormattedLine : BaseParameters -> Command
preFormattedLine params =
    { relation = Absolute
    , commandType =
        LineCommand
            params
            { afterLetter = defaultSeparator
            , afterTo = defaultPointSeparator
            }
    }


preFormattedHorizontalLine : HorizontalLineParameters -> Command
preFormattedHorizontalLine params =
    { relation = Absolute
    , commandType =
        HorizontalLineCommand
            params
            { afterLetter = defaultSeparator
            , afterToX = defaultSeparator
            }
    }


preFormattedVerticalLine : VerticalLineParameters -> Command
preFormattedVerticalLine params =
    { relation = Absolute
    , commandType =
        VerticalLineCommand
            params
            { afterLetter = defaultSeparator
            , afterToY = defaultSeparator
            }
    }


preFormattedCubicCurve : CubicCurveParameters -> Command
preFormattedCubicCurve params =
    { relation = Absolute
    , commandType =
        CubicCurveCommand
            params
            { afterLetter = defaultSeparator
            , afterStartControl = defaultPointSeparator
            , afterEndControl = defaultPointSeparator
            , afterTo = defaultPointSeparator
            }
    }


preFormattedSmoothCubicCurve : SmoothCubicCurveParameters -> Command
preFormattedSmoothCubicCurve params =
    { relation = Absolute
    , commandType =
        SmoothCubicCurveCommand
            params
            { afterLetter = defaultSeparator
            , afterEndControl = defaultPointSeparator
            , afterTo = defaultPointSeparator
            }
    }


preFormattedQuadraticCurve : QuadraticCurveParameters -> Command
preFormattedQuadraticCurve params =
    { relation = Absolute
    , commandType =
        QuadraticCurveCommand
            params
            { afterLetter = defaultSeparator
            , afterControl = defaultPointSeparator
            , afterTo = defaultPointSeparator
            }
    }


preFormattedSmoothQuadraticCurve : BaseParameters -> Command
preFormattedSmoothQuadraticCurve params =
    { relation = Absolute
    , commandType =
        SmoothQuadraticCurveCommand
            params
            { afterLetter = defaultSeparator
            , afterTo = defaultPointSeparator
            }
    }


preFormattedArc : ArcParameters -> Command
preFormattedArc params =
    { relation = Absolute
    , commandType =
        ArcCommand
            params
            { afterLetter = defaultSeparator
            , afterRadii = defaultPointSeparator
            , afterAngle = defaultSeparator
            , afterSize = defaultSeparator
            , afterRotation = defaultSeparator
            , afterTo = defaultPointSeparator
            }
    }


preFormattedClose : Command
preFormattedClose =
    { relation = Absolute
    , commandType = CloseCommand { afterLetter = defaultSeparator }
    }


componentPoints : Component -> List Point
componentPoints component =
    case component.segment of
        MoveSegment { to } ->
            [ to ]

        LineSegment { to } ->
            [ to ]

        CubicCurveSegment { startControl, endControl, to } ->
            [ startControl, endControl, to ]

        QuadraticCurveSegment { control, to } ->
            [ control, to ]

        ArcSegment { to } ->
            [ to ]

        CloseSegment { to } ->
            [ to ]


selectionsWithin : Point -> Point -> Path -> List Selection
selectionsWithin bounds1 bounds2 path =
    List.concatMap
        (\( index, component ) ->
            componentSelectionsWithin bounds1 bounds2 index component
        )
        (List.indexedMap Tuple.pair path.components)


{-| Returns a list of Selections for a given Component that are within a
bounding box described by two Points.
-}
componentSelectionsWithin : Point -> Point -> Int -> Component -> List Selection
componentSelectionsWithin bounds1 bounds2 index component =
    let
        withinBounds : Point -> Bool
        withinBounds point =
            Point.withinBounds point bounds1 bounds2
    in
    case component.segment of
        MoveSegment { to } ->
            if withinBounds to then
                [ { index = index, element = EndPoint } ]

            else
                []

        LineSegment { to } ->
            if withinBounds to then
                [ { index = index, element = EndPoint } ]

            else
                []

        CubicCurveSegment { startControl, endControl, to } ->
            List.concat
                [ if withinBounds startControl then
                    [ { index = index, element = StartControl } ]

                  else
                    []
                , if withinBounds endControl then
                    [ { index = index, element = EndControl } ]

                  else
                    []
                , if withinBounds to then
                    [ { index = index, element = EndPoint } ]

                  else
                    []
                ]

        QuadraticCurveSegment { control, to } ->
            List.concat
                [ if withinBounds control then
                    [ { index = index, element = Control } ]

                  else
                    []
                , if withinBounds to then
                    [ { index = index, element = EndPoint } ]

                  else
                    []
                ]

        ArcSegment { to } ->
            if withinBounds to then
                [ { index = index, element = EndPoint } ]

            else
                []

        CloseSegment { to } ->
            if withinBounds to then
                [ { index = index, element = EndPoint } ]

            else
                []



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
                        { startControl =
                            Point.reflectOver
                                currentPoint
                                previousControl
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
                        { control =
                            Point.reflectOver
                                currentPoint
                                previousControl
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


addSelection : Path -> Selection -> Path
addSelection path selection =
    if List.member selection path.selected then
        path

    else
        { path | selected = selection :: path.selected }


removeSelection : Path -> Selection -> Path
removeSelection path selection =
    let
        removedFromSelected : List Selection
        removedFromSelected =
            List.filter (\s -> s /= selection) path.selected
    in
    { path | selected = removedFromSelected }


toggleSelection : Path -> Selection -> Path
toggleSelection path selection =
    if List.member selection path.selected then
        removeSelection path selection

    else
        addSelection path selection


defaultLineCommand : Relation -> Command
defaultLineCommand relation =
    { relation = relation
    , commandType =
        LineCommand
            { to = Point.zero }
            { afterLetter = Spaces 0, afterTo = defaultPointSeparator }
    }


defaultCubicCurveCommand : Relation -> Command
defaultCubicCurveCommand relation =
    { relation = relation
    , commandType =
        CubicCurveCommand
            { to = Point.zero
            , startControl = Point.zero
            , endControl = Point.zero
            }
            { afterLetter = Spaces 0
            , afterStartControl = defaultPointSeparator
            , afterEndControl = defaultPointSeparator
            , afterTo = defaultPointSeparator
            }
    }


defaultQuadraticCurveCommand : Relation -> Command
defaultQuadraticCurveCommand relation =
    { relation = relation
    , commandType =
        QuadraticCurveCommand
            { to = Point.zero, control = Point.zero }
            { afterLetter = Spaces 0
            , afterControl = defaultPointSeparator
            , afterTo = defaultPointSeparator
            }
    }


{-| Determines whether thisCommand is in sequence with lastCommand. In other
words, whether thisCommand is the same type of Command and relation as
lastCommand. This is used when expanding the shorthand commands to determine
whether the afterLetter separator should be changed from `NoLetter`. If a
shorthand command is expanded, has a NoLetter separator (AKA is part of an
implicit sequence), and is not in sequence with the previous command, then the
afterLetter separator should be changed so that the String representation
of the expanded command is correct.
-}
commandInSequence : Command -> Command -> Bool
commandInSequence lastCommand thisCommand =
    let
        sameRelation : Bool
        sameRelation =
            lastCommand.relation == thisCommand.relation
    in
    case ( lastCommand.commandType, thisCommand.commandType ) of
        ( MoveCommand _ _, MoveCommand _ _ ) ->
            sameRelation

        ( MoveCommand _ _, LineCommand _ _ ) ->
            sameRelation

        ( LineCommand _ _, LineCommand _ _ ) ->
            sameRelation

        ( HorizontalLineCommand _ _, HorizontalLineCommand _ _ ) ->
            sameRelation

        ( VerticalLineCommand _ _, VerticalLineCommand _ _ ) ->
            sameRelation

        ( CubicCurveCommand _ _, CubicCurveCommand _ _ ) ->
            sameRelation

        ( SmoothCubicCurveCommand _ _, SmoothCubicCurveCommand _ _ ) ->
            sameRelation

        ( QuadraticCurveCommand _ _, QuadraticCurveCommand _ _ ) ->
            sameRelation

        ( SmoothQuadraticCurveCommand _ _, SmoothQuadraticCurveCommand _ _ ) ->
            sameRelation

        ( ArcCommand _ _, ArcCommand _ _ ) ->
            sameRelation

        ( CloseCommand _, CloseCommand _ ) ->
            sameRelation

        ( _, _ ) ->
            False


type alias UpdateBuilder =
    { selected : List Selection
    , offset : Point
    , lastEndPoint : Point
    , lastMovePoint : Point
    , lastControlPoint : Point
    , lastCommand : Command
    , updatedComponents : List Component
    }


initUpdateBuilder : List Selection -> Point -> UpdateBuilder
initUpdateBuilder selected offset =
    { selected = selected
    , offset = offset
    , lastEndPoint = Point.zero
    , lastMovePoint = Point.zero
    , lastControlPoint = Point.zero
    , lastCommand =
        { relation = Absolute
        , commandType = CloseCommand { afterLetter = NoLetter }
        }
    , updatedComponents = []
    }


type alias UpdateBaseParameters =
    { from : Point
    , to : Point
    , lastCommand : Command
    }


type alias UpdateCubicCurveParameters =
    { startControl : Point
    , endControl : Point
    , from : Point
    , to : Point
    , isSmooth : Bool
    , lastCommand : Command
    }


type alias UpdateQuadraticCurveParameters =
    { control : Point
    , from : Point
    , to : Point
    , isSmooth : Bool
    , lastCommand : Command
    }


type alias UpdateArcParameters =
    { radii : Point
    , angle : Float
    , size : ArcSize
    , rotation : ArcRotation
    , from : Point
    , to : Point
    , lastCommand : Command
    }


type alias AnyFormat r =
    { r | afterLetter : Separator }


updateFormat : AnyFormat r -> Bool -> AnyFormat r
updateFormat format inSequence =
    if not inSequence && format.afterLetter == NoLetter then
        { format | afterLetter = defaultSeparator }

    else
        format


updateMove : Component -> UpdateBaseParameters -> Component
updateMove { command } params =
    let
        inSequence : Bool
        inSequence =
            commandInSequence params.lastCommand command

        newCommand : Command
        newCommand =
            case ( command.relation, command.commandType ) of
                ( Absolute, MoveCommand _ format ) ->
                    { command
                        | commandType =
                            MoveCommand
                                { to = params.to }
                                (updateFormat format inSequence)
                    }

                ( Relative, MoveCommand _ format ) ->
                    { command
                        | commandType =
                            MoveCommand
                                { to = Point.subtract params.to params.from }
                                (updateFormat format inSequence)
                    }

                _ ->
                    command
    in
    { command = newCommand
    , segment = MoveSegment { to = params.to, from = params.from }
    }


updateLine : Component -> UpdateBaseParameters -> Component
updateLine { command } params =
    let
        newOffset : Point
        newOffset =
            Point.subtract params.to params.from

        inSequence : Bool
        inSequence =
            commandInSequence params.lastCommand command

        inSequenceExpansion : Bool
        inSequenceExpansion =
            commandInSequence
                params.lastCommand
                (defaultLineCommand command.relation)

        newCommand : Command
        newCommand =
            case ( command.relation, command.commandType ) of
                ( Absolute, LineCommand _ format ) ->
                    { command
                        | commandType =
                            LineCommand
                                { to = params.to }
                                (updateFormat format inSequence)
                    }

                ( Relative, LineCommand _ format ) ->
                    { command
                        | commandType =
                            LineCommand
                                { to = newOffset }
                                (updateFormat format inSequence)
                    }

                ( Absolute, HorizontalLineCommand _ format ) ->
                    if newOffset.y == 0 then
                        { command
                            | commandType =
                                HorizontalLineCommand
                                    { toX = params.to.x }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = params.to }
                                    (updateFormat format inSequenceExpansion
                                        |> expandHorizontalFormat
                                    )
                        }

                ( Relative, HorizontalLineCommand _ format ) ->
                    if newOffset.y == 0 then
                        { command
                            | commandType =
                                HorizontalLineCommand
                                    { toX = newOffset.x }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = newOffset }
                                    (updateFormat format inSequenceExpansion
                                        |> expandHorizontalFormat
                                    )
                        }

                ( Absolute, VerticalLineCommand _ format ) ->
                    if newOffset.x == 0 then
                        { command
                            | commandType =
                                VerticalLineCommand
                                    { toY = params.to.y }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = params.to }
                                    (updateFormat format inSequenceExpansion
                                        |> expandVerticalFormat
                                    )
                        }

                ( Relative, VerticalLineCommand _ format ) ->
                    if newOffset.x == 0 then
                        { command
                            | commandType =
                                VerticalLineCommand
                                    { toY = newOffset.y }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                LineCommand
                                    { to = newOffset }
                                    (updateFormat format inSequenceExpansion
                                        |> expandVerticalFormat
                                    )
                        }

                _ ->
                    command
    in
    { command = newCommand
    , segment = LineSegment { to = params.to, from = params.from }
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

        inSequence : Bool
        inSequence =
            commandInSequence params.lastCommand command

        inSequenceExpansion : Bool
        inSequenceExpansion =
            commandInSequence
                params.lastCommand
                (defaultCubicCurveCommand command.relation)

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
                                (updateFormat format inSequence)
                    }

                ( Relative, CubicCurveCommand _ format ) ->
                    { command
                        | commandType =
                            CubicCurveCommand
                                { startControl = newStartControlOffset
                                , endControl = newEndControlOffset
                                , to = newOffset
                                }
                                (updateFormat format inSequence)
                    }

                ( Absolute, SmoothCubicCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothCubicCurveCommand
                                    { endControl = params.endControl
                                    , to = params.to
                                    }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                CubicCurveCommand
                                    { startControl = params.startControl
                                    , endControl = params.endControl
                                    , to = params.to
                                    }
                                    (updateFormat format inSequenceExpansion
                                        |> expandSmoothCubicFormat
                                    )
                        }

                ( Relative, SmoothCubicCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothCubicCurveCommand
                                    { endControl = newEndControlOffset
                                    , to = newOffset
                                    }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                CubicCurveCommand
                                    { startControl = newStartControlOffset
                                    , endControl = newEndControlOffset
                                    , to = newOffset
                                    }
                                    (updateFormat format inSequenceExpansion
                                        |> expandSmoothCubicFormat
                                    )
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


updateQuadraticCurve : Component -> UpdateQuadraticCurveParameters -> Component
updateQuadraticCurve { command } params =
    let
        newControlOffset : Point
        newControlOffset =
            Point.subtract params.control params.from

        newOffset : Point
        newOffset =
            Point.subtract params.to params.from

        inSequence : Bool
        inSequence =
            commandInSequence params.lastCommand command

        inSequenceExpansion : Bool
        inSequenceExpansion =
            commandInSequence
                params.lastCommand
                (defaultCubicCurveCommand command.relation)

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
                                (updateFormat format inSequence)
                    }

                ( Relative, QuadraticCurveCommand _ format ) ->
                    { command
                        | commandType =
                            QuadraticCurveCommand
                                { control = newControlOffset
                                , to = newOffset
                                }
                                (updateFormat format inSequence)
                    }

                ( Absolute, SmoothQuadraticCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothQuadraticCurveCommand
                                    { to = params.to }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                QuadraticCurveCommand
                                    { control = params.control
                                    , to = params.to
                                    }
                                    (updateFormat format inSequenceExpansion
                                        |> expandSmoothQuadraticFormat
                                    )
                        }

                ( Relative, SmoothQuadraticCurveCommand _ format ) ->
                    if params.isSmooth then
                        { command
                            | commandType =
                                SmoothQuadraticCurveCommand
                                    { to = newOffset }
                                    (updateFormat format inSequence)
                        }

                    else
                        { command
                            | commandType =
                                QuadraticCurveCommand
                                    { control = newControlOffset
                                    , to = newOffset
                                    }
                                    (updateFormat format inSequenceExpansion
                                        |> expandSmoothQuadraticFormat
                                    )
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


updateArc : Component -> UpdateArcParameters -> Component
updateArc { command } params =
    let
        inSequence : Bool
        inSequence =
            commandInSequence params.lastCommand command

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
                                (updateFormat format inSequence)
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
                                (updateFormat format inSequence)
                    }

                _ ->
                    command
    in
    { command = newCommand
    , segment =
        ArcSegment
            { radii = params.radii
            , angle = params.angle
            , size = params.size
            , rotation = params.rotation
            , from = params.from
            , to = params.to
            }
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
                newParams : UpdateBaseParameters
                newParams =
                    { from = builder.lastEndPoint
                    , to =
                        if updatingEndPoint then
                            Point.add params.to builder.offset

                        else
                            params.to
                    , lastCommand = builder.lastCommand
                    }

                updatedComponent : Component
                updatedComponent =
                    updateMove component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastMovePoint = newParams.to
                , lastControlPoint = newParams.to
                , lastCommand = updatedComponent.command
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }

        LineSegment params ->
            let
                newParams : UpdateBaseParameters
                newParams =
                    { from = builder.lastEndPoint
                    , to =
                        if updatingEndPoint then
                            Point.add params.to builder.offset

                        else
                            params.to
                    , lastCommand = builder.lastCommand
                    }

                updatedComponent : Component
                updatedComponent =
                    updateLine component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.to
                , lastCommand = updatedComponent.command
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
                    , lastCommand = builder.lastCommand
                    }

                updatedComponent : Component
                updatedComponent =
                    updateCubicCurve component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.endControl
                , lastCommand = updatedComponent.command
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
                    , lastCommand = builder.lastCommand
                    }

                updatedComponent : Component
                updatedComponent =
                    updateQuadraticCurve component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.control
                , lastCommand = updatedComponent.command
                , updatedComponents =
                    updatedComponent :: builder.updatedComponents
            }

        ArcSegment params ->
            let
                newParams : UpdateArcParameters
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
                    , lastCommand = builder.lastCommand
                    }

                updatedComponent : Component
                updatedComponent =
                    updateArc component newParams
            in
            { builder
                | lastEndPoint = newParams.to
                , lastControlPoint = newParams.to
                , lastCommand = updatedComponent.command
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
                , lastCommand = updatedComponent.command
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


updateWithSelection : Path -> Point -> Selection -> Path
updateWithSelection path offset selection =
    removeSelection (update (addSelection path selection) offset) selection


type alias EndState =
    { endPoint : Point
    , movePoint : Point
    , endControlPoint : Point
    , controlPoint : Point
    }


initEndState : EndState
initEndState =
    { endPoint = Point.zero
    , movePoint = Point.zero
    , endControlPoint = Point.zero
    , controlPoint = Point.zero
    }


componentEndState : Component -> EndState -> EndState
componentEndState component endState =
    case component.segment of
        MoveSegment params ->
            { endState
                | endPoint = params.to
                , movePoint = params.to
                , endControlPoint = params.to
                , controlPoint = params.to
            }

        LineSegment params ->
            { endState
                | endPoint = params.to
                , endControlPoint = params.to
                , controlPoint = params.to
            }

        CubicCurveSegment params ->
            { endState
                | endPoint = params.to
                , endControlPoint = params.endControl
                , controlPoint = params.to
            }

        QuadraticCurveSegment params ->
            { endState
                | endPoint = params.to
                , endControlPoint = params.to
                , controlPoint = params.control
            }

        ArcSegment params ->
            { endState
                | endPoint = params.to
                , endControlPoint = params.to
                , controlPoint = params.to
            }

        CloseSegment _ ->
            { endState
                | endPoint = endState.movePoint
                , endControlPoint = endState.movePoint
                , controlPoint = endState.movePoint
            }


toEndState : Path -> EndState
toEndState path =
    List.foldl componentEndState initEndState path.components


appendCommand : Path -> Command -> Path
appendCommand path command =
    let
        endState : EndState
        endState =
            toEndState path

        newSegment : Segment
        newSegment =
            case command.commandType of
                MoveCommand params _ ->
                    MoveSegment
                        { from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                params.to

                            else
                                Point.add endState.endPoint params.to
                        }

                LineCommand params _ ->
                    LineSegment
                        { from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                params.to

                            else
                                Point.add endState.endPoint params.to
                        }

                HorizontalLineCommand params _ ->
                    LineSegment
                        { from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                { x = params.toX
                                , y = endState.endPoint.y
                                }

                            else
                                { x = endState.endPoint.x + params.toX
                                , y = endState.endPoint.y
                                }
                        }

                VerticalLineCommand params _ ->
                    LineSegment
                        { from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                { x = endState.endPoint.x
                                , y = params.toY
                                }

                            else
                                { x = endState.endPoint.x
                                , y = endState.endPoint.y + params.toY
                                }
                        }

                CubicCurveCommand params _ ->
                    CubicCurveSegment
                        { startControl =
                            if command.relation == Absolute then
                                params.startControl

                            else
                                Point.add endState.endPoint params.startControl
                        , endControl =
                            if command.relation == Absolute then
                                params.endControl

                            else
                                Point.add endState.endPoint params.endControl
                        , from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                params.to

                            else
                                Point.add endState.endPoint params.to
                        }

                SmoothCubicCurveCommand params _ ->
                    CubicCurveSegment
                        { startControl =
                            Point.reflectOver
                                endState.endPoint
                                endState.endControlPoint
                        , endControl =
                            if command.relation == Absolute then
                                params.endControl

                            else
                                Point.add endState.endPoint params.endControl
                        , from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                params.to

                            else
                                Point.add endState.endPoint params.to
                        }

                QuadraticCurveCommand params _ ->
                    QuadraticCurveSegment
                        { control =
                            if command.relation == Absolute then
                                params.control

                            else
                                Point.add endState.endPoint params.control
                        , from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                params.to

                            else
                                Point.add endState.endPoint params.to
                        }

                SmoothQuadraticCurveCommand params _ ->
                    QuadraticCurveSegment
                        { control =
                            Point.reflectOver
                                endState.endPoint
                                endState.controlPoint
                        , from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                params.to

                            else
                                Point.add endState.endPoint params.to
                        }

                ArcCommand params _ ->
                    ArcSegment
                        { radii = params.radii
                        , angle = params.angle
                        , size = params.size
                        , rotation = params.rotation
                        , from = endState.endPoint
                        , to =
                            if command.relation == Absolute then
                                params.to

                            else
                                Point.add endState.endPoint params.to
                        }

                CloseCommand _ ->
                    CloseSegment
                        { from = endState.endPoint
                        , to = endState.movePoint
                        }

        newComponent : Component
        newComponent =
            { segment = newSegment
            , command = command
            }
    in
    { path | components = path.components ++ [ newComponent ] }



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


selectionToString : Selection -> String
selectionToString { index, element } =
    case element of
        EndPoint ->
            "EndPoint " ++ String.fromInt index

        StartControl ->
            "StartControl " ++ String.fromInt index

        EndControl ->
            "EndControl " ++ String.fromInt index

        Control ->
            "Control " ++ String.fromInt index

        Segment ->
            "Segment " ++ String.fromInt index
