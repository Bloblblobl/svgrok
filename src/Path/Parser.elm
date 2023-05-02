module Path.Parser exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Path
    exposing
        ( ArcRotation(..)
        , ArcSize(..)
        , CloseFormat
        , Command
        , CommandType(..)
        , Path
        , PointSeparator
        , Relation(..)
        , Separator(..)
        )
import Point exposing (Point)


{-| The result of parsing a substring of a command String. It is either a Valid
Command, in which case it will be included in the final Path, or an Invalid
String.
-}
type Result
    = Valid Command
    | Invalid String


{-| All the CommandTypes that have parameters (everything but the Close
command).
-}
type ParameterizedCommandType
    = Move
    | Line
    | HorizontalLine
    | VerticalLine
    | CubicCurve
    | SmoothCubicCurve
    | QuadraticCurve
    | SmoothQuadraticCurve
    | Arc


{-| The State that the Builder is in.

  - In the Chomping State, the Builder builds up a String of chomped invalid
    characters until it finds a valid command letter. The Builder will enter one of
    its other States if it encounters a valid command letter, and store the
    chomped String as an Invalid Result.

  - In the ParsingParameterizedCommandType State, the Builder parses sets of
    parameters corresponding to a ParameterizedCommandType. Sequential sets of
    parameters are parsed into Valid Results. If the Builder encounters a
    command letter, and it has already parsed one set of parameters, the current
    State's commandType will switch to the corresponding command letter's type
    (unless the letter is "Z"/"z", in which case it will switch to the
    ParsedClose State). If it hasn't parsed one, or otherwise fails to parse a
    set of parameters at any point, it will enter the Chomping State. The
    Relation of the command sequence that is currently being parsed is
    determined by the case of the command letter that caused the Builder to
    enter this State (lowercase -> Relative, uppercase -> Absolute).

  - In the ParsedClose State, the Builder only attempts to parse command
    letters. If it fails to find one, it will enter the Chomping State.

-}
type State
    = Chomping String
    | ParsingParameterizedCommandType
        { commandType : ParameterizedCommandType
        , relation : Relation
        , parsedOne : Bool
        }
    | ParsedClose


{-| A data structure that stores the Results of parsing the command String along
with the current State of the parsing process.
-}
type alias Builder =
    { results : List Result
    , state : State
    }


initBuilder : Builder
initBuilder =
    { results = [], state = Chomping "" }


{-| A List of tuples mapping uppercase command letters to State builder
functions (which map a Relation to a State). Used by parameterizedCommandLetters
to build up parsers for all parameterized Command letters.
-}
commandLetterToStateBuilder : List ( String, Relation -> State )
commandLetterToStateBuilder =
    let
        paramStateBuilder : ParameterizedCommandType -> Relation -> State
        paramStateBuilder commandType relation =
            ParsingParameterizedCommandType
                { commandType = commandType
                , relation = relation
                , parsedOne = False
                }
    in
    [ ( "M", paramStateBuilder Move )
    , ( "L", paramStateBuilder Line )
    , ( "H", paramStateBuilder HorizontalLine )
    , ( "V", paramStateBuilder VerticalLine )
    , ( "C", paramStateBuilder CubicCurve )
    , ( "S", paramStateBuilder SmoothCubicCurve )
    , ( "Q", paramStateBuilder QuadraticCurve )
    , ( "T", paramStateBuilder SmoothQuadraticCurve )
    , ( "A", paramStateBuilder Arc )
    ]



-------------
-- PARSERS --
-------------


{-| A parsed float value along with the separator that follows it.
-}
type alias FormattedFloat =
    { value : Float
    , afterValue : Separator
    }


getFloat : FormattedFloat -> Float
getFloat { value } =
    value


getFloatSeparator : FormattedFloat -> Separator
getFloatSeparator { afterValue } =
    afterValue


{-| Two parsed Floats representing the x and y values of a Point along with the
respective separators that follow each value.
-}
type alias FormattedPoint =
    { x : Float
    , afterX : Separator
    , y : Float
    , afterY : Separator
    }


getPoint : FormattedPoint -> Point
getPoint { x, y } =
    { x = x, y = y }


getPointSeparator : FormattedPoint -> PointSeparator
getPointSeparator { afterX, afterY } =
    { x = afterX, y = afterY }


{-| A parsed ArcSize along with the separator that follows it.
-}
type alias FormattedArcSize =
    { size : ArcSize
    , afterSize : Separator
    }


getArcSize : FormattedArcSize -> ArcSize
getArcSize { size } =
    size


getArcSizeSeparator : FormattedArcSize -> Separator
getArcSizeSeparator { afterSize } =
    afterSize


{-| A parsed ArcRotation along with the separator that follows it.
-}
type alias FormattedArcRotation =
    { rotation : ArcRotation
    , afterRotation : Separator
    }


getArcRotation : FormattedArcRotation -> ArcRotation
getArcRotation { rotation } =
    rotation


getArcRotationSeparator : FormattedArcRotation -> Separator
getArcRotationSeparator { afterRotation } =
    afterRotation


{-| Chomps a character and adds it to the current chompedString.
-}
chompOne : String -> Builder -> Parser Builder
chompOne chompedString builder =
    P.getChompedString (P.chompIf (\_ -> True))
        |> P.map
            (\chompedChar ->
                { builder | state = Chomping (chompedString ++ chompedChar) }
            )


{-| Parser for a Float. The built-in Float parser doesn't account for negative
Floats.
-}
float : Parser Float
float =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.float
        , P.float
        ]


formattedFloat : Parser FormattedFloat
formattedFloat =
    P.succeed FormattedFloat
        |= float
        |= separator


formattedPoint : Parser FormattedPoint
formattedPoint =
    P.succeed FormattedPoint
        |= float
        |= separator
        |= float
        |= separator


{-| Parser for a Separator. Only accounts for a comma preceded and/or followed by
any number of spaces, or a sequence of any number of spaces. The `NoLetter`
special case is not parsed directly.
-}
separator : Parser Separator
separator =
    P.oneOf
        [ P.backtrackable <|
            P.succeed
                (\before1 after1 before2 after2 ->
                    Comma
                        { spacesBefore = after1 - before1
                        , spacesAfter = after2 - before2
                        }
                )
                |= P.getOffset
                |. P.spaces
                |= P.getOffset
                |. P.token ","
                |= P.getOffset
                |. P.spaces
                |= P.getOffset
        , P.succeed
            (\before after -> Spaces (after - before))
            |= P.getOffset
            |. P.spaces
            |= P.getOffset
        ]


formattedArcSize : Parser FormattedArcSize
formattedArcSize =
    P.succeed FormattedArcSize
        |= P.oneOf
            [ P.succeed Large
                |. P.symbol "1"
            , P.succeed Small
                |. P.symbol "0"
            ]
        |= separator


formattedArcRotation : Parser FormattedArcRotation
formattedArcRotation =
    P.succeed FormattedArcRotation
        |= P.oneOf
            [ P.succeed Clockwise
                |. P.symbol "1"
            , P.succeed CounterClockwise
                |. P.symbol "0"
            ]
        |= separator


{-| Parser that parses a command letter for a ParameterizedCommandType and
updates the Builder's State accordingly.
-}
parameterizedCommandLetters : Builder -> Parser Builder
parameterizedCommandLetters builder =
    let
        letterParser : ( String, Relation -> State ) -> Parser Builder
        letterParser ( letter, stateBuilder ) =
            P.oneOf
                [ P.succeed (stateBuilder Absolute)
                    |. P.token letter
                , P.succeed (stateBuilder Relative)
                    |. P.token (String.toLower letter)
                ]
                |> P.map (\state -> { builder | state = state })
    in
    P.oneOf (List.map letterParser commandLetterToStateBuilder)


{-| The parameters of a cubic curve which are always parsed regardless of
whether or not the command is part of the tail of an implicit sequence or not.
-}
type alias FormattedCubicCurve =
    { startControl : FormattedPoint
    , endControl : FormattedPoint
    , to : FormattedPoint
    }


{-| Parser for a FormattedCubicCurve. Parses a formatted end control Point, then
formatted to Point. The result is then paired with a separator to make a
complete CubicCurve.
-}
formattedCubicCurve : Parser FormattedCubicCurve
formattedCubicCurve =
    P.succeed FormattedCubicCurve
        |= formattedPoint
        |= formattedPoint
        |= formattedPoint


{-| The parameters of a smooth cubic curve which are always parsed regardless of
whether or not the command is part of the tail of an implicit sequence or not.
-}
type alias FormattedSmoothCubicCurve =
    { endControl : FormattedPoint
    , to : FormattedPoint
    }


{-| Parser for a FormattedSmoothCubicCurve. Parses a formatted end control
Point, then a formatted to Point. The result is then paired with a separator to
make a complete SmoothCubicCurve.
-}
formattedSmoothCubicCurve : Parser FormattedSmoothCubicCurve
formattedSmoothCubicCurve =
    P.succeed FormattedSmoothCubicCurve
        |= formattedPoint
        |= formattedPoint


{-| The parameters of a quadratic curve which are always parsed regardless of
whether or not the command is part of the tail of an implicit sequence or not.
-}
type alias FormattedQuadraticCurve =
    { control : FormattedPoint
    , to : FormattedPoint
    }


{-| Parser for a FormattedQuadraticCurve. Parses a formatted control Point, then
a formatted to Point. The result is then paired with a separator to make a
complete QuadraticCurveCommand.
-}
formattedQuadraticCurve : Parser FormattedQuadraticCurve
formattedQuadraticCurve =
    P.succeed FormattedQuadraticCurve
        |= formattedPoint
        |= formattedPoint


{-| The parameters of an arc which are always parsed regardless of whether or
not the command is part of the tail of an implicit sequence or not.
-}
type alias FormattedArc =
    { radii : FormattedPoint
    , angle : FormattedFloat
    , size : FormattedArcSize
    , rotation : FormattedArcRotation
    , to : FormattedPoint
    }


{-| Parser for a Formatted. Parses a formatted Point that represents the radii,
a formatted angle Float, a formatted ArcSize, a formatted ArcRotation, and a
formatted to Point. The result is then paired with a separator to make a
complete ArcCommand.
-}
formattedArc : Parser FormattedArc
formattedArc =
    P.succeed FormattedArc
        |= formattedPoint
        |= formattedFloat
        |= formattedArcSize
        |= formattedArcRotation
        |= formattedPoint


{-| Constructs a MoveCommand from a separator and a formatted to Point.
-}
makeMove : Separator -> FormattedPoint -> CommandType
makeMove sep formattedTo =
    MoveCommand
        { to = getPoint formattedTo }
        { afterLetter = sep
        , afterTo = getPointSeparator formattedTo
        }


{-| Constructs a LineCommand from a separator and a formatted to Point.
-}
makeLine : Separator -> FormattedPoint -> CommandType
makeLine sep formattedTo =
    LineCommand
        { to = getPoint formattedTo }
        { afterLetter = sep
        , afterTo = getPointSeparator formattedTo
        }


{-| Constructs a HorizontalLineCommand from a separator and a formatted toX
Float.
-}
makeHorizontalLine : Separator -> FormattedFloat -> CommandType
makeHorizontalLine sep formattedToX =
    HorizontalLineCommand
        { toX = getFloat formattedToX }
        { afterLetter = sep
        , afterToX = getFloatSeparator formattedToX
        }


{-| Constructs a VerticalLineCommand from a separator and a formatted toY Float.
-}
makeVerticalLine : Separator -> FormattedFloat -> CommandType
makeVerticalLine sep formattedToY =
    VerticalLineCommand
        { toY = getFloat formattedToY }
        { afterLetter = sep
        , afterToY = getFloatSeparator formattedToY
        }


{-| Constructs a CubicCurveCommand from a separator, a formatted start control
Point, a formatted end control Point, and a formatted to Point.
-}
makeCubicCurve : Separator -> FormattedCubicCurve -> CommandType
makeCubicCurve sep { startControl, endControl, to } =
    CubicCurveCommand
        { startControl = getPoint startControl
        , endControl = getPoint endControl
        , to = getPoint to
        }
        { afterLetter = sep
        , afterStartControl = getPointSeparator startControl
        , afterEndControl = getPointSeparator endControl
        , afterTo = getPointSeparator to
        }


{-| Constructs a SmoothCubicCurveCommand from a separator, a formatted end
control Point, and a formatted to Point.
-}
makeSmoothCubicCurve : Separator -> FormattedSmoothCubicCurve -> CommandType
makeSmoothCubicCurve sep { endControl, to } =
    SmoothCubicCurveCommand
        { endControl = getPoint endControl
        , to = getPoint to
        }
        { afterLetter = sep
        , afterEndControl = getPointSeparator endControl
        , afterTo = getPointSeparator to
        }


{-| Constructs a QuadraticCurveCommand from a separator, a formatted control
Point, and a formatted to Point.
-}
makeQuadraticCurve : Separator -> FormattedQuadraticCurve -> CommandType
makeQuadraticCurve sep { control, to } =
    QuadraticCurveCommand
        { control = getPoint control
        , to = getPoint to
        }
        { afterLetter = sep
        , afterControl = getPointSeparator control
        , afterTo = getPointSeparator to
        }


{-| Constructs a SmoothQuadraticCurveCommand from a separator and a formatted to
Point.
-}
makeSmoothQuadraticCurve : Separator -> FormattedPoint -> CommandType
makeSmoothQuadraticCurve sep formattedTo =
    SmoothQuadraticCurveCommand
        { to = getPoint formattedTo }
        { afterLetter = sep
        , afterTo = getPointSeparator formattedTo
        }


{-| Constructs a SmoothQuadraticCurveCommand from a separator and a formatted to
Point.
-}
makeArc : Separator -> FormattedArc -> CommandType
makeArc sep { radii, angle, size, rotation, to } =
    ArcCommand
        { radii = getPoint radii
        , angle = getFloat angle
        , size = getArcSize size
        , rotation = getArcRotation rotation
        , to = getPoint to
        }
        { afterLetter = sep
        , afterRadii = getPointSeparator radii
        , afterAngle = getFloatSeparator angle
        , afterSize = getArcSizeSeparator size
        , afterRotation = getArcRotationSeparator rotation
        , afterTo = getPointSeparator to
        }


{-| Parser for a MoveCommand. If the command is the first command parsed after
the command letter, then it is parsed with a separator after that letter.
Otherwise, it is parsed as a LineCommand instead as part of an implicit
sequence, so it skips parsing the afterLetter separator and automatically
populates it with NoLetter.
-}
moveCommand : Bool -> Parser CommandType
moveCommand parsedOne =
    if parsedOne then
        P.succeed (makeLine NoLetter)
            |= formattedPoint

    else
        P.succeed makeMove
            |= separator
            |= formattedPoint


{-| Parser for a LineCommand. If the command is the first command parsed after
the command letter, then it is parsed with a separator after the letter.
Otherwise, it is parsed as part of an implicit sequence, so it skips parsing the afterLetter separator and automatically populates it with NoLetter.
-}
lineCommand : Bool -> Parser CommandType
lineCommand parsedOne =
    if parsedOne then
        P.succeed (makeLine NoLetter)
            |= formattedPoint

    else
        P.succeed makeLine
            |= separator
            |= formattedPoint


{-| Parser for a HorizontalLineCommand. If the command is the first command
parsed after the command letter, then it is parsed with a separator after the
letter. Otherwise, it is parsed as part of an implicit sequence, so it skips
parsing the afterLetter separator and automatically populates it with NoLetter.
-}
horizontalLineCommand : Bool -> Parser CommandType
horizontalLineCommand parsedOne =
    if parsedOne then
        P.succeed (makeHorizontalLine NoLetter)
            |= formattedFloat

    else
        P.succeed makeHorizontalLine
            |= separator
            |= formattedFloat


{-| Parser for a VerticalLineCommand. If the command is the first command parsed
after the command letter, then it is parsed with a separator after the letter.
Otherwise, it is parsed as part of an implicit sequence, so it skips parsing the afterLetter separator and automatically populates it with NoLetter.
-}
verticalLineCommand : Bool -> Parser CommandType
verticalLineCommand parsedOne =
    if parsedOne then
        P.succeed (makeVerticalLine NoLetter)
            |= formattedFloat

    else
        P.succeed makeVerticalLine
            |= separator
            |= formattedFloat


{-| Parser for a CubicCurveCommand. If the command is the first command parsed
after the command letter, then it is parsed with a separator after the letter.
Otherwise, it is parsed as part of an implicit sequence, so it skips parsing the afterLetter separator and automatically populates it with NoLetter.
-}
cubicCurveCommand : Bool -> Parser CommandType
cubicCurveCommand parsedOne =
    if parsedOne then
        P.succeed (makeCubicCurve NoLetter)
            |= formattedCubicCurve

    else
        P.succeed makeCubicCurve
            |= separator
            |= formattedCubicCurve


{-| Parser for a SmoothCubicCurveCommand. If the command is the first command
parsed after the command letter, then it is parsed with a separator after the
letter. Otherwise, it is parsed as part of an implicit sequence, so it skips
parsing the afterLetter separator and automatically populates it with NoLetter.
-}
smoothCubicCurveCommand : Bool -> Parser CommandType
smoothCubicCurveCommand parsedOne =
    if parsedOne then
        P.succeed (makeSmoothCubicCurve NoLetter)
            |= formattedSmoothCubicCurve

    else
        P.succeed makeSmoothCubicCurve
            |= separator
            |= formattedSmoothCubicCurve


{-| Parser for a QuadraticCurveCommand. If the command is the first command
parsed after the command letter, then it is parsed with a separator after the
letter. Otherwise, it is parsed as part of an implicit sequence, so it skips
parsing the afterLetter separator and automatically populates it with NoLetter.
-}
quadraticCurveCommand : Bool -> Parser CommandType
quadraticCurveCommand parsedOne =
    if parsedOne then
        P.succeed (makeQuadraticCurve NoLetter)
            |= formattedQuadraticCurve

    else
        P.succeed makeQuadraticCurve
            |= separator
            |= formattedQuadraticCurve


{-| Parser for a SmoothQuadraticCurveCommand. If the command is the first
command parsed after the command letter, then it is parsed with a separator
after the letter. Otherwise, it is parsed as part of an implicit sequence, so it
skips parsing the afterLetter separator and automatically populates it with
NoLetter.
-}
smoothQuadraticCurveCommand : Bool -> Parser CommandType
smoothQuadraticCurveCommand parsedOne =
    if parsedOne then
        P.succeed (makeSmoothQuadraticCurve NoLetter)
            |= formattedPoint

    else
        P.succeed makeSmoothQuadraticCurve
            |= separator
            |= formattedPoint


{-| Parser for an ArcCommand. If the command is the first command parsed after
the command letter, then it is parsed with a separator after the letter.
Otherwise, it is parsed as part of an implicit sequence, so it skips parsing
the afterLetter separator and automatically populates it with NoLetter.
-}
arcCommand : Bool -> Parser CommandType
arcCommand parsedOne =
    if parsedOne then
        P.succeed (makeArc NoLetter)
            |= formattedArc

    else
        P.succeed makeArc
            |= separator
            |= formattedArc


{-| Special case parser for a Close Command, which consists of only the letter
"Z"/"z" and a Separator.
-}
closeCommand : Builder -> Parser Builder
closeCommand builder =
    let
        relationFormat : Parser ( Relation, CloseFormat )
        relationFormat =
            P.succeed Tuple.pair
                |= P.oneOf
                    [ P.succeed Absolute
                        |. P.token "Z"
                    , P.succeed Relative
                        |. P.token "z"
                    ]
                |= P.map CloseFormat separator

        result : Parser Result
        result =
            relationFormat
                |> P.map
                    (\( relation, format ) ->
                        Valid
                            { relation = relation
                            , commandType = CloseCommand format
                            }
                    )
    in
    result
        |> P.map
            (\newResult ->
                { results = newResult :: builder.results, state = ParsedClose }
            )


{-| Add an Invalid Result to the Builder. Used when transitioning from the
Chomping State to a different State to track the chomped String.
-}
addInvalidResult : Builder -> String -> Builder
addInvalidResult builder invalidString =
    case builder.results of
        (Invalid existing) :: rest ->
            { builder | results = Invalid (existing ++ invalidString) :: rest }

        _ ->
            if invalidString == "" then
                builder

            else
                { builder | results = Invalid invalidString :: builder.results }


{-| Adds a Valid Result to the Builder, meaning a parameterized Command was
successfully parsed, and sets parsedOne to True.
-}
addValidResult : Builder -> Relation -> CommandType -> Builder
addValidResult { results, state } relation commandType =
    let
        command : Command
        command =
            { relation = relation, commandType = commandType }

        newState : State
        newState =
            case state of
                ParsingParameterizedCommandType stateParams ->
                    ParsingParameterizedCommandType
                        { stateParams | parsedOne = True }

                _ ->
                    -- Shouldn't reach this case, but need to account for it
                    state
    in
    { results = Valid command :: results, state = newState }


{-| Adds all the possible branches for a specific parameterized CommandType
Parser. If the Builder has already parsedOne of the CommandType's parameter
sets, add the branches for other Command letters. Otherwise, just add the
branch to revert to the chomping State for when it fails to parse a set of
parameters.
-}
withAllBranches : Builder -> Bool -> String -> Parser Builder -> Parser Builder
withAllBranches builder parsedOne letter commandTypeParser =
    List.concat
        [ [ commandTypeParser ]
        , if parsedOne then
            [ parameterizedCommandLetters builder
            , closeCommand builder
            ]

          else
            []
        , [ chompOne letter builder ]
        ]
        |> P.oneOf


{-| Parser for a single step in the overall parse loop based on the Builder's
State. It will either parse a Command letter or a set of Command parameters, or
chomp a character.
-}
builderStep : Builder -> Parser Builder
builderStep builder =
    case builder.state of
        Chomping chompedString ->
            let
                builderWithResult : Builder
                builderWithResult =
                    addInvalidResult builder chompedString
            in
            P.oneOf
                [ parameterizedCommandLetters builderWithResult
                , closeCommand builderWithResult
                , chompOne chompedString builder
                ]

        ParsingParameterizedCommandType { commandType, relation, parsedOne } ->
            let
                letter : String -> String
                letter uppercase =
                    if parsedOne then
                        ""

                    else
                        case relation of
                            Absolute ->
                                uppercase

                            Relative ->
                                String.toLower uppercase

                makeParser : Parser CommandType -> String -> Parser Builder
                makeParser commandTypeParser uppercase =
                    P.succeed (addValidResult builder relation)
                        |= P.backtrackable commandTypeParser
                        |> withAllBranches builder parsedOne (letter uppercase)
            in
            case commandType of
                Move ->
                    makeParser (moveCommand parsedOne) "M"

                Line ->
                    makeParser (lineCommand parsedOne) "L"

                HorizontalLine ->
                    makeParser (horizontalLineCommand parsedOne) "H"

                VerticalLine ->
                    makeParser (verticalLineCommand parsedOne) "V"

                CubicCurve ->
                    makeParser (cubicCurveCommand parsedOne) "C"

                SmoothCubicCurve ->
                    makeParser (smoothCubicCurveCommand parsedOne) "S"

                QuadraticCurve ->
                    makeParser (quadraticCurveCommand parsedOne) "Q"

                SmoothQuadraticCurve ->
                    makeParser (smoothQuadraticCurveCommand parsedOne) "T"

                Arc ->
                    makeParser (arcCommand parsedOne) "A"

        ParsedClose ->
            P.oneOf
                [ parameterizedCommandLetters builder
                , closeCommand builder
                , chompOne "" builder
                ]


{-| Parser for a Builder. Parses an entire Command String.
-}
builderLoop : Parser Builder
builderLoop =
    P.loop initBuilder
        (\builder ->
            P.oneOf
                [ P.end
                    |> P.map (\_ -> P.Done builder)
                , P.succeed P.Loop
                    |= builderStep builder
                ]
        )


{-| Extracts any remaining Results from the Builder's final State.
-}
finishBuilder : Builder -> List Result
finishBuilder { results, state } =
    case state of
        Chomping chompedString ->
            addInvalidResult { results = results, state = state } chompedString
                |> .results

        ParsingParameterizedCommandType { commandType, relation, parsedOne } ->
            let
                letterCase : String -> String
                letterCase =
                    case relation of
                        Absolute ->
                            identity

                        Relative ->
                            String.toLower

                letter : String
                letter =
                    case commandType of
                        Move ->
                            letterCase "M"

                        Line ->
                            letterCase "L"

                        HorizontalLine ->
                            letterCase "H"

                        VerticalLine ->
                            letterCase "V"

                        CubicCurve ->
                            letterCase "C"

                        SmoothCubicCurve ->
                            letterCase "S"

                        QuadraticCurve ->
                            letterCase "Q"

                        SmoothQuadraticCurve ->
                            letterCase "T"

                        Arc ->
                            letterCase "A"
            in
            if parsedOne then
                results

            else
                addInvalidResult { results = results, state = state } letter
                    |> .results

        ParsedClose ->
            results


{-| Converts a Result to a Command if it is Valid, otherwise returns Nothing.
-}
resultToCommand : Result -> Maybe Command
resultToCommand result =
    case result of
        Valid command ->
            Just command

        Invalid _ ->
            Nothing


{-| Parses a command String into a Path. Skips over any invalid substrings in
the String and only includes valid substrings that map to commands in the Path.
The Builder keeps track of invalid substrings but for now those are just
discarded.
-}
parse : String -> Path
parse commandString =
    case P.run builderLoop commandString of
        Ok builder ->
            finishBuilder builder
                |> List.filterMap resultToCommand
                |> List.reverse
                |> Path.buildComponents
                |> Path.fromComponents

        Err _ ->
            Path.fromComponents []
