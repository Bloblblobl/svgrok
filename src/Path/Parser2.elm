module Path.Parser2 exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Path2 as Path
    exposing
        ( CloseFormat
        , Command
        , CommandType(..)
        , Path
        , PointSeparator
        , Relation(..)
        , Separator(..)
        )
import Point exposing (Point)


type alias FormattedFloat =
    { value : Float
    , afterValue : Separator
    }


type alias FormattedPoint =
    { x : Float
    , afterX : Separator
    , y : Float
    , afterY : Separator
    }


{-| The result of parsing a substring of a Command string.
-}
type Result
    = Valid Command
    | Invalid String


{-| All the CommandTypes with parameters (everything but the Close Command).
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
    characters until it finds a Command letter. The Builder will enter one of
    its other States if it encounters a valid Command letter, and store the
    chomped String as an Invalid Result.

  - In the ParsingParameterizedCommandType State, the Builder parses sets of
    parameters corresponding to the commandType. Sequential sets of parameters
    are parsed into Valid Results. If the Builder encounters a Command letter,
    and it has already parsedOne set of parameters, the current State's
    commandType will switch to the corresponding Command letter's type (unless
    the letter is "Z"/"z", in which case it will switch to the ParsedClose
    State). If it hasn't parsedOne, or otherwise fails to parse a set of
    parameters at any point, it will enter the Chomping State. The relation is
    determined by the case of the Command letter that was parsed to enter this
    State (lowercase -> Relative, uppercase -> Absolute).

  - In the ParsedClose State, the Builder only attempts to parse Command
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


{-| A data structure that stores the results of parsing the commandString along
with the current State.
-}
type alias Builder =
    { results : List Result
    , state : State
    }


initBuilder : Builder
initBuilder =
    { results = [], state = Chomping "" }


getPoint : FormattedPoint -> Point
getPoint { x, y } =
    { x = x, y = y }


getPointSeparator : FormattedPoint -> PointSeparator
getPointSeparator { afterX, afterY } =
    { x = afterX, y = afterY }


{-| A List of tuples mapping uppercase Command letters to State builder
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


{-| Chomp a character and add it to the current chompedString.
-}
chompOne : String -> Builder -> Parser Builder
chompOne chompedString builder =
    P.getChompedString (P.chompIf (\_ -> True))
        |> P.map
            (\chompedChar ->
                { builder | state = Chomping (chompedString ++ chompedChar) }
            )


float : Parser Float
float =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.float
        , P.float
        ]


formattedPoint : Parser FormattedPoint
formattedPoint =
    P.succeed FormattedPoint
        |= float
        |= separator
        |= float
        |= separator


separator : Parser Separator
separator =
    P.oneOf
        [ P.succeed
            (\before after -> Spaces (after - before))
            |= P.getOffset
            |. P.spaces
            |= P.getOffset
        , P.succeed
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
        ]


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



-- commandType : Bool -> (Separator -> Parser CommandType) -> Parser CommandType
-- command parsedOne commandTypeParameters =
--     if parsedOne then


moveCommand : Bool -> Parser CommandType
moveCommand parsedOne =
    let
        makeMove : Separator -> FormattedPoint -> CommandType
        makeMove sep formattedTo =
            MoveCommand
                { to = getPoint formattedTo }
                { afterLetter = sep
                , afterTo = getPointSeparator formattedTo
                }
    in
    if parsedOne then
        P.succeed (makeMove NoLetter)
            |= formattedPoint

    else
        P.succeed makeMove
            |= separator
            |= formattedPoint


lineCommand : Bool -> Parser CommandType
lineCommand parsedOne =
    let
        makeLine : Separator -> FormattedPoint -> CommandType
        makeLine sep formattedTo =
            LineCommand
                { to = getPoint formattedTo }
                { afterLetter = sep
                , afterTo = getPointSeparator formattedTo
                }
    in
    if parsedOne then
        P.succeed (makeLine NoLetter)
            |= formattedPoint

    else
        P.succeed makeLine
            |= separator
            |= formattedPoint


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

                _ ->
                    if parsedOne then
                        P.oneOf
                            [ parameterizedCommandLetters builder
                            , chompOne (letter ".") builder
                            ]

                    else
                        chompOne (letter ".") builder

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


resultToString : Result -> String
resultToString result =
    case result of
        Valid command ->
            "V|" ++ Path.commandToString command

        Invalid invalidString ->
            "I|" ++ invalidString


resultToCommand : Result -> Maybe Command
resultToCommand result =
    case result of
        Valid command ->
            Just command

        Invalid _ ->
            Nothing


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



-----------------
-- FOR TESTING --
-----------------


parse2 : String -> List String
parse2 commandString =
    case P.run builderLoop commandString of
        Ok builder ->
            finishBuilder builder
                |> List.map resultToString
                |> List.reverse

        Err deadEnds ->
            [ P.deadEndsToString deadEnds ]


quickParse : Parser a -> String -> Maybe a
quickParse parser string =
    case P.run parser string of
        Ok result ->
            Just result

        Err _ ->
            Nothing
