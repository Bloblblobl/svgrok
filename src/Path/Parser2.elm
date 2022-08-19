module Path.Parser2 exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Path2 as Path exposing (Path)
import Point exposing (Point)


{-| The result of parsing a substring of a Command string.
-}
type Result
    = Valid Path.Command
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

  - In the ParsedClose State, the Builder only attempts to parse other
    (non-Close) Command letters. If it fails to find one, it will enter the
    Chomping State.

-}
type State
    = Chomping String
    | ParsingParameterizedCommandType
        { commandType : ParameterizedCommandType
        , relation : Path.Relation
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


{-| Chomp a character and add it to the current chompedString.
-}
chompOne : String -> Builder -> Parser Builder
chompOne chompedString builder =
    P.getChompedString (P.chompIf (\_ -> True))
        |> P.map
            (\chompedChar ->
                { builder
                    | state = Chomping (chompedString ++ chompedChar)
                }
            )


{-| A List of tuples mapping uppercase Command letters to State builder
functions (which map a Relation to a State). Used by parameterizedCommandLetters
to build up parsers for all parameterized Command letters.
-}
commandLetterToStateBuilder : List ( String, Path.Relation -> State )
commandLetterToStateBuilder =
    let
        paramStateBuilder : ParameterizedCommandType -> Path.Relation -> State
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


{-| Parser for parameterized Command letters (all Commands other than Close).
-}
parameterizedCommandLetters : Builder -> Parser Builder
parameterizedCommandLetters builder =
    let
        letterParser : ( String, Path.Relation -> State ) -> Parser Builder
        letterParser ( letter, stateBuilder ) =
            P.oneOf
                [ P.succeed (stateBuilder Path.Absolute)
                    |. P.token letter
                , P.succeed (stateBuilder Path.Relative)
                    |. P.token (String.toLower letter)
                ]
                |> P.map (\state -> { builder | state = state })
    in
    P.oneOf (List.map letterParser commandLetterToStateBuilder)


{-| Parser for a Separator. If the Builder has already parsedOne set of
parameters for the current State and its parsing the first Separator after the
Command letter, it will parse no characters as NoLetter.
-}
separator : Bool -> Parser Path.Separator
separator parsedOne =
    P.oneOf
        [ P.succeed
            (\before after ->
                if after - before == 0 && parsedOne then
                    Path.NoLetter

                else
                    Path.Spaces (after - before)
            )
            |= P.getOffset
            |. P.spaces
            |= P.getOffset
        , P.succeed
            (\before1 after1 before2 after2 ->
                Path.Comma
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


{-| Parser for a Close Command, which consists of only the letter "Z"/"z" and a
Separator.
-}
closeCommand : Builder -> Parser Builder
closeCommand builder =
    let
        relationFormat : Parser ( Path.Relation, Path.CloseFormat )
        relationFormat =
            P.succeed Tuple.pair
                |= P.oneOf
                    [ P.succeed Path.Absolute
                        |. P.token "Z"
                    , P.succeed Path.Relative
                        |. P.token "z"
                    ]
                |= P.map Path.CloseFormat (separator False)

        result : Parser Result
        result =
            relationFormat
                |> P.map
                    (\( relation, format ) ->
                        Valid
                            { relation = relation
                            , commandType = Path.CloseCommand format
                            }
                    )
    in
    result
        |> P.map
            (\newResult ->
                { results = newResult :: builder.results, state = ParsedClose }
            )


{-| Parser for a single step in the overall parse loop based on the Builder's
State. It will either parse a Command letter or a set of Command parameters, or
chomp a character.
-}
builderStep : Builder -> Parser Builder
builderStep builder =
    let
        { results, state } =
            builder
    in
    case state of
        Chomping chompedString ->
            P.oneOf
                [ closeCommand builder
                , chompOne chompedString builder
                ]

        ParsingParameterizedCommandType { commandType, relation, parsedOne } ->
            if parsedOne then
                P.oneOf
                    [ parameterizedCommandLetters builder
                    , chompOne "" builder
                    ]

            else
                chompOne "" builder

        ParsedClose ->
            P.oneOf
                [ parameterizedCommandLetters builder
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


{-| Unwraps the Command in a Result if it's Valid.
-}
commandFromResult : Result -> Maybe Path.Command
commandFromResult result =
    case result of
        Valid command ->
            Just command

        Invalid _ ->
            Nothing


{-| Parses a commandString into a Path.
-}
parseCommandString : String -> Path
parseCommandString commandString =
    case P.run builderLoop commandString of
        Ok { results } ->
            List.filterMap commandFromResult results
                |> Path.buildComponents
                |> Path.fromComponents

        Err _ ->
            Path.fromComponents []


{-| Parses a commandString into a list of Parser Results. (FOR TESTING)
-}
parseCommandStringResults : String -> List Result
parseCommandStringResults commandString =
    case P.run builderLoop commandString of
        Ok { results } ->
            results

        Err _ ->
            []
