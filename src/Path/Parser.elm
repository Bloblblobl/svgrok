module Path.Parser exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Path2 as Path exposing (Command, Path)
import Point exposing (Point)


{-| An iteration of a Parser.loop call where the type of the intermediate state
is the same as the result.
-}
type alias SimpleLoopStep a =
    a -> Parser (P.Step a a)


{-| A part of the result of parsing a Command string. Valid Tokens correspond to
a single Command, whereas invalid Tokens correspond to substrings which do not
map to Commands, and errors represent a parsing error.
-}
type Token
    = Valid Command String
    | Invalid String
    | Error (List P.DeadEnd)


{-| Return True if the character is a valid Command character, False otherwise.
-}
isCommandChar : Char -> Bool
isCommandChar char =
    "MLHVCSQTAZ"
        |> String.toList
        |> List.member (Char.toUpper char)


{-| Get a Relation from the case of a Command character string. If the character
is uppercase, return Absolute, otherwise Relative.
-}
relationFromString : String -> Path.Relation
relationFromString string =
    let
        commandChar : Char
        commandChar =
            case String.uncons string of
                Just ( char, _ ) ->
                    char

                Nothing ->
                    'A'
    in
    if Char.isUpper commandChar then
        Path.Absolute

    else
        Path.Relative


{-| Split a Command string into substrings where each substring (other than the
first, which may be an invalid prefix) begins with a Command character and
corresponds to either a valid Command sequence or an invalid sequence.
-}
splitCommandString : String -> List String
splitCommandString commandString =
    let
        step : Char -> List String -> List String
        step char sequenceStrings =
            case sequenceStrings of
                head :: tail ->
                    if isCommandChar char then
                        String.fromChar char :: head :: tail

                    else
                        String.cons char head :: tail

                [] ->
                    [ String.fromChar char ]
    in
    String.foldl step [] commandString
        |> List.map String.reverse
        |> List.reverse


{-| Chomp exactly one character off of the string being parsed.
-}
chompOne : Parser ()
chompOne =
    P.chompIf (\_ -> True)


{-| Chomp the rest of the string being parsed.
-}
chompRest : Parser ()
chompRest =
    let
        step : SimpleLoopStep ()
        step _ =
            P.oneOf
                [ P.succeed ()
                    |. P.end
                    |> P.map (\_ -> P.Done ())
                , P.succeed (\_ -> P.Loop ())
                    |= chompOne
                ]
    in
    P.loop () step


{-| Parse an Invalid Token (chomps the rest of the string into the token).
-}
invalidToken : Parser Token
invalidToken =
    let
        firstLetter : Parser String
        firstLetter =
            P.getChompedString <|
                P.succeed ()
                    |. chompOne

        invalid : Parser String
        invalid =
            P.succeed Tuple.pair
                |= firstLetter
                |= P.getChompedString chompRest
                |> P.map (\( first, second ) -> first ++ second)
    in
    P.succeed Invalid
        |= invalid


{-| Parse a separator in a Command string. This can be either any number of
spaces or any number of spaces followed by a comma followed by any number of
spaces again.
-}
separator : Parser ()
separator =
    P.succeed ()
        |. P.spaces
        |. P.oneOf
            [ P.symbol ","
            , P.succeed ()
            ]
        |. P.spaces


{-| Parse a float (including negative floats starting with a "-").
-}
float : Parser Float
float =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.float
        , P.float
        ]


{-| Parse a Point.
-}
point : Parser Point
point =
    P.succeed Point
        |= float
        |. separator
        |= float


arcSize : Parser Path.ArcSize
arcSize =
    P.oneOf
        [ P.map (\_ -> Path.Large) (P.symbol "1")
        , P.map (\_ -> Path.Small) (P.symbol "0")
        ]


arcRotation : Parser Path.ArcRotation
arcRotation =
    P.oneOf
        [ P.map (\_ -> Path.Clockwise) (P.symbol "1")
        , P.map (\_ -> Path.CounterClockwise) (P.symbol "0")
        ]



{-

   {-| Parse all CommandTypes
   -}
   moveParameters : Parser Path.CommandType
   moveParameters =
       P.succeed BaseParameters
           |. separator
           |= point
           |. separator
           |> P.map Path.MoveCommand


   lineParameters : Parser Path.CommandType
   lineParameters =
       P.succeed BaseParameters
           |. separator
           |= point
           |. separator
           |> P.map Path.LineCommand


   horizontalLineParameters : Parser Path.CommandType
   horizontalLineParameters =
       P.succeed HorizontalLineParameters
           |. separator
           |= float
           |. separator
           |> P.map Path.HorizontalLineCommand


   verticalLineParameters : Parser Path.CommandType
   verticalLineParameters =
       P.succeed VerticalLineParameters
           |. separator
           |= float
           |. separator
           |> P.map Path.VerticalLineCommand


   cubicCurveParameters : Parser Path.CommandType
   cubicCurveParameters =
       P.succeed CubicCurveParameters
           |. separator
           |= point
           |. separator
           |= point
           |. separator
           |= point
           |. separator
           |> P.map Path.CubicCurveCommand


   smoothCubicCurveParameters : Parser Path.CommandType
   smoothCubicCurveParameters =
       P.succeed SmoothCubicCurveParameters
           |. separator
           |= point
           |. separator
           |= point
           |. separator
           |> P.map Path.SmoothCubicCurveCommand


   quadraticCurveParameters : Parser Path.CommandType
   quadraticCurveParameters =
       P.succeed QuadraticCurveParameters
           |. separator
           |= point
           |. separator
           |= point
           |. separator
           |> P.map Path.QuadraticCurveCommand


   smoothQuadraticCurveParameters : Parser Path.CommandType
   smoothQuadraticCurveParameters =
       P.succeed BaseParameters
           |. separator
           |= point
           |. separator
           |> P.map Path.SmoothQuadraticCurveCommand


   arcParameters : Parser Path.CommandType
   arcParameters =
       P.succeed ArcParameters
           |. separator
           |= point
           |. separator
           |= float
           |. separator
           |= arcSize
           |. separator
           |= arcRotation
           |. separator
           |= point
           |. separator
           |> P.map Path.ArcCommand


   {-| Parse a list of Tokens from a sequence string for a CloseCommand. A
   CloseCommand has no parameters, so any non-separator characters other than the
   Command character are invalid.
   -}
   parseCloseCommand : String -> List Token
   parseCloseCommand closeString =
       let
           closeCommand : Parser Token
           closeCommand =
               chompOne
                   |> P.getChompedString
                   |> P.map relationFromString
                   |> P.map (\relation -> Command relation Path.CloseCommand)
                   |> P.map (\command -> Valid command "")

           closeSequence : Parser (List Token)
           closeSequence =
               P.oneOf
                   [ P.map List.singleton closeCommand
                       |. P.end
                   , P.succeed Tuple.pair
                       |= closeCommand
                       |= invalidToken
                       |> P.map (\( close, invalid ) -> [ close, invalid ])
                   ]
       in
       case P.run closeSequence closeString of
           Ok tokens ->
               tokens

           Err deadEnds ->
               [ Error deadEnds ]


   {-| Parse an Invalid Token from a string.
   -}
   parseInvalid : String -> List Token
   parseInvalid invalidString =
       case P.run (P.map List.singleton invalidToken) invalidString of
           Ok tokens ->
               tokens

           Err deadEnds ->
               [ Error deadEnds ]


   {-| Parse a list of Tokens from a sequence string.
   -}
   parseSequence : String -> Parser Path.CommandType -> List Token
   parseSequence sequenceString commandParameters =
       let
           step : Path.Relation -> SimpleLoopStep (List Token)
           step relation reverseCommands =
               P.oneOf
                   [ P.succeed (\command -> P.Loop (command :: reverseCommands))
                       |= (commandParameters
                               |> P.mapChompedString (validToken relation)
                          )
                   , P.succeed ()
                       |> P.map (\_ -> P.Done (List.reverse reverseCommands))
                   ]

           validToken : Path.Relation -> String -> Path.CommandType -> Token
           validToken relation string commandType =
               Valid (Command relation commandType) string

           validSequenceFromRelation : Path.Relation -> Parser (List Token)
           validSequenceFromRelation relation =
               P.succeed Tuple.pair
                   |= (commandParameters
                           |> P.mapChompedString (validToken relation)
                      )
                   |= P.loop [] (step relation)
                   |> P.map (\( first, rest ) -> first :: rest)

           validSequence : Parser (List Token)
           validSequence =
               chompOne
                   |> P.getChompedString
                   |> P.map relationFromString
                   |> P.andThen validSequenceFromRelation

           trailingInvalid : Parser (List Token)
           trailingInvalid =
               P.succeed Tuple.pair
                   |= validSequence
                   |= invalidToken
                   |> P.map (\( tokens, invalid ) -> tokens ++ [ invalid ])

           sequence : Parser (List Token)
           sequence =
               P.oneOf
                   [ P.backtrackable validSequence
                   , P.backtrackable trailingInvalid
                   , P.map List.singleton invalidToken
                   ]
       in
       case P.run sequence sequenceString of
           Ok tokens ->
               tokens

           Err deadEnds ->
               [ Error deadEnds ]


   {-| Parse a sequence string into a list of Tokens. Sequences that do not begin
   with a Command character will become a single Invalid Token containing the
   whole sequence string. Sequences that do begin with a Command character will
   be parsed one set of parameters at a time into individual Valid Tokens. If
   parsing a set of parameters fails at any point, the rest of the sequence will
   become a single Invalid Token. Empty strings result in an empty list.
   -}
   tokensFromSequenceString : String -> List Token
   tokensFromSequenceString sequenceString =
       let
           commandLetter : String
           commandLetter =
               String.toUpper (String.left 1 sequenceString)
       in
       case commandLetter of
           "M" ->
               parseSequence sequenceString moveParameters

           "L" ->
               parseSequence sequenceString lineParameters

           "H" ->
               parseSequence sequenceString horizontalLineParameters

           "V" ->
               parseSequence sequenceString verticalLineParameters

           "C" ->
               parseSequence sequenceString cubicCurveParameters

           "S" ->
               parseSequence sequenceString smoothCubicCurveParameters

           "Q" ->
               parseSequence sequenceString quadraticCurveParameters

           "T" ->
               parseSequence sequenceString smoothQuadraticCurveParameters

           "A" ->
               parseSequence sequenceString arcParameters

           "Z" ->
               parseCloseCommand sequenceString

           _ ->
               parseInvalid sequenceString


   {-| Filter through a list of Tokens and return a list of tuples of Commands and
   strings from the valid Tokens.
   -}
   commandsAndStringsFromValidTokens : List Token -> List ( Command, String )
   commandsAndStringsFromValidTokens tokens =
       let
           step : Token -> List ( Command, String ) -> List ( Command, String )
           step token commandsWithStrings =
               case token of
                   Valid command string ->
                       ( command, string ) :: commandsWithStrings

                   Invalid _ ->
                       commandsWithStrings

                   Error _ ->
                       commandsWithStrings
       in
       List.reverse (List.foldl step [] tokens)


   {-| Parse a Path from a Command string.
   -}
   parseCommandString : String -> Path
   parseCommandString commandString =
       splitCommandString commandString
           |> List.concatMap tokensFromSequenceString
           |> commandsAndStringsFromValidTokens
           |> Path.buildComponents
           |> Path.fromComponents



   --------------------
   -- TEST FUNCTIONS --
   --  TODO: REMOVE  --
   --------------------


   testTokens : String -> List Token
   testTokens string =
       splitCommandString string
           |> List.concatMap tokensFromSequenceString

-}
