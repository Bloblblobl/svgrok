module Path.Parser exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Path2 as Path
import Point exposing (Point)


type CommandType
    = Move
    | Line
    | HorizontalLine
    | VerticalLine
    | CubicCurve
    | SmoothCubicCurve
    | QuadraticCurve
    | SmoothQuadraticCurve
    | Arc


type State
    = NotStarted
    | ParameterizedCommand
        { relation : Path.Relation
        , commandType : CommandType
        , parsedOne : Bool
        }
    | CloseCommand
    | InvalidTokens String


type ParsedPart
    = Valid Path.Component
    | Invalid String


type alias PathBuilder =
    { state : State
    , currentPoint : Point
    , firstConnectedPoint : Point
    , parsedParts : List ParsedPart
    }


type alias PathParser =
    Parser PathBuilder


initPathBuilder : PathBuilder
initPathBuilder =
    { state = NotStarted
    , currentPoint = Point.zero
    , firstConnectedPoint = Point.zero
    , parsedParts = []
    }


isCommandChar : Char -> Bool
isCommandChar char =
    "MLHVCSQTAZ"
        |> String.toList
        |> List.member (Char.toUpper char)


commandToken : Parser String
commandToken =
    P.getChompedString <|
        P.oneOf
            [ P.succeed ()
                |. P.chompIf isCommandChar
                |. P.chompWhile (not << isCommandChar)
            , P.succeed ()
                |. P.chompWhile (not << isCommandChar)
            ]


commandTokensStep : List String -> Parser (P.Step (List String) (List String))
commandTokensStep reversedTokens =
    P.oneOf
        [ P.succeed ()
            |. P.end
            |> P.map (\_ -> P.Done (List.reverse reversedTokens))
        , P.succeed (\t -> P.Loop (t :: reversedTokens))
            |= commandToken
        ]


commandTokens : Parser (List String)
commandTokens =
    P.loop [] commandTokensStep


tokenizeCommandString : String -> List String
tokenizeCommandString commandString =
    case P.run commandTokens commandString of
        Ok tokens ->
            tokens

        Err _ ->
            []
