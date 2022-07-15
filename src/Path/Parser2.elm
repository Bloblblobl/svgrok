module Path.Parser2 exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Path2 as Path exposing (Path)
import Point exposing (Point)


{-| An iteration of a Parser.loop call where the type of the intermediate state
is the same as the result.
-}
type alias SimpleLoopStep a =
    a -> Parser (P.Step a a)


{-| The result of parsing a substring of a Command string.
-}
type ParsedResult
    = Valid Path.Command
    | Invalid String


{-| All the CommandTypes with parameters.
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
-}
type State
    = Initial
    | ParsingParameterizedCommandType
        { commandType : ParameterizedCommandType
        , relation : Path.Relation
        , parsedOne : Bool
        }
    | ParsingClose
    | ParsingInvalid


{-| Builder of the Path.
-}
type alias Builder =
    {}


updateBuilder : Builder -> Parser Builder
updateBuilder builder =
    P.succeed builder


step : SimpleLoopStep Builder
step builder =
    P.oneOf
        [ P.end
            |> P.map (\_ -> P.Done builder)
        , P.succeed P.Loop
            |= updateBuilder builder
        ]
