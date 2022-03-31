module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (append)
import Parser exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type PathPositioning
    = Absolute
    | Relative


type PathArcSize
    = Large
    | Small


type PathArcRotation
    = Clockwise
    | CounterClockwise


type alias PathPoint =
    { x : Float, y : Float }


type PathCommand
    = Move PathPositioning PathPoint
    | Line PathPositioning PathPoint
    | HorizontalLine PathPositioning Float
    | VerticalLine PathPositioning Float
    | CubicCurve PathPositioning PathPoint PathPoint PathPoint
    | SmoothCubicCurve PathPositioning PathPoint PathPoint
    | QuadraticCurve PathPositioning PathPoint PathPoint
    | SmoothQuadraticCurve PathPositioning PathPoint
    | Arc PathPositioning Float Float Float PathArcSize PathArcRotation PathPoint
    | Close


type alias Path =
    List PathCommand


pointString : PathPoint -> String
pointString point =
    String.fromFloat point.x ++ "," ++ String.fromFloat point.y


commandString : PathCommand -> String
commandString command =
    let
        commandLetter : PathPositioning -> String -> String
        commandLetter positioning letter =
            if positioning == Absolute then
                letter

            else
                String.toLower letter
    in
    String.join " "
        (case command of
            Move positioning point ->
                [ "M" |> commandLetter positioning
                , pointString point
                ]

            Line positioning point ->
                [ "L" |> commandLetter positioning
                , pointString point
                ]

            HorizontalLine positioning dx ->
                [ "H" |> commandLetter positioning
                , String.fromFloat dx
                ]

            VerticalLine positioning dy ->
                [ "V" |> commandLetter positioning
                , String.fromFloat dy
                ]

            CubicCurve positioning control1 control2 point ->
                [ "C" |> commandLetter positioning
                , pointString control1
                , pointString control2
                , pointString point
                ]

            SmoothCubicCurve positioning control point ->
                [ "S" |> commandLetter positioning
                , pointString control
                , pointString point
                ]

            QuadraticCurve positioning control point ->
                [ "Q" |> commandLetter positioning
                , pointString control
                , pointString point
                ]

            SmoothQuadraticCurve positioning point ->
                [ "T" |> commandLetter positioning
                , pointString point
                ]

            Arc positioning rx ry angle arcSize arcRotation point ->
                let
                    arcSizeFlag =
                        if arcSize == Large then
                            "1"

                        else
                            "0"

                    arcRotationFlag =
                        if arcRotation == Clockwise then
                            "1"

                        else
                            "0"
                in
                [ "A" |> commandLetter positioning
                , String.fromFloat rx
                , String.fromFloat ry
                , String.fromFloat angle
                , arcSizeFlag
                , arcRotationFlag
                , pointString point
                ]

            Close ->
                [ "Z" ]
        )


pathString : Path -> String
pathString p =
    let
        appendCommand : PathCommand -> String -> String
        appendCommand command string =
            string ++ commandString command
    in
    List.foldl appendCommand "" p


pathFloat : Parser Float
pathFloat =
    oneOf
        [ succeed negate
            |. Parser.symbol "-"
            |= float
        , float
        ]


pathPoint : Parser PathPoint
pathPoint =
    succeed PathPoint
        |= pathFloat
        |. oneOf [ Parser.symbol ",", spaces ]
        |= pathFloat


pathCommand : Parser PathCommand
pathCommand =
    let
        commandLetter : String -> String -> Parser PathPositioning
        commandLetter absoluteLetter relativeLetter =
            oneOf
                [ Parser.map (\_ -> Absolute) (Parser.symbol absoluteLetter)
                , Parser.map (\_ -> Relative) (Parser.symbol relativeLetter)
                ]

        arcSizeFlag : Parser PathArcSize
        arcSizeFlag =
            oneOf
                [ Parser.map (\_ -> Large) (Parser.symbol "1")
                , Parser.map (\_ -> Small) (Parser.symbol "0")
                ]

        arcRotationFlag : Parser PathArcRotation
        arcRotationFlag =
            oneOf
                [ Parser.map (\_ -> Clockwise) (Parser.symbol "1")
                , Parser.map (\_ -> CounterClockwise) (Parser.symbol "0")
                ]
    in
    oneOf
        [ succeed Move
            |= commandLetter "M" "m"
            |. spaces
            |= pathPoint
            |. spaces
        , succeed Line
            |= commandLetter "L" "l"
            |. spaces
            |= pathPoint
            |. spaces
        , succeed HorizontalLine
            |= commandLetter "H" "h"
            |. spaces
            |= pathFloat
            |. spaces
        , succeed VerticalLine
            |= commandLetter "V" "v"
            |. spaces
            |= pathFloat
            |. spaces
        , succeed CubicCurve
            |= commandLetter "C" "c"
            |. spaces
            |= pathPoint
            |. spaces
            |= pathPoint
            |. spaces
            |= pathPoint
            |. spaces
        , succeed SmoothCubicCurve
            |= commandLetter "S" "s"
            |. spaces
            |= pathPoint
            |. spaces
            |= pathPoint
            |. spaces
        , succeed QuadraticCurve
            |= commandLetter "Q" "q"
            |. spaces
            |= pathPoint
            |. spaces
            |= pathPoint
            |. spaces
        , succeed SmoothQuadraticCurve
            |= commandLetter "T" "t"
            |. spaces
            |= pathPoint
            |. spaces
        , succeed Arc
            |= commandLetter "A" "a"
            |. spaces
            |= pathFloat
            |. spaces
            |= pathFloat
            |. spaces
            |= pathFloat
            |. spaces
            |= arcSizeFlag
            |. spaces
            |= arcRotationFlag
            |. spaces
            |= pathPoint
            |. spaces
        , succeed Close
            |. oneOf
                [ Parser.symbol "Z"
                , Parser.symbol "z"
                ]
            |. spaces
        ]


path : Parser Path
path =
    let
        pathHelp : Path -> Parser (Step Path Path)
        pathHelp reversePath =
            oneOf
                [ succeed (\command -> Loop (command :: reversePath))
                    |= pathCommand
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse reversePath))
                ]
    in
    Parser.loop [] pathHelp


parsePath : String -> ( Path, String )
parsePath string =
    case run path string of
        Ok p ->
            ( p, "No Errors Found" )

        Err _ ->
            ( [], "Errors!" )


type alias Model =
    { pathString : String
    , path : Path
    , errorString : String
    }


init : Model
init =
    { pathString = ""
    , path = []
    , errorString = ""
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newPath ->
            let
                parseResult =
                    parsePath newPath
            in
            { model
                | pathString = newPath
                , path = Tuple.first parseResult
                , errorString = Tuple.second parseResult
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.pathString, onInput Change ] []
        , svg
            [ Svg.Attributes.height "120"
            , Svg.Attributes.width "120"
            , viewBox "0 0 10 10"
            ]
            [ Svg.path [ d model.pathString ] [] ]
        , p [] [ Html.text model.errorString ]
        , ul []
            (List.map
                (\command -> li [] [ Html.text (commandString command) ])
                model.path
            )
        ]
