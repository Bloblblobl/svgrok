module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=))
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


type alias PathPoint =
    { x : Float, y : Float }


type alias PathPointPair =
    { start : PathPoint, end : PathPoint }


type PathArcSize
    = Large
    | Small


type PathArcRotation
    = Clockwise
    | CounterClockwise


type alias PathArcParameters =
    { radii : PathPoint
    , angle : Float
    , size : PathArcSize
    , rotation : PathArcRotation
    }


type PathCommandType
    = MoveCmd PathPoint
    | LineCmd PathPoint
    | CubicCurveCmd PathPoint PathPointPair
    | SmoothCubicCurveCmd PathPoint PathPoint
    | QuadraticCurveCmd PathPoint PathPoint
    | SmoothQuadraticCurveCmd PathPoint
    | ArcCmd PathPoint PathArcParameters
    | CloseCmd


type PathCommand
    = PathCommand Bool PathCommandType


type alias PathCommands =
    List PathCommand


type PathSegmentType
    = Line
    | CubicCurve PathPointPair
    | QuadraticCurve PathPoint
    | Arc
        { radii : PathPoint
        , angle : Float
        , size : PathArcSize
        , rotation : PathArcRotation
        }


type PathSegment
    = PathSegment PathPointPair PathSegmentType


type alias Path =
    List PathSegment


type alias PathWithPoint =
    { path : Path
    , currentPoint : PathPoint
    }


origin : PathPoint
origin =
    { x = 0, y = 0 }


pointOperate : (Float -> Float -> Float) -> PathPoint -> PathPoint -> PathPoint
pointOperate op point1 point2 =
    { x = op point1.x point2.x
    , y = op point1.y point2.y
    }


pointAdd : PathPoint -> PathPoint -> PathPoint
pointAdd =
    pointOperate (+)


pointSubtract : PathPoint -> PathPoint -> PathPoint
pointSubtract =
    pointOperate (-)



-- TRANSFORMATIONS


pointString : PathPoint -> String
pointString point =
    String.fromFloat point.x ++ "," ++ String.fromFloat point.y


commandString : PathCommand -> String
commandString (PathCommand isRelative commandType) =
    let
        letterCase : String -> String
        letterCase letter =
            if isRelative then
                String.toLower letter

            else
                letter

        arcSizeString : PathArcSize -> String
        arcSizeString size =
            case size of
                Large ->
                    "1"

                Small ->
                    "0"

        arcRotationString : PathArcRotation -> String
        arcRotationString rotation =
            case rotation of
                Clockwise ->
                    "1"

                CounterClockwise ->
                    "0"
    in
    String.join " "
        (case commandType of
            MoveCmd endPoint ->
                [ letterCase "M"
                , pointString endPoint
                ]

            LineCmd endPoint ->
                if endPoint.x == 0 then
                    [ letterCase "V"
                    , String.fromFloat endPoint.y
                    ]

                else if endPoint.y == 0 then
                    [ letterCase "H"
                    , String.fromFloat endPoint.x
                    ]

                else
                    [ letterCase "L"
                    , pointString endPoint
                    ]

            CubicCurveCmd endPoint controls ->
                [ letterCase "C"
                , pointString controls.start
                , pointString controls.end
                , pointString endPoint
                ]

            SmoothCubicCurveCmd endPoint control ->
                [ letterCase "S"
                , pointString control
                , pointString endPoint
                ]

            QuadraticCurveCmd endPoint control ->
                [ letterCase "Q"
                , pointString control
                , pointString endPoint
                ]

            SmoothQuadraticCurveCmd endPoint ->
                [ letterCase "T"
                , pointString endPoint
                ]

            ArcCmd endPoint { radii, angle, size, rotation } ->
                [ letterCase "A"
                , String.fromFloat radii.x
                , String.fromFloat radii.y
                , String.fromFloat angle
                , arcSizeString size
                , arcRotationString rotation
                , pointString endPoint
                ]

            CloseCmd ->
                [ letterCase "Z" ]
        )


commandFromSegment : PathPoint -> PathSegment -> PathCommands
commandFromSegment currentPoint (PathSegment points segmentType) =
    let
        isRelative : Bool
        isRelative =
            False

        makeCommands : PathCommandType -> PathCommands
        makeCommands commandType =
            if points.start /= currentPoint then
                [ PathCommand isRelative (MoveCmd points.start)
                , PathCommand isRelative commandType
                ]

            else
                [ PathCommand isRelative commandType ]
    in
    case segmentType of
        Line ->
            makeCommands (LineCmd points.end)

        CubicCurve controls ->
            makeCommands (CubicCurveCmd points.end controls)

        QuadraticCurve control ->
            makeCommands (QuadraticCurveCmd points.end control)

        Arc parameters ->
            makeCommands (ArcCmd points.end parameters)


segmentFromCommand : PathCommand -> PathWithPoint -> PathWithPoint
segmentFromCommand (PathCommand isRelative commandType) currentPathWithPoint =
    let
        currentPath : Path
        currentPath =
            currentPathWithPoint.path

        targetPoints : PathPoint -> PathPointPair
        targetPoints endPoint =
            { start = currentPathWithPoint.currentPoint
            , end =
                if isRelative then
                    pointAdd currentPathWithPoint.currentPoint endPoint

                else
                    endPoint
            }

        lastSegmentType : Maybe PathSegmentType
        lastSegmentType =
            Maybe.map (\(PathSegment _ lastType) -> lastType) (List.head currentPath)

        previousControlPoint : Maybe PathPoint
        previousControlPoint =
            case ( commandType, lastSegmentType ) of
                ( CubicCurveCmd _ _, Just (CubicCurve controls) ) ->
                    Just controls.end

                ( QuadraticCurveCmd _ _, Just (QuadraticCurve control) ) ->
                    Just control

                _ ->
                    Nothing
    in
    case commandType of
        MoveCmd endPoint ->
            { currentPathWithPoint | currentPoint = (targetPoints endPoint).end }

        LineCmd endPoint ->
            { path = PathSegment (targetPoints endPoint) Line :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        CubicCurveCmd endPoint controls ->
            let
                targetControls : PathPointPair
                targetControls =
                    if isRelative then
                        { start = pointAdd (targetPoints endPoint).start controls.start
                        , end = pointAdd (targetPoints endPoint).start controls.end
                        }

                    else
                        controls
            in
            { path = PathSegment (targetPoints endPoint) (CubicCurve targetControls) :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        SmoothCubicCurveCmd endPoint control ->
            let
                controlDiff : PathPoint
                controlDiff =
                    pointSubtract (targetPoints endPoint).start (targetPoints endPoint).end

                controlStart : PathPoint
                controlStart =
                    case previousControlPoint of
                        Just point ->
                            pointAdd point controlDiff

                        Nothing ->
                            (targetPoints endPoint).start

                targetControls : PathPointPair
                targetControls =
                    if isRelative then
                        { start = controlStart
                        , end = pointAdd (targetPoints endPoint).start control
                        }

                    else
                        { start = controlStart
                        , end = control
                        }
            in
            { path = PathSegment (targetPoints endPoint) (CubicCurve targetControls) :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        QuadraticCurveCmd endPoint control ->
            let
                targetControl : PathPoint
                targetControl =
                    if isRelative then
                        pointAdd (targetPoints endPoint).start control

                    else
                        control
            in
            { path = PathSegment (targetPoints endPoint) (QuadraticCurve targetControl) :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        SmoothQuadraticCurveCmd endPoint ->
            let
                controlDiff : PathPoint
                controlDiff =
                    pointSubtract (targetPoints endPoint).start (targetPoints endPoint).end

                targetControl : PathPoint
                targetControl =
                    case previousControlPoint of
                        Just point ->
                            pointAdd point controlDiff

                        Nothing ->
                            (targetPoints endPoint).start
            in
            { path = PathSegment (targetPoints endPoint) (QuadraticCurve targetControl) :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        ArcCmd endPoint parameters ->
            { path = PathSegment (targetPoints endPoint) (Arc parameters) :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        CloseCmd ->
            let
                firstSegmentHelper : Path -> Maybe PathSegment
                firstSegmentHelper remainingPath =
                    case remainingPath of
                        segment :: [] ->
                            Just segment

                        _ :: rest ->
                            firstSegmentHelper rest

                        [] ->
                            Nothing

                firstSegment : Maybe PathSegment
                firstSegment =
                    firstSegmentHelper currentPath
            in
            case firstSegment of
                Just (PathSegment points _) ->
                    let
                        closingLinePoints : PathPointPair
                        closingLinePoints =
                            { start = currentPathWithPoint.currentPoint, end = points.start }
                    in
                    { path = PathSegment closingLinePoints Line :: currentPath
                    , currentPoint = points.start
                    }

                Nothing ->
                    currentPathWithPoint


pathFromCommands : PathCommands -> Path
pathFromCommands commands =
    List.foldl segmentFromCommand { path = [], currentPoint = origin } commands
        |> .path
        |> List.reverse



-- PARSERS


pathFloat : Parser.Parser Float
pathFloat =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.float
        , Parser.float
        ]


pathPoint : Parser.Parser PathPoint
pathPoint =
    Parser.succeed PathPoint
        |= pathFloat
        |. Parser.oneOf [ Parser.symbol ",", Parser.spaces ]
        |= pathFloat


pathPointPair : Parser.Parser PathPointPair
pathPointPair =
    Parser.succeed PathPointPair
        |= pathPoint
        |. Parser.spaces
        |= pathPoint


moveCommandType : Parser.Parser PathCommandType
moveCommandType =
    Parser.succeed MoveCmd
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces


lineCommandType : Parser.Parser PathCommandType
lineCommandType =
    Parser.succeed LineCmd
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces


horizontalLineCommandType : Parser.Parser PathCommandType
horizontalLineCommandType =
    Parser.succeed LineCmd
        |. Parser.spaces
        |= Parser.map (\dx -> { x = dx, y = 0 }) pathFloat
        |. Parser.spaces


verticalLineCommandType : Parser.Parser PathCommandType
verticalLineCommandType =
    Parser.succeed LineCmd
        |. Parser.spaces
        |= Parser.map (\dy -> { x = 0, y = dy }) pathFloat
        |. Parser.spaces


cubicCurveCommandType : Parser.Parser PathCommandType
cubicCurveCommandType =
    Parser.succeed CubicCurveCmd
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces
        |= pathPointPair
        |. Parser.spaces


smoothCubicCurveCommandType : Parser.Parser PathCommandType
smoothCubicCurveCommandType =
    Parser.succeed SmoothCubicCurveCmd
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces


quadraticCurveCommandType : Parser.Parser PathCommandType
quadraticCurveCommandType =
    Parser.succeed QuadraticCurveCmd
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces


smoothQuadraticCurveCommandType : Parser.Parser PathCommandType
smoothQuadraticCurveCommandType =
    Parser.succeed SmoothQuadraticCurveCmd
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces


arcCommandType : Parser.Parser PathCommandType
arcCommandType =
    let
        arcSizeFlag : Parser.Parser PathArcSize
        arcSizeFlag =
            Parser.oneOf
                [ Parser.map (\_ -> Large) (Parser.symbol "1")
                , Parser.map (\_ -> Small) (Parser.symbol "0")
                ]

        arcRotationFlag : Parser.Parser PathArcRotation
        arcRotationFlag =
            Parser.oneOf
                [ Parser.map (\_ -> Clockwise) (Parser.symbol "1")
                , Parser.map (\_ -> CounterClockwise) (Parser.symbol "0")
                ]

        arcParameters : Parser.Parser PathArcParameters
        arcParameters =
            Parser.succeed PathArcParameters
                |= pathPoint
                |. Parser.spaces
                |= pathFloat
                |. Parser.spaces
                |= arcSizeFlag
                |. Parser.spaces
                |= arcRotationFlag
    in
    Parser.succeed ArcCmd
        |. Parser.spaces
        |= pathPoint
        |. Parser.spaces
        |= arcParameters
        |. Parser.spaces


closeCommandType : Parser.Parser PathCommandType
closeCommandType =
    Parser.succeed CloseCmd
        |. Parser.spaces


pathCommandType : String -> Parser.Parser PathCommandType -> Parser.Parser PathCommand
pathCommandType commandLetter commandType =
    Parser.succeed PathCommand
        |= Parser.oneOf
            [ Parser.map (\_ -> False) (Parser.symbol commandLetter)
            , Parser.map (\_ -> True) (Parser.symbol (String.toLower commandLetter))
            ]
        |= commandType


pathCommand : Parser.Parser PathCommand
pathCommand =
    Parser.oneOf
        [ pathCommandType "M" moveCommandType
        , pathCommandType "L" lineCommandType
        , pathCommandType "H" horizontalLineCommandType
        , pathCommandType "V" verticalLineCommandType
        , pathCommandType "C" cubicCurveCommandType
        , pathCommandType "S" smoothCubicCurveCommandType
        , pathCommandType "Q" quadraticCurveCommandType
        , pathCommandType "T" smoothQuadraticCurveCommandType
        , pathCommandType "A" arcCommandType
        , pathCommandType "Z" closeCommandType
        ]


pathCommands : Parser.Parser PathCommands
pathCommands =
    let
        pathHelp : PathCommands -> Parser.Parser (Parser.Step PathCommands PathCommands)
        pathHelp reversePath =
            Parser.oneOf
                [ Parser.succeed (\command -> Parser.Loop (command :: reversePath))
                    |= pathCommand
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse reversePath))
                ]
    in
    Parser.loop [] pathHelp


parsePathString : String -> ( PathCommands, String )
parsePathString pathString =
    case Parser.run pathCommands pathString of
        Ok pcs ->
            ( pcs, "No Errors Found" )

        Err _ ->
            ( [], "Errors!" )


type alias Model =
    { path : Path
    , pathCommands : PathCommands
    , pathCommandsString : String
    , parseErrorString : String
    }


init : Model
init =
    { path = []
    , pathCommands = []
    , pathCommandsString = ""
    , parseErrorString = ""
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newPathString ->
            let
                ( parsedPathCommands, parsedErrorString ) =
                    parsePathString newPathString
            in
            { model
                | path = pathFromCommands parsedPathCommands
                , pathCommands = parsedPathCommands
                , pathCommandsString = newPathString
                , parseErrorString = parsedErrorString
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.pathCommandsString, onInput Change ] []
        , svg
            [ Svg.Attributes.height "120"
            , Svg.Attributes.width "120"
            , viewBox "0 0 10 10"
            ]
            [ Svg.path [ d model.pathCommandsString ] [] ]
        , p [] [ Html.text model.parseErrorString ]
        , ul []
            (List.map
                (\command -> li [] [ Html.text (commandString command) ])
                model.pathCommands
            )
        ]
