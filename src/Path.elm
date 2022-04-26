module Path exposing (..)

import Parser as P exposing ((|.), (|=))



-- TYPES


type alias Point =
    { x : Float, y : Float }


type alias PointPair =
    { start : Point, end : Point }


type ArcSize
    = Large
    | Small


type ArcRotation
    = Clockwise
    | CounterClockwise


type alias ArcParameters =
    { radii : Point
    , angle : Float
    , size : ArcSize
    , rotation : ArcRotation
    }


type CommandType
    = MoveCommand Point
    | LineCommand Point
    | HorizontalLineCommand Float
    | VerticalLineCommand Float
    | CubicCurveCommand Point PointPair
    | SmoothCubicCurveCommand Point Point
    | QuadraticCurveCommand Point Point
    | SmoothQuadraticCurveCommand Point
    | ArcCommand Point ArcParameters
    | CloseCommand


type Command
    = Command Bool CommandType


type alias Commands =
    List Command


type SegmentType
    = Line
    | CubicCurve PointPair
    | QuadraticCurve Point
    | Arc
        { radii : Point
        , angle : Float
        , size : ArcSize
        , rotation : ArcRotation
        }


type Segment
    = Segment PointPair SegmentType


type alias Path =
    List Segment


type ParsingCommandType
    = ParsingMove
    | ParsingLine
    | ParsingHorizontalLine
    | ParsingVerticalLine
    | ParsingCubicCurve
    | ParsingSmoothCubicCurve
    | ParsingQuadraticCurve
    | ParsingSmoothQuadraticCurve
    | ParsingArc
    | ParsingClose


type ParseState
    = NotStarted
    | ParsingCommand Bool ParsingCommandType
    | ParsedCommand Bool ParsingCommandType
    | Invalid


type alias ParseInfo =
    { path : Path
    , commands : Commands
    , state : ParseState
    , currentPoint : Point
    , firstConnectedPoint : Maybe Point
    }


type alias CommandToSegmentInfo =
    { command : Command
    , currentPoint : Point
    , firstConnectedPoint : Maybe Point
    , lastCommandType : Maybe CommandType
    , lastSegmentType : Maybe SegmentType
    }



-- STRING UTILS


wrapParens : String -> String
wrapParens string =
    "(" ++ string ++ ")"


wrapBraces : String -> String
wrapBraces string =
    "{" ++ string ++ "}"



-- POINT FUNCTIONS


origin : Point
origin =
    { x = 0, y = 0 }


originPair : PointPair
originPair =
    { start = origin, end = origin }


pointOperate : (Float -> Float -> Float) -> Point -> Point -> Point
pointOperate op point1 point2 =
    { x = op point1.x point2.x
    , y = op point1.y point2.y
    }


pointAdd : Point -> Point -> Point
pointAdd =
    pointOperate (+)


pointSubtract : Point -> Point -> Point
pointSubtract =
    pointOperate (-)


pointPairToVector : PointPair -> Point
pointPairToVector pair =
    pointSubtract pair.end pair.start



-- TO STRING FUNCTIONS


pointToString : Point -> String
pointToString p =
    String.fromFloat p.x ++ "," ++ String.fromFloat p.y


pointPairToString : PointPair -> String
pointPairToString { start, end } =
    String.join ""
        [ "start: "
        , wrapParens (pointToString start)
        , ", end: "
        , wrapParens (pointToString end)
        ]
        |> wrapBraces


commandToString : Command -> String
commandToString (Command isRelative cmdType) =
    let
        letterCase : String -> String
        letterCase letter =
            if isRelative then
                String.toLower letter

            else
                letter

        arcSizeString : ArcSize -> String
        arcSizeString size =
            case size of
                Large ->
                    "1"

                Small ->
                    "0"

        arcRotationString : ArcRotation -> String
        arcRotationString rotation =
            case rotation of
                Clockwise ->
                    "1"

                CounterClockwise ->
                    "0"
    in
    String.join " "
        (case cmdType of
            MoveCommand endPoint ->
                [ letterCase "M"
                , pointToString endPoint
                ]

            LineCommand endPoint ->
                [ letterCase "L"
                , pointToString endPoint
                ]

            HorizontalLineCommand x ->
                [ letterCase "H"
                , String.fromFloat x
                ]

            VerticalLineCommand y ->
                [ letterCase "V"
                , String.fromFloat y
                ]

            CubicCurveCommand endPoint controls ->
                [ letterCase "C"
                , pointToString controls.start
                , pointToString controls.end
                , pointToString endPoint
                ]

            SmoothCubicCurveCommand endPoint control ->
                [ letterCase "S"
                , pointToString control
                , pointToString endPoint
                ]

            QuadraticCurveCommand endPoint control ->
                [ letterCase "Q"
                , pointToString control
                , pointToString endPoint
                ]

            SmoothQuadraticCurveCommand endPoint ->
                [ letterCase "T"
                , pointToString endPoint
                ]

            ArcCommand endPoint { radii, angle, size, rotation } ->
                [ letterCase "A"
                , String.fromFloat radii.x
                , String.fromFloat radii.y
                , String.fromFloat angle
                , arcSizeString size
                , arcRotationString rotation
                , pointToString endPoint
                ]

            CloseCommand ->
                [ letterCase "Z" ]
        )


segmentToString : Segment -> String
segmentToString (Segment points segmentType) =
    case segmentType of
        Line ->
            "Line at " ++ pointPairToString points

        CubicCurve controls ->
            String.join " "
                [ "Cubic Curve at"
                , pointPairToString points
                , "with controls at"
                , pointPairToString controls
                ]

        QuadraticCurve control ->
            String.join " "
                [ "Quadratic Curve at"
                , pointPairToString points
                , "with control at"
                , wrapParens (pointToString control)
                ]

        Arc parameters ->
            let
                arcSizeString : String
                arcSizeString =
                    case parameters.size of
                        Large ->
                            "Large"

                        Small ->
                            "Small"

                arcRotationString : String
                arcRotationString =
                    case parameters.rotation of
                        Clockwise ->
                            "Clockwise"

                        CounterClockwise ->
                            "Counter-Clockwise"
            in
            String.join " "
                [ "Arc at"
                , pointPairToString points
                , "with x,y radii"
                , pointToString parameters.radii
                , "and that is"
                , arcSizeString
                , "and"
                , arcRotationString
                , "rotated"
                , String.fromFloat parameters.angle
                , "degrees"
                ]



-- CONVERSION FUNCTIONS


commandFromSegment : Point -> Segment -> Commands
commandFromSegment currentPoint (Segment points segmentType) =
    let
        isRelative : Bool
        isRelative =
            False

        makeCommands : CommandType -> Commands
        makeCommands cmdType =
            if points.start /= currentPoint then
                [ Command isRelative (MoveCommand points.start)
                , Command isRelative cmdType
                ]

            else
                [ Command isRelative cmdType ]
    in
    case segmentType of
        Line ->
            makeCommands (LineCommand points.end)

        CubicCurve controls ->
            makeCommands (CubicCurveCommand points.end controls)

        QuadraticCurve control ->
            makeCommands (QuadraticCurveCommand points.end control)

        Arc parameters ->
            makeCommands (ArcCommand points.end parameters)


segmentFromCommand : CommandToSegmentInfo -> Maybe Segment
segmentFromCommand info =
    let
        ( isRelative, cmdType ) =
            (\(Command ir ct) -> ( ir, ct )) info.command

        currentPoint : Point
        currentPoint =
            info.currentPoint

        targetPoints : Point -> PointPair
        targetPoints endPoint =
            if isRelative then
                { start = info.currentPoint
                , end = pointAdd info.currentPoint endPoint
                }

            else
                { start = info.currentPoint
                , end = endPoint
                }
    in
    case cmdType of
        MoveCommand _ ->
            Nothing

        LineCommand endPoint ->
            Just <| Segment (targetPoints endPoint) Line

        HorizontalLineCommand x ->
            let
                points : PointPair
                points =
                    if isRelative then
                        { start = currentPoint
                        , end = pointAdd currentPoint { x = x, y = 0 }
                        }

                    else
                        { start = currentPoint
                        , end = { currentPoint | x = x }
                        }
            in
            Just <| Segment points Line

        VerticalLineCommand y ->
            let
                points : PointPair
                points =
                    if isRelative then
                        { start = currentPoint
                        , end = pointAdd currentPoint { x = 0, y = y }
                        }

                    else
                        { start = currentPoint
                        , end = { currentPoint | y = y }
                        }
            in
            Just <| Segment points Line

        CubicCurveCommand endPoint controls ->
            let
                targetControls : PointPair
                targetControls =
                    if isRelative then
                        { start = pointAdd info.currentPoint controls.start
                        , end = pointAdd info.currentPoint controls.end
                        }

                    else
                        controls
            in
            Just <| Segment (targetPoints endPoint) (CubicCurve targetControls)

        SmoothCubicCurveCommand endPoint control ->
            let
                endControl : Point
                endControl =
                    if isRelative then
                        pointAdd info.currentPoint control

                    else
                        control

                defaultControls : PointPair
                defaultControls =
                    { start = info.currentPoint
                    , end = endControl
                    }

                followingCubicCurve : Bool
                followingCubicCurve =
                    case info.lastCommandType of
                        Just (CubicCurveCommand _ _) ->
                            True

                        Just (SmoothCubicCurveCommand _ _) ->
                            True

                        _ ->
                            False

                targetControls : PointPair
                targetControls =
                    if followingCubicCurve then
                        case info.lastSegmentType of
                            Just (CubicCurve previousControls) ->
                                { start =
                                    pointSubtract
                                        previousControls.end
                                        (pointPairToVector <|
                                            targetPoints endPoint
                                        )
                                , end = endControl
                                }

                            _ ->
                                defaultControls

                    else
                        defaultControls
            in
            Just <|
                Segment (targetPoints endPoint) (CubicCurve targetControls)

        QuadraticCurveCommand endPoint control ->
            let
                targetControl : Point
                targetControl =
                    if isRelative then
                        pointAdd info.currentPoint control

                    else
                        control
            in
            Just <|
                Segment (targetPoints endPoint) (QuadraticCurve targetControl)

        SmoothQuadraticCurveCommand endPoint ->
            let
                followingQuadraticCurve : Bool
                followingQuadraticCurve =
                    case info.lastCommandType of
                        Just (QuadraticCurveCommand _ _) ->
                            True

                        Just (SmoothQuadraticCurveCommand _) ->
                            True

                        _ ->
                            False

                targetControl : Point
                targetControl =
                    if followingQuadraticCurve then
                        case info.lastSegmentType of
                            Just (QuadraticCurve previousControl) ->
                                pointSubtract
                                    previousControl
                                    (pointPairToVector <| targetPoints endPoint)

                            _ ->
                                info.currentPoint

                    else
                        info.currentPoint
            in
            Just <|
                Segment (targetPoints endPoint) (QuadraticCurve targetControl)

        ArcCommand endPoint parameters ->
            Just <| Segment (targetPoints endPoint) (Arc parameters)

        CloseCommand ->
            Maybe.map
                (\endPoint -> Segment (targetPoints endPoint) Line)
                info.firstConnectedPoint



-- PARSERS


initParseInfo : ParseInfo
initParseInfo =
    { path = []
    , commands = []
    , state = NotStarted
    , currentPoint = origin
    , firstConnectedPoint = Nothing
    }


float : P.Parser Float
float =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.float
        , P.float
        ]


point : P.Parser Point
point =
    P.succeed Point
        |= float
        |. P.spaces
        |. P.oneOf
            [ P.symbol ","
            , P.succeed ()
            ]
        |. P.spaces
        |= float


pointPair : P.Parser PointPair
pointPair =
    P.succeed PointPair
        |= point
        |. P.spaces
        |= point


letterToParseState : String -> ParseState
letterToParseState letter =
    let
        isRelative : Bool
        isRelative =
            String.toUpper letter /= letter
    in
    case String.toUpper letter of
        "M" ->
            ParsingCommand isRelative ParsingMove

        "L" ->
            ParsingCommand isRelative ParsingLine

        "H" ->
            ParsingCommand isRelative ParsingHorizontalLine

        "V" ->
            ParsingCommand isRelative ParsingVerticalLine

        "C" ->
            ParsingCommand isRelative ParsingCubicCurve

        "S" ->
            ParsingCommand isRelative ParsingSmoothCubicCurve

        "Q" ->
            ParsingCommand isRelative ParsingQuadraticCurve

        "T" ->
            ParsingCommand isRelative ParsingSmoothCubicCurve

        "A" ->
            ParsingCommand isRelative ParsingArc

        "Z" ->
            ParsedCommand isRelative ParsingClose

        _ ->
            Invalid


commandLetters : ParseInfo -> String -> P.Parser ParseInfo
commandLetters info validLetters =
    let
        helper : Char -> List (P.Parser String) -> List (P.Parser String)
        helper letter letterParsers =
            List.concat
                [ letterParsers
                , P.succeed ()
                    |. P.token (String.fromChar letter)
                    |> P.getChompedString
                    |> List.singleton
                , P.succeed ()
                    |. P.token (String.fromChar <| Char.toLower letter)
                    |> P.getChompedString
                    |> List.singleton
                ]

        letterToInfo : String -> ParseInfo
        letterToInfo letter =
            if String.toUpper letter == "Z" then
                createCommand
                    { info | state = letterToParseState letter }
                    CloseCommand

            else
                { info | state = letterToParseState letter }
    in
    String.foldl helper [] validLetters
        |> List.map (P.map letterToInfo)
        |> P.oneOf


commandType : ParsingCommandType -> P.Parser CommandType
commandType parsingCommandType =
    case parsingCommandType of
        ParsingMove ->
            P.succeed MoveCommand
                |. P.spaces
                |= point
                |. P.spaces

        ParsingLine ->
            P.succeed LineCommand
                |. P.spaces
                |= point
                |. P.spaces

        ParsingHorizontalLine ->
            P.succeed HorizontalLineCommand
                |. P.spaces
                |= float
                |. P.spaces

        ParsingVerticalLine ->
            P.succeed VerticalLineCommand
                |. P.spaces
                |= float
                |. P.spaces

        ParsingCubicCurve ->
            P.succeed CubicCurveCommand
                |. P.spaces
                |= point
                |. P.spaces
                |= pointPair
                |. P.spaces

        ParsingSmoothCubicCurve ->
            P.succeed SmoothCubicCurveCommand
                |. P.spaces
                |= point
                |. P.spaces
                |= point
                |. P.spaces

        ParsingQuadraticCurve ->
            P.succeed QuadraticCurveCommand
                |. P.spaces
                |= point
                |. P.spaces
                |= point
                |. P.spaces

        ParsingSmoothQuadraticCurve ->
            P.succeed SmoothQuadraticCurveCommand
                |. P.spaces
                |= point
                |. P.spaces

        ParsingArc ->
            let
                arcCommand : ArcParameters -> Point -> CommandType
                arcCommand params endPoint =
                    ArcCommand endPoint params
            in
            P.succeed arcCommand
                |. P.spaces
                |= (P.succeed ArcParameters
                        |= point
                        |. P.spaces
                        |= float
                        |. P.spaces
                        |= P.oneOf
                            [ P.map (\_ -> Large) (P.symbol "1")
                            , P.map (\_ -> Small) (P.symbol "0")
                            ]
                        |. P.spaces
                        |= P.oneOf
                            [ P.map (\_ -> Clockwise) (P.symbol "1")
                            , P.map (\_ -> CounterClockwise) (P.symbol "0")
                            ]
                   )
                |. P.spaces
                |= point
                |. P.spaces

        ParsingClose ->
            P.problem "Should not arrive here"


createCommand : ParseInfo -> CommandType -> ParseInfo
createCommand info cmdType =
    let
        ( newState, isRelative ) =
            case info.state of
                ParsingCommand ir parsingCommandType ->
                    case parsingCommandType of
                        ParsingMove ->
                            ( ParsedCommand ir ParsingLine, ir )

                        _ ->
                            ( ParsedCommand ir parsingCommandType, ir )

                ParsedCommand ir _ ->
                    ( info.state, ir )

                _ ->
                    ( Invalid, False )

        currentPoint : Point
        currentPoint =
            info.currentPoint

        newCommand : Command
        newCommand =
            Command isRelative cmdType

        newSegment : Maybe Segment
        newSegment =
            segmentFromCommand
                { command = newCommand
                , currentPoint = currentPoint
                , firstConnectedPoint = info.firstConnectedPoint
                , lastCommandType =
                    Maybe.map
                        (\(Command _ ct) -> ct)
                        (List.head info.commands)
                , lastSegmentType =
                    Maybe.map
                        (\(Segment _ st) -> st)
                        (List.head info.path)
                }

        newPath : Path
        newPath =
            case newSegment of
                Just ns ->
                    ns :: info.path

                Nothing ->
                    info.path

        newCurrentPointFromEndPoint : Point -> Point
        newCurrentPointFromEndPoint endPoint =
            if isRelative then
                pointAdd currentPoint endPoint

            else
                endPoint

        newCurrentPoint : Point
        newCurrentPoint =
            case cmdType of
                MoveCommand endPoint ->
                    newCurrentPointFromEndPoint endPoint

                LineCommand endPoint ->
                    newCurrentPointFromEndPoint endPoint

                HorizontalLineCommand x ->
                    if isRelative then
                        pointAdd currentPoint { x = x, y = 0 }

                    else
                        { currentPoint | x = x }

                VerticalLineCommand y ->
                    if isRelative then
                        pointAdd currentPoint { x = 0, y = y }

                    else
                        { currentPoint | y = y }

                CubicCurveCommand endPoint _ ->
                    newCurrentPointFromEndPoint endPoint

                SmoothCubicCurveCommand endPoint _ ->
                    newCurrentPointFromEndPoint endPoint

                QuadraticCurveCommand endPoint _ ->
                    newCurrentPointFromEndPoint endPoint

                SmoothQuadraticCurveCommand endPoint ->
                    newCurrentPointFromEndPoint endPoint

                ArcCommand endPoint _ ->
                    newCurrentPointFromEndPoint endPoint

                CloseCommand ->
                    Maybe.withDefault origin info.firstConnectedPoint

        firstCommandType : Maybe CommandType
        firstCommandType =
            Maybe.map (\(Command _ ct) -> ct) (List.head info.commands)

        newFirstConnectedPoint : Maybe Point
        newFirstConnectedPoint =
            case ( firstCommandType, cmdType ) of
                ( _, MoveCommand _ ) ->
                    Nothing

                ( _, CloseCommand ) ->
                    Nothing

                ( Just (MoveCommand _), _ ) ->
                    Just info.currentPoint

                _ ->
                    info.firstConnectedPoint
    in
    { path = newPath
    , commands = newCommand :: info.commands
    , state = newState
    , currentPoint = newCurrentPoint
    , firstConnectedPoint = newFirstConnectedPoint
    }


parseStep : ParseInfo -> P.Parser ParseInfo
parseStep info =
    case info.state of
        NotStarted ->
            commandLetters info "M"

        ParsingCommand _ parsingCommandType ->
            P.map (createCommand info) (commandType parsingCommandType)

        ParsedCommand _ ParsingClose ->
            P.oneOf
                [ commandLetters info "MLHVCSQTA"
                , P.map (\_ -> info) (P.token " ")
                ]

        ParsedCommand _ parsingCommandType ->
            P.oneOf
                [ commandLetters info "MLHVCSQTAZ"
                , P.map (createCommand info) (commandType parsingCommandType)
                ]

        Invalid ->
            P.problem "Invalid command string"


parseCommandString : P.Parser ParseInfo
parseCommandString =
    let
        helper : ParseInfo -> P.Parser (P.Step ParseInfo ParseInfo)
        helper info =
            P.oneOf
                [ P.end
                    |> P.map
                        (\_ ->
                            P.Done
                                { info
                                    | path = List.reverse info.path
                                    , commands = List.reverse info.commands
                                }
                        )
                , P.succeed
                    (\s -> P.Loop s)
                    |= parseStep info
                ]
    in
    P.loop initParseInfo helper


fromString : String -> ( Path, Commands, String )
fromString string =
    case P.run parseCommandString string of
        Ok info ->
            ( info.path, info.commands, "No Errors Found" )

        Err _ ->
            ( [], [], "Errors!" )
