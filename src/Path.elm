module Path exposing (..)

import Parser exposing ((|.), (|=))



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
    = MoveCmd Point
    | LineCmd Point
    | CubicCurveCmd Point PointPair
    | SmoothCubicCurveCmd Point Point
    | QuadraticCurveCmd Point Point
    | SmoothQuadraticCurveCmd Point
    | ArcCmd Point ArcParameters
    | CloseCmd


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


type alias PathWithPoint =
    { path : Path
    , currentPoint : Point
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
            MoveCmd endPoint ->
                [ letterCase "M"
                , pointToString endPoint
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
                    , pointToString endPoint
                    ]

            CubicCurveCmd endPoint controls ->
                [ letterCase "C"
                , pointToString controls.start
                , pointToString controls.end
                , pointToString endPoint
                ]

            SmoothCubicCurveCmd endPoint control ->
                [ letterCase "S"
                , pointToString control
                , pointToString endPoint
                ]

            QuadraticCurveCmd endPoint control ->
                [ letterCase "Q"
                , pointToString control
                , pointToString endPoint
                ]

            SmoothQuadraticCurveCmd endPoint ->
                [ letterCase "T"
                , pointToString endPoint
                ]

            ArcCmd endPoint { radii, angle, size, rotation } ->
                [ letterCase "A"
                , String.fromFloat radii.x
                , String.fromFloat radii.y
                , String.fromFloat angle
                , arcSizeString size
                , arcRotationString rotation
                , pointToString endPoint
                ]

            CloseCmd ->
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
                [ Command isRelative (MoveCmd points.start)
                , Command isRelative cmdType
                ]

            else
                [ Command isRelative cmdType ]
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


segmentFromCommand : Command -> PathWithPoint -> PathWithPoint
segmentFromCommand (Command isRelative cmdType) currentPathWithPoint =
    let
        currentPath : Path
        currentPath =
            currentPathWithPoint.path

        targetPoints : Point -> PointPair
        targetPoints endPoint =
            { start = currentPathWithPoint.currentPoint
            , end =
                if isRelative then
                    pointAdd currentPathWithPoint.currentPoint endPoint

                else
                    endPoint
            }

        lastSegmentType : Maybe SegmentType
        lastSegmentType =
            Maybe.map
                (\(Segment _ lastType) -> lastType)
                (List.head currentPath)

        previousControlPoint : Maybe Point
        previousControlPoint =
            case ( cmdType, lastSegmentType ) of
                ( CubicCurveCmd _ _, Just (CubicCurve controls) ) ->
                    Just controls.end

                ( QuadraticCurveCmd _ _, Just (QuadraticCurve control) ) ->
                    Just control

                _ ->
                    Nothing
    in
    case cmdType of
        MoveCmd endPoint ->
            { currentPathWithPoint
                | currentPoint = (targetPoints endPoint).end
            }

        LineCmd endPoint ->
            { path = Segment (targetPoints endPoint) Line :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        CubicCurveCmd endPoint controls ->
            let
                targetControls : PointPair
                targetControls =
                    if isRelative then
                        { start =
                            pointAdd
                                (targetPoints endPoint).start
                                controls.start
                        , end =
                            pointAdd
                                (targetPoints endPoint).start
                                controls.end
                        }

                    else
                        controls
            in
            { path =
                Segment (targetPoints endPoint) (CubicCurve targetControls)
                    :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        SmoothCubicCurveCmd endPoint control ->
            let
                controlDiff : Point
                controlDiff =
                    pointSubtract
                        (targetPoints endPoint).start
                        (targetPoints endPoint).end

                controlStart : Point
                controlStart =
                    case previousControlPoint of
                        Just p ->
                            pointAdd p controlDiff

                        Nothing ->
                            (targetPoints endPoint).start

                targetControls : PointPair
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
            { path =
                Segment (targetPoints endPoint) (CubicCurve targetControls)
                    :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        QuadraticCurveCmd endPoint control ->
            let
                targetControl : Point
                targetControl =
                    if isRelative then
                        pointAdd (targetPoints endPoint).start control

                    else
                        control
            in
            { path =
                Segment (targetPoints endPoint) (QuadraticCurve targetControl)
                    :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        SmoothQuadraticCurveCmd endPoint ->
            let
                controlDiff : Point
                controlDiff =
                    pointSubtract
                        (targetPoints endPoint).start
                        (targetPoints endPoint).end

                targetControl : Point
                targetControl =
                    case previousControlPoint of
                        Just p ->
                            pointAdd p controlDiff

                        Nothing ->
                            (targetPoints endPoint).start
            in
            { path =
                Segment (targetPoints endPoint) (QuadraticCurve targetControl)
                    :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        ArcCmd endPoint parameters ->
            { path =
                Segment (targetPoints endPoint) (Arc parameters)
                    :: currentPath
            , currentPoint = (targetPoints endPoint).end
            }

        CloseCmd ->
            let
                firstSegmentHelper : Path -> Maybe Segment
                firstSegmentHelper remainingPath =
                    case remainingPath of
                        segment :: [] ->
                            Just segment

                        _ :: rest ->
                            firstSegmentHelper rest

                        [] ->
                            Nothing

                firstSegment : Maybe Segment
                firstSegment =
                    firstSegmentHelper currentPath
            in
            case firstSegment of
                Just (Segment points _) ->
                    let
                        closingLinePoints : PointPair
                        closingLinePoints =
                            { start = currentPathWithPoint.currentPoint
                            , end = points.start
                            }
                    in
                    { path = Segment closingLinePoints Line :: currentPath
                    , currentPoint = points.start
                    }

                Nothing ->
                    currentPathWithPoint


fromCommands : Commands -> Path
fromCommands cmds =
    List.foldl segmentFromCommand { path = [], currentPoint = origin } cmds
        |> .path
        |> List.reverse



-- PARSERS


float : Parser.Parser Float
float =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.float
        , Parser.float
        ]


point : Parser.Parser Point
point =
    Parser.succeed Point
        |= float
        |. Parser.spaces
        |. Parser.oneOf
            [ Parser.symbol ","
            , Parser.succeed ()
            ]
        |. Parser.spaces
        |= float


pointPair : Parser.Parser PointPair
pointPair =
    Parser.succeed PointPair
        |= point
        |. Parser.spaces
        |= point


moveCommandType : Parser.Parser CommandType
moveCommandType =
    Parser.succeed MoveCmd
        |. Parser.spaces
        |= point
        |. Parser.spaces


lineCommandType : Parser.Parser CommandType
lineCommandType =
    Parser.succeed LineCmd
        |. Parser.spaces
        |= point
        |. Parser.spaces


horizontalLineCommandType : Parser.Parser CommandType
horizontalLineCommandType =
    Parser.succeed LineCmd
        |. Parser.spaces
        |= Parser.map (\dx -> { x = dx, y = 0 }) float
        |. Parser.spaces


verticalLineCommandType : Parser.Parser CommandType
verticalLineCommandType =
    Parser.succeed LineCmd
        |. Parser.spaces
        |= Parser.map (\dy -> { x = 0, y = dy }) float
        |. Parser.spaces


cubicCurveCommandType : Parser.Parser CommandType
cubicCurveCommandType =
    Parser.succeed CubicCurveCmd
        |. Parser.spaces
        |= point
        |. Parser.spaces
        |= pointPair
        |. Parser.spaces


smoothCubicCurveCommandType : Parser.Parser CommandType
smoothCubicCurveCommandType =
    Parser.succeed SmoothCubicCurveCmd
        |. Parser.spaces
        |= point
        |. Parser.spaces
        |= point
        |. Parser.spaces


quadraticCurveCommandType : Parser.Parser CommandType
quadraticCurveCommandType =
    Parser.succeed QuadraticCurveCmd
        |. Parser.spaces
        |= point
        |. Parser.spaces
        |= point
        |. Parser.spaces


smoothQuadraticCurveCommandType : Parser.Parser CommandType
smoothQuadraticCurveCommandType =
    Parser.succeed SmoothQuadraticCurveCmd
        |. Parser.spaces
        |= point
        |. Parser.spaces


arcCommandType : Parser.Parser CommandType
arcCommandType =
    let
        arcSizeFlag : Parser.Parser ArcSize
        arcSizeFlag =
            Parser.oneOf
                [ Parser.map (\_ -> Large) (Parser.symbol "1")
                , Parser.map (\_ -> Small) (Parser.symbol "0")
                ]

        arcRotationFlag : Parser.Parser ArcRotation
        arcRotationFlag =
            Parser.oneOf
                [ Parser.map (\_ -> Clockwise) (Parser.symbol "1")
                , Parser.map (\_ -> CounterClockwise) (Parser.symbol "0")
                ]

        arcParameters : Parser.Parser ArcParameters
        arcParameters =
            Parser.succeed ArcParameters
                |= point
                |. Parser.spaces
                |= float
                |. Parser.spaces
                |= arcSizeFlag
                |. Parser.spaces
                |= arcRotationFlag
    in
    Parser.succeed ArcCmd
        |. Parser.spaces
        |= point
        |. Parser.spaces
        |= arcParameters
        |. Parser.spaces


closeCommandType : Parser.Parser CommandType
closeCommandType =
    Parser.succeed CloseCmd
        |. Parser.spaces


commandType : String -> Parser.Parser CommandType -> Parser.Parser Command
commandType commandLetter cmdType =
    Parser.succeed Command
        |= Parser.oneOf
            [ Parser.map
                (\_ -> False)
                (Parser.symbol commandLetter)
            , Parser.map
                (\_ -> True)
                (Parser.symbol (String.toLower commandLetter))
            ]
        |= cmdType


command : Parser.Parser Command
command =
    Parser.oneOf
        [ commandType "M" moveCommandType
        , commandType "L" lineCommandType
        , commandType "H" horizontalLineCommandType
        , commandType "V" verticalLineCommandType
        , commandType "C" cubicCurveCommandType
        , commandType "S" smoothCubicCurveCommandType
        , commandType "Q" quadraticCurveCommandType
        , commandType "T" smoothQuadraticCurveCommandType
        , commandType "A" arcCommandType
        , commandType "Z" closeCommandType
        ]


commands : Parser.Parser Commands
commands =
    let
        helper : Commands -> Parser.Parser (Parser.Step Commands Commands)
        helper reverseCommands =
            Parser.oneOf
                [ Parser.succeed
                    (\cmd -> Parser.Loop (cmd :: reverseCommands))
                    |= command
                , Parser.succeed ()
                    |> Parser.map
                        (\_ -> Parser.Done (List.reverse reverseCommands))
                ]
    in
    Parser.loop [] helper


commandsFromString : String -> ( Commands, String )
commandsFromString string =
    case Parser.run commands string of
        Ok cmds ->
            ( cmds, "No Errors Found" )

        Err _ ->
            ( [], "Errors!" )
