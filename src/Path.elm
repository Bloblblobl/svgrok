module Path exposing (..)

import Parser as P exposing ((|.), (|=))



-- TYPES


type alias Point =
    { x : Float, y : Float }


type alias PointPair =
    { start : Point, end : Point }


type alias Index2 =
    ( Int, Int )


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
    | CubicCurveCommand PointPair Point
    | SmoothCubicCurveCommand Point Point
    | QuadraticCurveCommand Point Point
    | SmoothQuadraticCurveCommand Point
    | ArcCommand ArcParameters Point
    | CloseCommand


type Command
    = Command Bool CommandType


type AbsoluteCommand
    = AbsoluteMove Point
    | AbsoluteLine Point
    | AbsoluteCubicCurve PointPair Point
    | AbsoluteQuadraticCurve Point Point
    | AbsoluteArc ArcParameters Point


type ParsingCommandType
    = ParsingMove Bool
    | ParsingLine Bool
    | ParsingHorizontalLine Bool
    | ParsingVerticalLine Bool
    | ParsingCubicCurve Bool
    | ParsingSmoothCubicCurve Bool
    | ParsingQuadraticCurve Bool
    | ParsingSmoothQuadraticCurve Bool
    | ParsingArc Bool
    | ParsingClose


type ParseState
    = NotStarted
    | ParsingCommand Bool ParsingCommandType


type alias ParseInfo =
    { commands : List Command
    , state : ParseState
    , currentPoint : Point
    , firstConnectedPoint : Maybe Point
    }


type alias InfoParser =
    P.Parser ParseInfo



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


pointScale : Float -> Point -> Point
pointScale scale { x, y } =
    { x = scale * x
    , y = scale * y
    }


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


absolutePoint : Point -> Bool -> Point -> Point
absolutePoint currentPoint isRelative p =
    if isRelative then
        pointAdd currentPoint p

    else
        p



-- TO STRING FUNCTIONS


pointToString : Point -> String
pointToString p =
    String.fromFloat p.x ++ "," ++ String.fromFloat p.y


pointPairToString : PointPair -> String
pointPairToString { start, end } =
    String.concat
        [ "start: "
        , wrapParens (pointToString start)
        , ", end: "
        , wrapParens (pointToString end)
        ]
        |> wrapBraces


arcSizeToString : ArcSize -> String
arcSizeToString size =
    case size of
        Large ->
            "1"

        Small ->
            "0"


arcRotationToString : ArcRotation -> String
arcRotationToString rotation =
    case rotation of
        Clockwise ->
            "1"

        CounterClockwise ->
            "0"


commandToString : Command -> String
commandToString (Command isRelative cmdType) =
    let
        letterCase : String -> String
        letterCase letter =
            if isRelative then
                String.toLower letter

            else
                letter
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

            CubicCurveCommand controls endPoint ->
                [ letterCase "C"
                , pointToString controls.start
                , pointToString controls.end
                , pointToString endPoint
                ]

            SmoothCubicCurveCommand control endPoint ->
                [ letterCase "S"
                , pointToString control
                , pointToString endPoint
                ]

            QuadraticCurveCommand control endPoint ->
                [ letterCase "Q"
                , pointToString control
                , pointToString endPoint
                ]

            SmoothQuadraticCurveCommand endPoint ->
                [ letterCase "T"
                , pointToString endPoint
                ]

            ArcCommand { radii, angle, size, rotation } endPoint ->
                [ letterCase "A"
                , String.fromFloat radii.x
                , String.fromFloat radii.y
                , String.fromFloat angle
                , arcSizeToString size
                , arcRotationToString rotation
                , pointToString endPoint
                ]

            CloseCommand ->
                [ letterCase "Z" ]
        )


commandsToString : List Command -> String
commandsToString commands =
    String.join " " (List.map commandToString commands)


absoluteCommandToString : AbsoluteCommand -> String
absoluteCommandToString absoluteCommand =
    case absoluteCommand of
        AbsoluteMove endPoint ->
            "M " ++ pointToString endPoint

        AbsoluteLine endPoint ->
            "L " ++ pointToString endPoint

        AbsoluteCubicCurve controls endPoint ->
            String.join " "
                [ "C"
                , pointToString controls.start
                , pointToString controls.end
                , pointToString endPoint
                ]

        AbsoluteQuadraticCurve control endPoint ->
            String.join " "
                [ "Q"
                , pointToString control
                , pointToString endPoint
                ]

        AbsoluteArc params endPoint ->
            String.join " "
                [ "A"
                , String.fromFloat params.radii.x
                , String.fromFloat params.radii.y
                , String.fromFloat params.angle
                , arcSizeToString params.size
                , arcRotationToString params.rotation
                , pointToString endPoint
                ]


absoluteCommandOverlayString : Point -> AbsoluteCommand -> String
absoluteCommandOverlayString currentPoint absoluteCommand =
    String.join " "
        [ absoluteCommandToString (AbsoluteMove currentPoint)
        , absoluteCommandToString absoluteCommand
        ]



-- PARSERS


initParseInfo : ParseInfo
initParseInfo =
    { commands = []
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


commandLetterToParsingType : List ( String, ParsingCommandType )
commandLetterToParsingType =
    [ ( "M", ParsingMove False )
    , ( "L", ParsingLine False )
    , ( "H", ParsingHorizontalLine False )
    , ( "V", ParsingVerticalLine False )
    , ( "C", ParsingCubicCurve False )
    , ( "S", ParsingSmoothCubicCurve False )
    , ( "Q", ParsingQuadraticCurve False )
    , ( "T", ParsingSmoothQuadraticCurve False )
    , ( "A", ParsingArc False )
    , ( "Z", ParsingClose )
    ]


commandLetter : ParseInfo -> ( String, ParsingCommandType ) -> InfoParser
commandLetter info ( letter, parseType ) =
    let
        commandCase : String -> Bool -> InfoParser
        commandCase letterCase isRelative =
            P.succeed ()
                |. P.token letterCase
                |> P.map
                    (\_ ->
                        { info | state = ParsingCommand isRelative parseType }
                    )
    in
    P.oneOf
        [ commandCase letter False
        , commandCase (String.toLower letter) True
        ]


moveCommandLetter : ParseInfo -> InfoParser
moveCommandLetter info =
    commandLetter info ( "M", ParsingMove False )


allCommandLetters : ParseInfo -> InfoParser
allCommandLetters info =
    P.oneOf <| List.map (commandLetter info) commandLetterToParsingType


updateInfoWithCommand : ParseInfo -> Bool -> CommandType -> ParseInfo
updateInfoWithCommand info isRelative commandType =
    let
        currentPoint : Point
        currentPoint =
            info.currentPoint

        newCommand : Command
        newCommand =
            Command isRelative commandType

        newState : ParseState
        newState =
            case commandType of
                MoveCommand _ ->
                    ParsingCommand isRelative (ParsingMove True)

                LineCommand _ ->
                    ParsingCommand isRelative (ParsingLine True)

                HorizontalLineCommand _ ->
                    ParsingCommand isRelative (ParsingHorizontalLine True)

                VerticalLineCommand _ ->
                    ParsingCommand isRelative (ParsingVerticalLine True)

                CubicCurveCommand _ _ ->
                    ParsingCommand isRelative (ParsingCubicCurve True)

                SmoothCubicCurveCommand _ _ ->
                    ParsingCommand isRelative (ParsingSmoothCubicCurve True)

                QuadraticCurveCommand _ _ ->
                    ParsingCommand isRelative (ParsingQuadraticCurve True)

                SmoothQuadraticCurveCommand _ ->
                    ParsingCommand isRelative (ParsingSmoothQuadraticCurve True)

                ArcCommand _ _ ->
                    ParsingCommand isRelative (ParsingArc True)

                CloseCommand ->
                    ParsingCommand isRelative ParsingClose

        newCurrentPointFromEndPoint : Point -> Point
        newCurrentPointFromEndPoint endPoint =
            if isRelative then
                pointAdd currentPoint endPoint

            else
                endPoint

        newCurrentPoint : Point
        newCurrentPoint =
            case commandType of
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

                CubicCurveCommand _ endPoint ->
                    newCurrentPointFromEndPoint endPoint

                SmoothCubicCurveCommand _ endPoint ->
                    newCurrentPointFromEndPoint endPoint

                QuadraticCurveCommand _ endPoint ->
                    newCurrentPointFromEndPoint endPoint

                SmoothQuadraticCurveCommand endPoint ->
                    newCurrentPointFromEndPoint endPoint

                ArcCommand _ endPoint ->
                    newCurrentPointFromEndPoint endPoint

                CloseCommand ->
                    Maybe.withDefault origin info.firstConnectedPoint

        firstCommandType : Maybe CommandType
        firstCommandType =
            Maybe.map (\(Command _ ct) -> ct) (List.head info.commands)

        newFirstConnectedPoint : Maybe Point
        newFirstConnectedPoint =
            case ( firstCommandType, commandType ) of
                ( _, MoveCommand _ ) ->
                    Nothing

                ( _, CloseCommand ) ->
                    Nothing

                ( Just (MoveCommand _), _ ) ->
                    Just info.currentPoint

                _ ->
                    info.firstConnectedPoint
    in
    { commands = newCommand :: info.commands
    , state = newState
    , currentPoint = newCurrentPoint
    , firstConnectedPoint = newFirstConnectedPoint
    }


moveCommand : Bool -> P.Parser CommandType
moveCommand parsedOne =
    let
        commandType : Point -> CommandType
        commandType =
            if parsedOne then
                LineCommand

            else
                MoveCommand
    in
    P.succeed commandType
        |. P.spaces
        |= point
        |. P.spaces


lineCommand : P.Parser CommandType
lineCommand =
    P.succeed LineCommand
        |. P.spaces
        |= point
        |. P.spaces


horizontalLineCommand : P.Parser CommandType
horizontalLineCommand =
    P.succeed HorizontalLineCommand
        |. P.spaces
        |= float
        |. P.spaces


verticalLineCommand : P.Parser CommandType
verticalLineCommand =
    P.succeed VerticalLineCommand
        |. P.spaces
        |= float
        |. P.spaces


cubicCurveCommand : P.Parser CommandType
cubicCurveCommand =
    P.succeed CubicCurveCommand
        |. P.spaces
        |= pointPair
        |. P.spaces
        |= point
        |. P.spaces


smoothCubicCurveCommand : P.Parser CommandType
smoothCubicCurveCommand =
    P.succeed SmoothCubicCurveCommand
        |. P.spaces
        |= point
        |. P.spaces
        |= point
        |. P.spaces


quadraticCurveCommand : P.Parser CommandType
quadraticCurveCommand =
    P.succeed QuadraticCurveCommand
        |. P.spaces
        |= point
        |. P.spaces
        |= point
        |. P.spaces


smoothQuadraticCurveCommand : P.Parser CommandType
smoothQuadraticCurveCommand =
    P.succeed SmoothQuadraticCurveCommand
        |. P.spaces
        |= point
        |. P.spaces


arcCommand : P.Parser CommandType
arcCommand =
    let
        arcSizeFlag : P.Parser ArcSize
        arcSizeFlag =
            P.oneOf
                [ P.map (\_ -> Large) (P.symbol "1")
                , P.map (\_ -> Small) (P.symbol "0")
                ]

        arcRotationFlag : P.Parser ArcRotation
        arcRotationFlag =
            P.oneOf
                [ P.map (\_ -> Clockwise) (P.symbol "1")
                , P.map (\_ -> CounterClockwise) (P.symbol "0")
                ]
    in
    P.succeed ArcCommand
        |. P.spaces
        |= (P.succeed ArcParameters
                |= point
                |. P.spaces
                |= float
                |. P.spaces
                |= arcSizeFlag
                |. P.spaces
                |= arcRotationFlag
           )
        |. P.spaces
        |= point
        |. P.spaces


parseStep : ParseInfo -> InfoParser
parseStep info =
    case info.state of
        NotStarted ->
            moveCommandLetter info

        ParsingCommand isRelative parsingCommandType ->
            let
                wrappedUpdateInfo : CommandType -> ParseInfo
                wrappedUpdateInfo =
                    updateInfoWithCommand info isRelative

                command : P.Parser CommandType -> Bool -> InfoParser
                command commandType parsedOne =
                    if parsedOne then
                        P.oneOf
                            [ P.map wrappedUpdateInfo commandType
                            , allCommandLetters info
                            ]

                    else
                        P.map wrappedUpdateInfo commandType
            in
            case parsingCommandType of
                ParsingMove parsedOne ->
                    command (moveCommand parsedOne) parsedOne

                ParsingLine parsedOne ->
                    command lineCommand parsedOne

                ParsingHorizontalLine parsedOne ->
                    command horizontalLineCommand parsedOne

                ParsingVerticalLine parsedOne ->
                    command verticalLineCommand parsedOne

                ParsingCubicCurve parsedOne ->
                    command cubicCurveCommand parsedOne

                ParsingSmoothCubicCurve parsedOne ->
                    command smoothCubicCurveCommand parsedOne

                ParsingQuadraticCurve parsedOne ->
                    command quadraticCurveCommand parsedOne

                ParsingSmoothQuadraticCurve parsedOne ->
                    command smoothQuadraticCurveCommand parsedOne

                ParsingArc parsedOne ->
                    command arcCommand parsedOne

                ParsingClose ->
                    let
                        updatedInfo : ParseInfo
                        updatedInfo =
                            updateInfoWithCommand info isRelative CloseCommand
                    in
                    P.oneOf
                        [ allCommandLetters updatedInfo
                        , P.map (\_ -> updatedInfo) (P.token " ")
                        ]


parseCommandString : InfoParser
parseCommandString =
    let
        helper : ParseInfo -> P.Parser (P.Step ParseInfo ParseInfo)
        helper info =
            P.oneOf
                [ P.end
                    |> P.map
                        (\_ ->
                            P.Done
                                { info | commands = List.reverse info.commands }
                        )
                , P.succeed
                    (\s -> P.Loop s)
                    |= parseStep info
                ]
    in
    P.loop initParseInfo helper


fromString : String -> ( List Command, String )
fromString string =
    case P.run parseCommandString string of
        Ok info ->
            ( info.commands, "No Errors Found" )

        Err _ ->
            ( [], "Errors!" )



-- UPDATE FUNCTIONS


updateCommand : Int -> Point -> Command -> Command
updateCommand secondary newPoint (Command isRelative commandType) =
    let
        currentCommand : Command
        currentCommand =
            Command isRelative commandType

        updateCommandType : CommandType -> Command
        updateCommandType updatedCommmandType =
            Command isRelative updatedCommmandType

        adjustedPoint : Point -> Point
        adjustedPoint currentPoint =
            if isRelative then
                pointSubtract newPoint currentPoint

            else
                newPoint
    in
    case commandType of
        MoveCommand currentPoint ->
            updateCommandType <| MoveCommand (adjustedPoint currentPoint)

        LineCommand currentPoint ->
            updateCommandType <| LineCommand (adjustedPoint currentPoint)

        HorizontalLineCommand x ->
            let
                newX : Float
                newX =
                    if isRelative then
                        newPoint.x - x

                    else
                        x
            in
            updateCommandType (HorizontalLineCommand newX)

        VerticalLineCommand y ->
            let
                newY : Float
                newY =
                    if isRelative then
                        newPoint.y - y

                    else
                        y
            in
            updateCommandType (HorizontalLineCommand newY)

        CubicCurveCommand controls currentPoint ->
            if secondary == 1 then
                updateCommandType <|
                    CubicCurveCommand
                        { controls | start = adjustedPoint controls.start }
                        currentPoint

            else if secondary == 2 then
                updateCommandType <|
                    CubicCurveCommand
                        { controls | end = adjustedPoint controls.end }
                        currentPoint

            else if secondary == 3 then
                updateCommandType <|
                    CubicCurveCommand controls (adjustedPoint currentPoint)

            else
                currentCommand

        SmoothCubicCurveCommand control currentPoint ->
            if secondary == 1 then
                updateCommandType <|
                    CubicCurveCommand
                        { start = adjustedPoint currentPoint, end = control }
                        currentPoint

            else if secondary == 2 then
                updateCommandType <|
                    SmoothCubicCurveCommand (adjustedPoint control) currentPoint

            else if secondary == 3 then
                updateCommandType <|
                    SmoothCubicCurveCommand control (adjustedPoint currentPoint)

            else
                currentCommand

        QuadraticCurveCommand control currentPoint ->
            if secondary == 1 then
                updateCommandType <|
                    QuadraticCurveCommand (adjustedPoint control) currentPoint

            else if secondary == 2 then
                updateCommandType <|
                    QuadraticCurveCommand control (adjustedPoint currentPoint)

            else
                currentCommand

        SmoothQuadraticCurveCommand currentPoint ->
            if secondary == 1 then
                updateCommandType <|
                    QuadraticCurveCommand
                        (adjustedPoint currentPoint)
                        currentPoint

            else if secondary == 2 then
                updateCommandType <|
                    SmoothQuadraticCurveCommand (adjustedPoint currentPoint)

            else
                currentCommand

        ArcCommand params currentPoint ->
            updateCommandType <|
                ArcCommand params (adjustedPoint currentPoint)

        CloseCommand ->
            currentCommand


updateCommandsAtIndex : Index2 -> Point -> List Command -> List Command
updateCommandsAtIndex ( primary, secondary ) newPoint commands =
    let
        validUpdate : Bool
        validUpdate =
            primary < List.length commands && secondary > 0

        targetCommand : Command
        targetCommand =
            List.drop primary commands
                |> List.head
                |> Maybe.withDefault (Command False CloseCommand)

        updatedCommand : Command
        updatedCommand =
            updateCommand secondary newPoint targetCommand

        updatedCommands : List Command
        updatedCommands =
            List.concat
                [ List.take primary commands
                , updatedCommand :: List.drop (primary + 1) commands
                ]
    in
    if validUpdate then
        updatedCommands

    else
        commands



-- TODO: move this somewhere else


type alias Rect =
    { x : Float, y : Float, width : Float, height : Float }


parseViewBoxString : P.Parser Rect
parseViewBoxString =
    P.succeed Rect
        |. P.spaces
        |= float
        |. P.spaces
        |= float
        |. P.spaces
        |= float
        |. P.spaces
        |= float


viewBoxRectFromString : String -> Rect
viewBoxRectFromString string =
    case P.run parseViewBoxString string of
        Ok rect ->
            rect

        Err _ ->
            { x = 0, y = 0, width = 0, height = 0 }



-- CONVERSION TO ABSOLUTE COMMANDS


type alias ResolveInfo =
    { absoluteCommands : List AbsoluteCommand
    , currentPoint : Point
    , firstConnectedPoint : Point
    , previousAbsoluteCommand : Maybe AbsoluteCommand
    }


initResolveInfo : ResolveInfo
initResolveInfo =
    { absoluteCommands = []
    , currentPoint = origin
    , firstConnectedPoint = origin
    , previousAbsoluteCommand = Nothing
    }


resolveStep : Command -> ResolveInfo -> ResolveInfo
resolveStep (Command isRelative commandType) info =
    case commandType of
        MoveCommand endPoint ->
            let
                absoluteEndPoint : Point
                absoluteEndPoint =
                    absolutePoint info.currentPoint isRelative endPoint

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteMove absoluteEndPoint
            in
            { absoluteCommands = absoluteCommand :: info.absoluteCommands
            , currentPoint = absoluteEndPoint
            , firstConnectedPoint = absoluteEndPoint
            , previousAbsoluteCommand = Just absoluteCommand
            }

        LineCommand endPoint ->
            let
                absoluteEndPoint : Point
                absoluteEndPoint =
                    absolutePoint info.currentPoint isRelative endPoint

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteLine absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        HorizontalLineCommand x ->
            let
                currentPoint : Point
                currentPoint =
                    info.currentPoint

                absoluteEndPoint : Point
                absoluteEndPoint =
                    if isRelative then
                        { currentPoint | x = currentPoint.x + x }

                    else
                        { currentPoint | x = x }

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteLine absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        VerticalLineCommand y ->
            let
                currentPoint : Point
                currentPoint =
                    info.currentPoint

                absoluteEndPoint : Point
                absoluteEndPoint =
                    if isRelative then
                        { currentPoint | y = currentPoint.y + y }

                    else
                        { currentPoint | y = y }

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteLine absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        CubicCurveCommand { start, end } endPoint ->
            let
                absoluteEndPoint : Point
                absoluteEndPoint =
                    absolutePoint info.currentPoint isRelative endPoint

                absoluteControls : PointPair
                absoluteControls =
                    { start = absolutePoint info.currentPoint isRelative start
                    , end = absolutePoint info.currentPoint isRelative end
                    }

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteCubicCurve absoluteControls absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        SmoothCubicCurveCommand control endPoint ->
            let
                absoluteEndPoint : Point
                absoluteEndPoint =
                    absolutePoint info.currentPoint isRelative endPoint

                controlStart : Point
                controlStart =
                    case info.previousAbsoluteCommand of
                        Just (AbsoluteCubicCurve { end } _) ->
                            pointSubtract (pointScale 2 info.currentPoint) end

                        _ ->
                            info.currentPoint

                absoluteControls : PointPair
                absoluteControls =
                    { start = controlStart
                    , end = absolutePoint info.currentPoint isRelative control
                    }

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteCubicCurve absoluteControls absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        QuadraticCurveCommand control endPoint ->
            let
                absoluteEndPoint : Point
                absoluteEndPoint =
                    absolutePoint info.currentPoint isRelative endPoint

                absoluteControl : Point
                absoluteControl =
                    absolutePoint info.currentPoint isRelative control

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteQuadraticCurve absoluteControl absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        SmoothQuadraticCurveCommand endPoint ->
            let
                absoluteEndPoint : Point
                absoluteEndPoint =
                    absolutePoint info.currentPoint isRelative endPoint

                control : Point
                control =
                    case info.previousAbsoluteCommand of
                        Just (AbsoluteQuadraticCurve previousControl _) ->
                            pointSubtract
                                (pointScale 2 info.currentPoint)
                                previousControl

                        _ ->
                            info.currentPoint

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteQuadraticCurve control absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        ArcCommand parameters endPoint ->
            let
                absoluteEndPoint : Point
                absoluteEndPoint =
                    absolutePoint info.currentPoint isRelative endPoint

                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteArc parameters absoluteEndPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = absoluteEndPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }

        CloseCommand ->
            let
                absoluteCommand : AbsoluteCommand
                absoluteCommand =
                    AbsoluteLine info.firstConnectedPoint
            in
            { info
                | absoluteCommands = absoluteCommand :: info.absoluteCommands
                , currentPoint = info.firstConnectedPoint
                , previousAbsoluteCommand = Just absoluteCommand
            }


resolveRawCommands : List Command -> List AbsoluteCommand
resolveRawCommands commands =
    List.foldl resolveStep initResolveInfo commands
        |> .absoluteCommands
        |> List.reverse
