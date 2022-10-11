module Old.Path exposing (..)

import Parser as P exposing ((|.), (|=))
import Point exposing (Point)



-- TYPES


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
    | CubicCurveCommand Point.Pair Point
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
    | AbsoluteCubicCurve Point.Pair Point
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


type alias TraversalPoints =
    { currentPoint : Point, firstConnectedPoint : Point }



-- POINT FUNCTIONS


absolutePoint : Point -> Bool -> Point -> Point
absolutePoint currentPoint isRelative p =
    if isRelative then
        Point.add currentPoint p

    else
        p


getEndPoint : List Command -> Point
getEndPoint commands =
    let
        initialPoints : TraversalPoints
        initialPoints =
            { currentPoint = Point.zero
            , firstConnectedPoint = Point.zero
            }

        traverse : Command -> TraversalPoints -> TraversalPoints
        traverse (Command isRelative commandType) traversalPoints =
            let
                currentPoint : Point
                currentPoint =
                    traversalPoints.currentPoint

                adjustPoint : Point -> Point
                adjustPoint endPoint =
                    if isRelative then
                        Point.add traversalPoints.currentPoint endPoint

                    else
                        endPoint
            in
            case commandType of
                MoveCommand endPoint ->
                    { currentPoint = adjustPoint endPoint
                    , firstConnectedPoint = adjustPoint endPoint
                    }

                LineCommand endPoint ->
                    { traversalPoints | currentPoint = adjustPoint endPoint }

                HorizontalLineCommand x ->
                    let
                        newPoint : Point
                        newPoint =
                            if isRelative then
                                { currentPoint | x = currentPoint.x + x }

                            else
                                { currentPoint | x = x }
                    in
                    { traversalPoints | currentPoint = newPoint }

                VerticalLineCommand y ->
                    let
                        newPoint : Point
                        newPoint =
                            if isRelative then
                                { currentPoint | y = currentPoint.y + y }

                            else
                                { currentPoint | y = y }
                    in
                    { traversalPoints | currentPoint = newPoint }

                CubicCurveCommand _ endPoint ->
                    { traversalPoints | currentPoint = adjustPoint endPoint }

                SmoothCubicCurveCommand _ endPoint ->
                    { traversalPoints | currentPoint = adjustPoint endPoint }

                QuadraticCurveCommand _ endPoint ->
                    { traversalPoints | currentPoint = adjustPoint endPoint }

                SmoothQuadraticCurveCommand endPoint ->
                    { traversalPoints | currentPoint = adjustPoint endPoint }

                ArcCommand _ endPoint ->
                    { traversalPoints | currentPoint = adjustPoint endPoint }

                CloseCommand ->
                    { traversalPoints
                        | currentPoint = traversalPoints.firstConnectedPoint
                    }
    in
    .currentPoint (List.foldl traverse initialPoints commands)



-- TO STRING FUNCTIONS


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
                , Point.toString endPoint
                ]

            LineCommand endPoint ->
                [ letterCase "L"
                , Point.toString endPoint
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
                , Point.toString controls.start
                , Point.toString controls.end
                , Point.toString endPoint
                ]

            SmoothCubicCurveCommand control endPoint ->
                [ letterCase "S"
                , Point.toString control
                , Point.toString endPoint
                ]

            QuadraticCurveCommand control endPoint ->
                [ letterCase "Q"
                , Point.toString control
                , Point.toString endPoint
                ]

            SmoothQuadraticCurveCommand endPoint ->
                [ letterCase "T"
                , Point.toString endPoint
                ]

            ArcCommand { radii, angle, size, rotation } endPoint ->
                [ letterCase "A"
                , String.fromFloat radii.x
                , String.fromFloat radii.y
                , String.fromFloat angle
                , arcSizeToString size
                , arcRotationToString rotation
                , Point.toString endPoint
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
            "M " ++ Point.toString endPoint

        AbsoluteLine endPoint ->
            "L " ++ Point.toString endPoint

        AbsoluteCubicCurve controls endPoint ->
            String.join " "
                [ "C"
                , Point.toString controls.start
                , Point.toString controls.end
                , Point.toString endPoint
                ]

        AbsoluteQuadraticCurve control endPoint ->
            String.join " "
                [ "Q"
                , Point.toString control
                , Point.toString endPoint
                ]

        AbsoluteArc params endPoint ->
            String.join " "
                [ "A"
                , String.fromFloat params.radii.x
                , String.fromFloat params.radii.y
                , String.fromFloat params.angle
                , arcSizeToString params.size
                , arcRotationToString params.rotation
                , Point.toString endPoint
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
    , currentPoint = Point.zero
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


pointPair : P.Parser Point.Pair
pointPair =
    P.succeed Point.Pair
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
        newState : Bool -> ParseInfo
        newState isRelative =
            if letter == "Z" then
                updateInfoWithCommand
                    { info | state = ParsingCommand isRelative parseType }
                    isRelative
                    CloseCommand

            else
                { info | state = ParsingCommand isRelative parseType }

        commandCase : String -> Bool -> InfoParser
        commandCase letterCase isRelative =
            P.succeed ()
                |. P.token letterCase
                |> P.map (\_ -> newState isRelative)
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
                Point.add currentPoint endPoint

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
                        Point.add currentPoint { x = x, y = 0 }

                    else
                        { currentPoint | x = x }

                VerticalLineCommand y ->
                    if isRelative then
                        Point.add currentPoint { x = 0, y = y }

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
                    Maybe.withDefault Point.zero info.firstConnectedPoint

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
                    P.oneOf
                        [ allCommandLetters info
                        , P.map (\_ -> info) (P.token " ")
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


type alias UpdatedCommand =
    { command : Command
    , pointDifference : Point
    }


endPointIndex : Command -> Int
endPointIndex (Command _ commandType) =
    case commandType of
        MoveCommand _ ->
            1

        LineCommand _ ->
            1

        HorizontalLineCommand _ ->
            1

        VerticalLineCommand _ ->
            1

        CubicCurveCommand _ _ ->
            3

        SmoothCubicCurveCommand _ _ ->
            3

        QuadraticCurveCommand _ _ ->
            2

        SmoothQuadraticCurveCommand _ ->
            2

        ArcCommand _ _ ->
            1

        CloseCommand ->
            1


updateEndPoint : Point -> Command -> ( Command, Point )
updateEndPoint newPoint (Command isRelative commandType) =
    let
        updateCommandType : CommandType -> Command
        updateCommandType updatedCommmandType =
            Command isRelative updatedCommmandType
    in
    case commandType of
        MoveCommand currentPoint ->
            ( updateCommandType (MoveCommand newPoint)
            , Point.subtract newPoint currentPoint
            )

        LineCommand currentPoint ->
            ( updateCommandType (LineCommand newPoint)
            , Point.subtract newPoint currentPoint
            )

        HorizontalLineCommand currentX ->
            ( updateCommandType (LineCommand newPoint)
            , { x = newPoint.x - currentX, y = 0 }
            )

        VerticalLineCommand currentY ->
            ( updateCommandType (LineCommand newPoint)
            , { x = 0, y = newPoint.y - currentY }
            )

        CubicCurveCommand controls currentPoint ->
            ( updateCommandType (CubicCurveCommand controls newPoint)
            , Point.subtract newPoint currentPoint
            )

        SmoothCubicCurveCommand control currentPoint ->
            ( updateCommandType (SmoothCubicCurveCommand control newPoint)
            , Point.subtract newPoint currentPoint
            )

        QuadraticCurveCommand control currentPoint ->
            ( updateCommandType (QuadraticCurveCommand control newPoint)
            , Point.subtract newPoint currentPoint
            )

        SmoothQuadraticCurveCommand currentPoint ->
            ( updateCommandType (SmoothQuadraticCurveCommand newPoint)
            , Point.subtract newPoint currentPoint
            )

        ArcCommand parameters currentPoint ->
            ( updateCommandType (ArcCommand parameters newPoint)
            , Point.subtract newPoint currentPoint
            )

        CloseCommand ->
            ( Command isRelative CloseCommand
            , Point.zero
            )


updateStartControl : Point -> Command -> Command
updateStartControl newPoint (Command isRelative commandType) =
    let
        updateCommandType : CommandType -> Command
        updateCommandType updatedCommmandType =
            Command isRelative updatedCommmandType
    in
    case commandType of
        CubicCurveCommand controls endPoint ->
            updateCommandType <|
                CubicCurveCommand
                    { controls | start = newPoint }
                    endPoint

        SmoothCubicCurveCommand control endPoint ->
            updateCommandType <|
                CubicCurveCommand
                    { start = newPoint, end = control }
                    endPoint

        _ ->
            Command isRelative commandType


updateEndControl : Point -> Command -> Command
updateEndControl newPoint (Command isRelative commandType) =
    let
        updateCommandType : CommandType -> Command
        updateCommandType updatedCommmandType =
            Command isRelative updatedCommmandType
    in
    case commandType of
        CubicCurveCommand controls endPoint ->
            updateCommandType <|
                CubicCurveCommand
                    { controls | end = newPoint }
                    endPoint

        SmoothCubicCurveCommand _ endPoint ->
            updateCommandType <| SmoothCubicCurveCommand newPoint endPoint

        _ ->
            Command isRelative commandType


updateControl : Point -> Command -> Command
updateControl newPoint (Command isRelative commandType) =
    let
        updateCommandType : CommandType -> Command
        updateCommandType updatedCommmandType =
            Command isRelative updatedCommmandType
    in
    case commandType of
        QuadraticCurveCommand _ endPoint ->
            updateCommandType <| QuadraticCurveCommand newPoint endPoint

        SmoothQuadraticCurveCommand endPoint ->
            updateCommandType <| QuadraticCurveCommand newPoint endPoint

        _ ->
            Command isRelative commandType


updateRelativeCommand : Point -> Command -> Command
updateRelativeCommand pointDifference (Command _ commandType) =
    let
        updatePoint : Point -> Point
        updatePoint oldPoint =
            Point.subtract oldPoint pointDifference

        updateCommandType : CommandType -> Command
        updateCommandType updatedCommmandType =
            Command True updatedCommmandType
    in
    case commandType of
        MoveCommand endPoint ->
            updateCommandType <| MoveCommand (updatePoint endPoint)

        LineCommand endPoint ->
            updateCommandType <| LineCommand (updatePoint endPoint)

        HorizontalLineCommand x ->
            updateCommandType <| HorizontalLineCommand (x - pointDifference.x)

        VerticalLineCommand y ->
            updateCommandType <| VerticalLineCommand (y - pointDifference.y)

        CubicCurveCommand { start, end } endPoint ->
            updateCommandType <|
                CubicCurveCommand
                    { start = updatePoint start, end = updatePoint end }
                    (updatePoint endPoint)

        SmoothCubicCurveCommand control endPoint ->
            updateCommandType <|
                SmoothCubicCurveCommand
                    (updatePoint control)
                    (updatePoint endPoint)

        QuadraticCurveCommand control endPoint ->
            updateCommandType <|
                QuadraticCurveCommand
                    (updatePoint control)
                    (updatePoint endPoint)

        SmoothQuadraticCurveCommand endPoint ->
            updateCommandType <|
                SmoothQuadraticCurveCommand (updatePoint endPoint)

        ArcCommand parameters endPoint ->
            updateCommandType <| ArcCommand parameters (updatePoint endPoint)

        CloseCommand ->
            updateCommandType commandType


type CommandComponent
    = EndPoint
    | StartControl
    | EndControl
    | Control


getCommandComponent : Int -> Command -> CommandComponent
getCommandComponent secondary (Command _ commandType) =
    case ( commandType, secondary ) of
        ( CubicCurveCommand _ _, 1 ) ->
            StartControl

        ( SmoothCubicCurveCommand _ _, 1 ) ->
            StartControl

        ( CubicCurveCommand _ _, 2 ) ->
            EndControl

        ( SmoothCubicCurveCommand _ _, 2 ) ->
            EndControl

        ( QuadraticCurveCommand _ _, 1 ) ->
            Control

        ( SmoothQuadraticCurveCommand _, 1 ) ->
            Control

        _ ->
            EndPoint


updateCommand : Int -> Point -> Command -> List Command -> List Command
updateCommand secondary newPoint command nextCommands =
    let
        commandComponent : CommandComponent
        commandComponent =
            getCommandComponent secondary command

        updatedCommand : Command
        updatedCommand =
            case commandComponent of
                EndPoint ->
                    Tuple.first (updateEndPoint newPoint command)

                StartControl ->
                    updateStartControl newPoint command

                EndControl ->
                    updateEndControl newPoint command

                Control ->
                    updateControl newPoint command

        updateNextCommand : Command -> Command
        updateNextCommand (Command isRelative commandType) =
            let
                nextCommand : Command
                nextCommand =
                    Command isRelative commandType

                pointDifference : Point
                pointDifference =
                    Tuple.second (updateEndPoint newPoint command)
            in
            case ( isRelative, commandComponent ) of
                ( True, EndPoint ) ->
                    updateRelativeCommand pointDifference nextCommand

                _ ->
                    nextCommand

        updatedNextCommand : Maybe Command
        updatedNextCommand =
            Maybe.map updateNextCommand (List.head nextCommands)

        updatedNextCommands : List Command
        updatedNextCommands =
            case updatedNextCommand of
                Just nextCommand ->
                    nextCommand :: List.drop 1 nextCommands

                Nothing ->
                    nextCommands
    in
    updatedCommand :: updatedNextCommands


updateCommands : Index2 -> Point -> List Command -> List Command
updateCommands ( primary, secondary ) newAbsolutePoint commands =
    let
        previousCommands : List Command
        previousCommands =
            List.take primary commands

        targetCommand : Maybe Command
        targetCommand =
            List.head (List.drop primary commands)

        nextCommands : List Command
        nextCommands =
            List.drop (primary + 1) commands
    in
    case targetCommand of
        Just (Command True commandType) ->
            let
                command : Command
                command =
                    Command True commandType

                newPoint : Point
                newPoint =
                    Point.subtract
                        newAbsolutePoint
                        (getEndPoint previousCommands)
            in
            List.append
                previousCommands
                (updateCommand secondary newPoint command nextCommands)

        Just (Command False commandType) ->
            let
                command : Command
                command =
                    Command False commandType
            in
            List.append
                previousCommands
                (updateCommand secondary newAbsolutePoint command nextCommands)

        Nothing ->
            commands



-- UTIL FUNCTIONS
-- TODO: move this somewhere else


type alias Grid =
    { horizontal : Int, vertical : Int }


type alias Rect =
    { x : Float, y : Float, width : Float, height : Float }


snapToGrid : Grid -> Point -> Point
snapToGrid grid { x, y } =
    let
        snap : Int -> Float -> Float
        snap spacing value =
            toFloat <| spacing * round (value / toFloat spacing)
    in
    { x = snap grid.horizontal x, y = snap grid.vertical y }


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


{-| Rounds the number to the specified significant digits.
-}
roundSignificantly : Int -> Float -> Float
roundSignificantly digits number =
    let
        roundedString : String
        roundedString =
            number
                * toFloat (10 ^ digits)
                |> round
                |> String.fromInt

        roundedLength : Int
        roundedLength =
            String.length roundedString
    in
    if roundedLength < digits then
        String.concat
            [ "."
            , String.repeat (digits - roundedLength) "0"
            , roundedString
            ]
            |> String.toFloat
            |> Maybe.withDefault 0

    else
        String.concat
            [ String.dropRight digits roundedString
            , "."
            , String.dropLeft (roundedLength - digits) roundedString
            ]
            |> String.toFloat
            |> Maybe.withDefault 0


round2 : Float -> Float
round2 =
    roundSignificantly 2



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
    , currentPoint = Point.zero
    , firstConnectedPoint = Point.zero
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

                absoluteControls : Point.Pair
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

                startControl : Point
                startControl =
                    case info.previousAbsoluteCommand of
                        Just (AbsoluteCubicCurve { end } _) ->
                            Point.subtract (Point.scale 2 info.currentPoint) end

                        _ ->
                            info.currentPoint

                absoluteControls : Point.Pair
                absoluteControls =
                    { start = startControl
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
                            Point.subtract
                                (Point.scale 2 info.currentPoint)
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
