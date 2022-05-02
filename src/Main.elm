module Main exposing (..)

import Browser
import Browser.Events as BrowserE
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Path
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias ColorConfig =
    { fillColor : String
    , strokeColor : String
    }


type alias OverlayConfig =
    { fillColor : String
    , strokeColor : String
    , marker : ColorConfig
    }


type alias Config =
    { fillColor : String
    , strokeColor : String
    , strokeWidth : String
    , overlay : OverlayConfig
    , viewBox : String
    , editorHeight : String
    , editorWidth : String
    }


type alias Model =
    { config : Config
    , parseErrorString : String
    , path : Path.Path
    , pathCommands : Path.Commands
    , pathCommandsString : String
    , mouseOverOverlay : Bool
    , mouseOffset : Path.Point
    }


initOverlayConfig : OverlayConfig
initOverlayConfig =
    { fillColor = "none"
    , strokeColor = "red"
    , marker =
        { fillColor = "red"
        , strokeColor = "none"
        }
    }


initConfig : Config
initConfig =
    { fillColor = "none"
    , strokeColor = "black"
    , strokeWidth = "1"
    , overlay = initOverlayConfig
    , viewBox = "0 0 100 100"
    , editorHeight = "400"
    , editorWidth = "400"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { config = initConfig
      , parseErrorString = ""
      , path = []
      , pathCommands = []
      , pathCommandsString = ""
      , mouseOverOverlay = False
      , mouseOffset = { x = 0, y = 0 }
      }
    , Cmd.none
    )



-- UPDATE


type ConfigChange
    = FillColor String
    | StrokeColor String
    | StrokeWidth String
    | ViewBox String


type Msg
    = PathStringChanged String
    | ConfigChanged ConfigChange
    | MouseOverOverlay
    | MouseOutOverlay
    | MouseMoveOverlay Path.Point


updateConfig : ConfigChange -> Config -> Config
updateConfig configChange config =
    case configChange of
        FillColor newValue ->
            { config | fillColor = newValue }

        StrokeColor newValue ->
            { config | strokeColor = newValue }

        StrokeWidth newValue ->
            { config | strokeWidth = newValue }

        ViewBox newValue ->
            { config | viewBox = newValue }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PathStringChanged newPathString ->
            let
                ( path, commands, errorString ) =
                    Path.fromString newPathString
            in
            ( { model
                | path = path
                , pathCommands = commands
                , pathCommandsString = newPathString
                , parseErrorString = errorString
              }
            , Cmd.none
            )

        ConfigChanged configChange ->
            ( { model | config = updateConfig configChange model.config }
            , Cmd.none
            )

        MouseOverOverlay ->
            ( { model | mouseOverOverlay = True }
            , Cmd.none
            )

        MouseOutOverlay ->
            ( { model | mouseOverOverlay = False }
            , Cmd.none
            )

        MouseMoveOverlay offset ->
            ( { model | mouseOffset = offset }
            , Cmd.none
            )



-- SUBSCRIPTIONS


decodeMouseOffset : JsonD.Decoder Path.Point
decodeMouseOffset =
    JsonD.map2 Path.Point
        (JsonD.field "offsetX" JsonD.float)
        (JsonD.field "offsetY" JsonD.float)


subscriptions : Model -> Sub Msg
subscriptions { mouseOverOverlay } =
    if mouseOverOverlay then
        BrowserE.onMouseMove (JsonD.map MouseMoveOverlay decodeMouseOffset)

    else
        Sub.none



-- VIEW


preserveAspectRatio : Svg.Attribute msg
preserveAspectRatio =
    -- See preserveAspectRatio on MDN
    SvgA.preserveAspectRatio "xMinYMin slice"


elementToViewBoxPoint : Config -> Path.Point -> Path.Point
elementToViewBoxPoint config point =
    -- See https://www.w3.org/TR/SVG2/coords.html#ComputingAViewportsTransform
    let
        editorHeight : Float
        editorHeight =
            Maybe.withDefault 0 (String.toFloat config.editorHeight)

        editorWidth : Float
        editorWidth =
            Maybe.withDefault 0 (String.toFloat config.editorWidth)

        viewBox : Path.Rect
        viewBox =
            Path.viewBoxRectFromString config.viewBox

        scale : Float
        scale =
            max (viewBox.height / editorHeight) (viewBox.width / editorWidth)
    in
    { x = (point.x * scale) + viewBox.x, y = (point.y * scale) + viewBox.y }


viewConfigPath : Config -> Html.Html Msg
viewConfigPath { fillColor, strokeColor, strokeWidth, viewBox } =
    Html.div []
        [ Html.input
            [ HtmlA.value fillColor
            , HtmlE.onInput (FillColor >> ConfigChanged)
            ]
            []
        , Html.input
            [ HtmlA.value strokeColor
            , HtmlE.onInput (StrokeColor >> ConfigChanged)
            ]
            []
        , Html.input
            [ HtmlA.value strokeWidth
            , HtmlE.onInput (StrokeWidth >> ConfigChanged)
            ]
            []
        , Html.input
            [ HtmlA.value viewBox
            , HtmlE.onInput (ViewBox >> ConfigChanged)
            ]
            []
        ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.input
            [ HtmlA.value model.pathCommandsString
            , HtmlE.onInput PathStringChanged
            ]
            []
        , viewConfigPath model.config
        , viewSvg model
        , viewOverlay model
        , viewMouseOffset model
        , Html.p [] [ Html.text model.parseErrorString ]
        , Html.ul []
            (List.map
                (\command ->
                    Html.li []
                        [ Html.text (Path.commandToString command) ]
                )
                model.pathCommands
            )
        , Html.hr [] []
        , Html.ul []
            (List.map
                (\segment ->
                    Html.li []
                        [ Html.text (Path.segmentToString segment) ]
                )
                model.path
            )
        ]


viewMouseOffset : Model -> Html.Html Msg
viewMouseOffset { config, mouseOffset } =
    let
        transformedOffset : Path.Point
        transformedOffset =
            elementToViewBoxPoint config mouseOffset
    in
    Html.text <|
        String.join ""
            [ "Mouse Offset: "
            , String.fromFloat transformedOffset.x
            , ","
            , String.fromFloat transformedOffset.y
            ]


viewSvg : Model -> Html.Html Msg
viewSvg { config, pathCommandsString } =
    Svg.svg
        [ SvgA.height config.editorHeight
        , SvgA.width config.editorWidth
        , SvgA.fill config.fillColor
        , SvgA.stroke config.strokeColor
        , SvgA.strokeWidth config.strokeWidth
        , SvgA.viewBox config.viewBox
        , preserveAspectRatio
        ]
        [ Svg.path [ SvgA.d pathCommandsString ] [] ]


viewOverlay : Model -> Html.Html Msg
viewOverlay { path, config } =
    Svg.svg
        [ SvgA.height config.editorHeight
        , SvgA.width config.editorWidth
        , SvgA.fill config.overlay.fillColor
        , SvgA.stroke config.overlay.strokeColor
        , SvgA.strokeWidth config.strokeWidth
        , SvgA.viewBox config.viewBox
        , SvgE.onMouseOver MouseOverOverlay
        , SvgE.onMouseOut MouseOutOverlay
        , preserveAspectRatio
        ]
        (List.concatMap (viewOverlaySegment config) path)


viewMarker : Config -> Path.Point -> Html.Html Msg
viewMarker config point =
    -- TODO: use marker-start/marker-end attributes on the overlay segments
    Svg.circle
        [ SvgA.fill config.overlay.marker.fillColor
        , SvgA.stroke config.overlay.marker.strokeColor
        , SvgA.cx (String.fromFloat point.x)
        , SvgA.cy (String.fromFloat point.y)
        , SvgA.r config.strokeWidth
        ]
        []


viewOverlaySegment : Config -> Path.Segment -> List (Html.Html Msg)
viewOverlaySegment config (Path.Segment points segmentType) =
    case segmentType of
        Path.Line ->
            [ viewMarker config points.start
            , Svg.line
                [ SvgA.x1 (String.fromFloat points.start.x)
                , SvgA.y1 (String.fromFloat points.start.y)
                , SvgA.x2 (String.fromFloat points.end.x)
                , SvgA.y2 (String.fromFloat points.end.y)
                ]
                []
            , viewMarker config points.end
            ]

        _ ->
            [ viewMarker config points.start
            , Svg.path
                [ SvgA.d <|
                    Path.segmentToPathString (Path.Segment points segmentType)
                ]
                []
            , viewMarker config points.end
            ]
