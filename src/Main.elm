module Main exposing (..)

import Browser
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Path
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
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
    }


type alias Model =
    { config : Config
    , parseErrorString : String
    , path : Path.Path
    , pathCommands : Path.Commands
    , pathCommandsString : String
    , mouseOverPath : Bool
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
    }


init : Model
init =
    { config = initConfig
    , parseErrorString = ""
    , path = []
    , pathCommands = []
    , pathCommandsString = ""
    , mouseOverPath = False
    }



-- UPDATE


type ConfigChange
    = FillColor String
    | StrokeColor String
    | StrokeWidth String
    | ViewBox String


type Msg
    = PathStringChanged String
    | ConfigChanged ConfigChange
    | MouseOverPath
    | MouseOutPath


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        PathStringChanged newPathString ->
            let
                ( path, commands, errorString ) =
                    Path.fromString newPathString
            in
            { model
                | path = path
                , pathCommands = commands
                , pathCommandsString = newPathString
                , parseErrorString = errorString
            }

        ConfigChanged configChange ->
            { model | config = updateConfig configChange model.config }

        MouseOverPath ->
            { model | mouseOverPath = True }

        MouseOutPath ->
            { model | mouseOverPath = False }



-- VIEW


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
        , if model.mouseOverPath then
            Html.text "Mouse Over"

          else
            Html.text "Mouse Not Over"
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


viewSvg : Model -> Html.Html Msg
viewSvg { config, pathCommandsString } =
    Svg.svg
        [ SvgA.height "120"
        , SvgA.width "120"
        , SvgA.fill config.fillColor
        , SvgA.stroke config.strokeColor
        , SvgA.strokeWidth config.strokeWidth
        , SvgA.viewBox config.viewBox
        , SvgE.onMouseOver MouseOverPath
        , SvgE.onMouseOut MouseOutPath
        ]
        [ Svg.path [ SvgA.d pathCommandsString ] [] ]


viewOverlay : Model -> Html.Html Msg
viewOverlay { path, config } =
    Svg.svg
        [ SvgA.height "120"
        , SvgA.width "120"
        , SvgA.fill config.overlay.fillColor
        , SvgA.stroke config.overlay.strokeColor
        , SvgA.strokeWidth config.strokeWidth
        , SvgA.viewBox config.viewBox
        ]
        (List.concatMap (viewOverlaySegment config) path)



-- TODO: use marker-start/marker-end attributes


viewMarker : Config -> Path.Point -> Html.Html Msg
viewMarker config point =
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
