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


type alias Config =
    { fillColor : String
    , strokeColor : String
    , strokeWidth : String
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


initConfig : Config
initConfig =
    { fillColor = "none"
    , strokeColor = "black"
    , strokeWidth = "1"
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


viewConfig : Config -> Html.Html Msg
viewConfig config =
    Html.div []
        [ Html.input
            [ HtmlA.value config.fillColor
            , HtmlE.onInput (\s -> ConfigChanged (FillColor s))
            ]
            []
        , Html.input
            [ HtmlA.value config.strokeColor
            , HtmlE.onInput (\s -> ConfigChanged (StrokeColor s))
            ]
            []
        , Html.input
            [ HtmlA.value config.strokeWidth
            , HtmlE.onInput (\s -> ConfigChanged (StrokeWidth s))
            ]
            []
        , Html.input
            [ HtmlA.value config.viewBox
            , HtmlE.onInput (\s -> ConfigChanged (ViewBox s))
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
        , viewConfig model.config
        , Svg.svg
            [ SvgA.height "120"
            , SvgA.width "120"
            , SvgA.fill model.config.fillColor
            , SvgA.stroke model.config.strokeColor
            , SvgA.strokeWidth model.config.strokeWidth
            , SvgA.viewBox model.config.viewBox
            , SvgE.onMouseOver MouseOverPath
            , SvgE.onMouseOut MouseOutPath
            ]
            [ Svg.path [ SvgA.d model.pathCommandsString ] [] ]
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
