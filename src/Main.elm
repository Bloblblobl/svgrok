module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events as BrowserE
import Canvas
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Path exposing (Path)
import Path.Parser
import Point exposing (Point)
import Svg exposing (Svg)
import Task
import ViewBox exposing (ViewBox)


viewBoxScale : Float
viewBoxScale =
    100


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { pathString : String
    , path : Path
    , mouseOffset : Maybe Point
    , mouseOverOverlay : Bool
    , mouseDown : Bool
    , overlayConfig : Canvas.OverlayConfig
    , viewBox : ViewBox
    }


type Msg
    = CanvasMsg Canvas.Msg
    | SetViewBox ViewBox
    | PathStringChanged String
    | WindowResized Int Int


initPath : Path
initPath =
    { components = []
    , hovered = Nothing
    , selected = []
    }


initModel : Model
initModel =
    { pathString = ""
    , path = Path.init
    , mouseOffset = Nothing
    , mouseOverOverlay = False
    , mouseDown = False
    , overlayConfig = Canvas.initOverlayConfig
    , viewBox = ViewBox.init
    }


getInitialViewBoxFromViewport : Float -> Cmd Msg
getInitialViewBoxFromViewport scaleFactor =
    Task.perform
        (SetViewBox << ViewBox.scale scaleFactor << ViewBox.fromViewport)
        Browser.Dom.getViewport


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, getInitialViewBoxFromViewport viewBoxScale )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CanvasMsg canvasMsg ->
            ( Canvas.update canvasMsg model, Cmd.none )

        SetViewBox newViewBox ->
            ( { model | viewBox = newViewBox }, Cmd.none )

        PathStringChanged newPathString ->
            ( { model
                | pathString = newPathString
                , path = Path.Parser.parse newPathString
              }
            , Cmd.none
            )

        WindowResized newWidth newHeight ->
            let
                newViewBox : ViewBox
                newViewBox =
                    { minX = model.viewBox.minX
                    , minY = model.viewBox.minY
                    , width = toFloat newWidth
                    , height = toFloat newHeight
                    }
            in
            ( { model | viewBox = ViewBox.scale viewBoxScale newViewBox }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    BrowserE.onResize WindowResized


view : Model -> Html Msg
view model =
    let
        canvas : Svg Msg
        canvas =
            Html.map CanvasMsg
                (Canvas.view
                    model.viewBox
                    model.overlayConfig
                    model.path
                )
    in
    Html.div [] [ canvas, viewUI model ]


viewUI : Model -> Html Msg
viewUI model =
    Html.div
        [ HtmlA.style "position" "fixed"
        , HtmlA.style "display" "flex"
        , HtmlA.style "flex-direction" "column"
        , HtmlA.style "width" "100%"
        , HtmlA.style "bottom" "0"
        ]
        [ viewViewBoxSize model.viewBox
        , viewPathStringInput model.pathString
        ]


viewViewBoxSize : ViewBox -> Html Msg
viewViewBoxSize viewBox =
    Html.p
        [ HtmlA.style "margin" "0"
        , HtmlA.style "padding-left" "10px"
        ]
        [ Html.text "Width: "
        , Html.text (String.fromInt (round viewBox.width))
        , Html.text ", Height: "
        , Html.text (String.fromInt (round viewBox.height))
        ]


viewPathStringInput : String -> Html Msg
viewPathStringInput pathString =
    Html.div
        [ HtmlA.style "display" "flex"
        , HtmlA.style "padding" "10px"
        ]
        [ Html.input
            [ HtmlA.value pathString
            , HtmlE.onInput PathStringChanged
            , HtmlA.style "width" "100%"
            , HtmlA.style "font-size" "64px"
            ]
            [ Html.text pathString ]
        ]
