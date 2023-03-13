module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Canvas
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Path exposing (Path)
import Path.Parser
import Point exposing (Point)
import Svg exposing (Svg)
import Task


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
    , viewBox : Canvas.ViewBox
    }


type Msg
    = CanvasMsg Canvas.Msg
    | SetViewBox Canvas.ViewBox
    | PathStringChanged String


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
    , viewBox = Canvas.initViewBox
    }


scaleViewBoxTo : Float -> Canvas.ViewBox -> Canvas.ViewBox
scaleViewBoxTo scaleTo viewBox =
    let
        scaleFactor : Float
        scaleFactor =
            if viewBox.width > viewBox.height then
                scaleTo / viewBox.width

            else
                scaleTo / viewBox.height
    in
    { viewBox
        | width = viewBox.width * scaleFactor
        , height = viewBox.height * scaleFactor
    }


viewBoxFromViewport : Viewport -> Canvas.ViewBox
viewBoxFromViewport { scene } =
    { minX = 0
    , minY = 0
    , width = scene.width
    , height = scene.height
    }
        |> scaleViewBoxTo 100


getInitialViewport : Cmd Msg
getInitialViewport =
    Task.perform (SetViewBox << viewBoxFromViewport) Browser.Dom.getViewport


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, getInitialViewport )


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view { path, pathString, overlayConfig, viewBox } =
    let
        canvas : Svg Msg
        canvas =
            Html.map CanvasMsg (Canvas.view viewBox overlayConfig path)
    in
    Html.div [] [ canvas, viewUI pathString ]


viewUI : String -> Html Msg
viewUI pathString =
    Html.div
        [ HtmlA.style "position" "fixed"
        , HtmlA.style "display" "flex"
        , HtmlA.style "width" "100%"
        , HtmlA.style "bottom" "0"
        ]
        [ Html.input
            [ HtmlA.value pathString
            , HtmlE.onInput PathStringChanged
            , HtmlA.style "width" "100%"
            , HtmlA.style "margin" "10px"
            , HtmlA.style "font-size" "64px"
            ]
            [ Html.text pathString ]
        ]
