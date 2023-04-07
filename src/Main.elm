module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events as BrowserE
import Canvas
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
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
    , mouseDown : Maybe Point
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
    , mouseDown = Nothing
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
                    , actualWidth = toFloat newWidth
                    , actualHeight = toFloat newHeight
                    }
            in
            ( { model | viewBox = ViewBox.scale viewBoxScale newViewBox }
            , Cmd.none
            )


decodeMouseOffset : JsonD.Decoder Point
decodeMouseOffset =
    JsonD.map2 Point
        (JsonD.field "offsetX" JsonD.float)
        (JsonD.field "offsetY" JsonD.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    (if model.mouseOverOverlay then
        [ BrowserE.onMouseMove <|
            JsonD.map (CanvasMsg << Canvas.MouseMove) decodeMouseOffset
        , BrowserE.onMouseDown <|
            JsonD.map (CanvasMsg << Canvas.MouseDown) decodeMouseOffset
        , BrowserE.onMouseUp (JsonD.succeed (CanvasMsg Canvas.MouseUp))
        , BrowserE.onResize WindowResized
        ]

     else if model.mouseDown /= Nothing then
        [ BrowserE.onMouseUp (JsonD.succeed (CanvasMsg Canvas.MouseUp))
        , BrowserE.onResize WindowResized
        ]

     else
        [ BrowserE.onResize WindowResized
        ]
    )
        |> Sub.batch


stringFromBool : Bool -> String
stringFromBool value =
    if value then
        "True"

    else
        "False"


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
        [ Html.p [ HtmlA.style "padding-left" "10px" ]
            [ Html.text
                (Point.toString
                    (Maybe.withDefault Point.zero model.mouseOffset)
                )
            ]
        , Html.p [ HtmlA.style "padding-left" "10px" ]
            [ Html.text <| "over: " ++ stringFromBool model.mouseOverOverlay ]
        , Html.p [ HtmlA.style "padding-left" "10px" ]
            [ Html.text <|
                "down: "
                    ++ Point.toString
                        (Maybe.withDefault Point.zero model.mouseDown)
            ]
        , viewViewBoxSize model.viewBox
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
