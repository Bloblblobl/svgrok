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
    , overlayConfig : Canvas.OverlayConfig
    , viewBox : ViewBox
    , mouseOffset : Point
    , state : Canvas.State
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
    , overlayConfig = Canvas.initOverlayConfig
    , viewBox = ViewBox.init
    , mouseOffset = Point.zero
    , state = Canvas.Neutral
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
subscriptions _ =
    [ BrowserE.onMouseMove <|
        JsonD.map (CanvasMsg << Canvas.MouseMove) decodeMouseOffset
    , BrowserE.onMouseDown (JsonD.succeed (CanvasMsg Canvas.MouseDown))
    , BrowserE.onMouseUp (JsonD.succeed (CanvasMsg Canvas.MouseUp))
    , BrowserE.onResize WindowResized
    ]
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
        ghost : Maybe Path
        ghost =
            case model.state of
                Canvas.Dragging { dragStart, temporarySelection } ->
                    case temporarySelection of
                        Just selection ->
                            Just
                                (Path.update
                                    (Canvas.addSelection model.path selection)
                                    (Point.subtract model.mouseOffset dragStart)
                                )

                        Nothing ->
                            Just
                                (Path.update
                                    model.path
                                    (Point.subtract model.mouseOffset dragStart)
                                )

                _ ->
                    Nothing

        canvas : Svg Msg
        canvas =
            Html.map CanvasMsg
                (Canvas.view
                    model.viewBox
                    model.overlayConfig
                    model.path
                    ghost
                )
    in
    Html.div [] [ canvas, viewUI model ]


viewState : Canvas.State -> Point -> Html Msg
viewState state offset =
    Html.p
        [ HtmlA.style "padding-left" "10px" ]
        [ Html.text <| Canvas.stateToString state ++ Point.toString offset ]


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
        , viewState model.state model.mouseOffset
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
