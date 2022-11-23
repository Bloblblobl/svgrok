module Main exposing (..)

import Browser
import Html exposing (Html)
import Path exposing (Path)
import Path.View
import Point exposing (Point)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { path : Path
    , pathString : String
    , overlayConfig : Path.View.OverlayConfig
    , mouseOffset : Point
    , mouseOverOverlay : Bool
    }


type Msg
    = PathChanged Path.Msg


initPath : Path
initPath =
    { components = []
    , hovered = Nothing
    , selected = []
    }


initModel : Model
initModel =
    { path = initPath
    , pathString = ""
    , overlayConfig =
        { default = []
        , hovered = []
        , selected = []
        }
    , mouseOffset = Point.zero
    , mouseOverOverlay = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PathChanged pathMsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view _ =
    Html.div [] []
