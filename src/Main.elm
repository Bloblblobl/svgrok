module Main exposing (..)

import Browser
import Canvas
import Html exposing (Html)
import Path exposing (Path)


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
    , canvasState : Canvas.State
    }


type alias Msg =
    Canvas.Msg


initPath : Path
initPath =
    { components = []
    , hovered = Nothing
    , selected = []
    }


initModel : Model
initModel =
    { pathString = ""
    , canvasState = Canvas.initState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model | canvasState = Canvas.update msg model.canvasState }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view _ =
    Html.div [] []
