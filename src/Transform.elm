port module Transform exposing (..)

{-| This is a standalone utility that takes a command string and outputs a
cleaned up version of it by trimming it up to the first invalid command it
encounters.
-}

import Path
import Path.Parser
import Platform exposing (Program)


port output : String -> Cmd msg


type alias Config =
    { commandString : String
    , scaleX : Float
    , scaleY : Float
    }


transformCommandString : Config -> String
transformCommandString { commandString, scaleX, scaleY } =
    Path.Parser.parseAndTrim commandString
        |> Path.scale scaleX scaleY
        |> Path.toString


init : Config -> ( (), Cmd msg )
init config =
    ( (), output (transformCommandString config) )


update : msg -> () -> ( (), Cmd msg )
update _ _ =
    ( (), Cmd.none )


subscriptions : () -> Sub msg
subscriptions _ =
    Sub.none


main : Program Config () msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
