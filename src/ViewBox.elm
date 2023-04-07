module ViewBox exposing (..)

import Browser.Dom
import Point exposing (Point)


type alias ViewBox =
    { minX : Float
    , minY : Float
    , width : Float
    , height : Float
    , actualWidth : Float
    , actualHeight : Float
    }


init : ViewBox
init =
    { minX = 0
    , minY = 0
    , width = 0
    , height = 0
    , actualWidth = 0
    , actualHeight = 0
    }


toString : ViewBox -> String
toString { minX, minY, width, height } =
    String.join " "
        [ String.fromFloat minX
        , String.fromFloat minY
        , String.fromFloat width
        , String.fromFloat height
        ]


fromViewport : Browser.Dom.Viewport -> ViewBox
fromViewport { scene } =
    { minX = 0
    , minY = 0
    , width = scene.width
    , height = scene.height
    , actualWidth = scene.width
    , actualHeight = scene.height
    }


scale : Float -> ViewBox -> ViewBox
scale scaleTarget viewBox =
    let
        scaleFactor : Float
        scaleFactor =
            if viewBox.width < viewBox.height then
                scaleTarget / viewBox.width

            else
                scaleTarget / viewBox.height
    in
    { viewBox
        | width = viewBox.width * scaleFactor
        , height = viewBox.height * scaleFactor
    }


scalePoint : ViewBox -> Point -> Point
scalePoint { width, height, actualWidth, actualHeight } { x, y } =
    { x = toFloat << round <| x * width / actualWidth
    , y = toFloat << round <| y * height / actualHeight
    }
