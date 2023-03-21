module ViewBox exposing (..)

import Browser.Dom


type alias ViewBox =
    { minX : Float
    , minY : Float
    , width : Float
    , height : Float
    }


init : ViewBox
init =
    { minX = 0
    , minY = 0
    , width = 100
    , height = 100
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
