module Point exposing (..)


type alias Point =
    { x : Float, y : Float }


type alias Pair =
    { start : Point, end : Point }


scale : Float -> Point -> Point
scale factor { x, y } =
    { x = factor * x
    , y = factor * y
    }


add : Point -> Point -> Point
add point1 point2 =
    { x = point1.x + point2.x
    , y = point1.y + point2.y
    }


subtract : Point -> Point -> Point
subtract point1 point2 =
    { x = point1.x - point2.x
    , y = point1.y - point2.y
    }


zero : Point
zero =
    { x = 0, y = 0 }
