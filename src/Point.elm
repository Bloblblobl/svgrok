module Point exposing (..)

{-| A Point is a pair of Floats, represented as an x and y value.
Usually this represents a position in 2D space, but it can also be used to
represent a vector.
-}


type alias Point =
    { x : Float, y : Float }


{-| TODO: remove when removing Old.
-}
type alias Pair =
    { start : Point, end : Point }


{-| Point zero, AKA the origin.
-}
zero : Point
zero =
    { x = 0, y = 0 }


{-| Helper function to create a point concisely.
-}
p : Float -> Float -> Point
p x y =
    { x = x, y = y }


{-| Scales a point by a factor.
The x and y values of the result are the given factor times the original x and y
values.
-}
scale : Float -> Point -> Point
scale factor { x, y } =
    { x = factor * x
    , y = factor * y
    }


{-| Returns the sum of the two given points.
The x and y values of the result are the sum of the x and y values of the two
given points.
-}
add : Point -> Point -> Point
add point1 point2 =
    { x = point1.x + point2.x
    , y = point1.y + point2.y
    }


{-| Returns the difference between the two given points.
The x and y values of the result are the difference between the x and y values
of the two given points.
-}
subtract : Point -> Point -> Point
subtract point1 point2 =
    { x = point1.x - point2.x
    , y = point1.y - point2.y
    }


{-| Converts the given Point to a String, with the x and y values separated by a
comma.
-}
toString : Point -> String
toString point =
    String.fromFloat point.x ++ "," ++ String.fromFloat point.y


{-| TODO: remove when removing Old.
-}
pairToString : Pair -> String
pairToString { start, end } =
    String.concat
        [ "{ start: ("
        , toString start
        , "), end: ("
        , toString end
        , ") }"
        ]


{-| Finds the midpoint between the two given Points.
-}
midpoint : Point -> Point -> Point
midpoint point1 point2 =
    scale 0.5 (add point1 point2)


{-| Determines whether point1 and point2 are reflections of each other over the
given reflection Point.
-}
isReflectionOver : Point -> Point -> Point -> Bool
isReflectionOver reflection point1 point2 =
    midpoint point1 point2 == reflection


{-| Reflects a Point over a given reflection Point.
-}
reflectOver : Point -> Point -> Point
reflectOver reflection point =
    add reflection (subtract reflection point)
