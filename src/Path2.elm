module Path2 exposing (..)

import Point exposing (Point)


type Relation
    = Absolute
    | Relative


type ArcSize
    = Large
    | Small


type ArcRotation
    = Clockwise
    | CounterClockwise


type CommandType
    = MoveCommand { to : Point }
    | LineCommand { to : Point }
    | HorizontalLineCommand { toX : Float }
    | VerticalLineCommand { toY : Float }
    | CubicCurveCommand { startControl : Point, endControl : Point, to : Point }
    | SmoothCubicCurveCommand { endControl : Point, to : Point }
    | QuadraticCurveCommand { control : Point, to : Point }
    | SmoothQuadraticCurveCommand { to : Point }
    | ArcCommand
        { radii : Point
        , angle : Float
        , size : ArcSize
        , rotation : ArcRotation
        , to : Point
        }
    | CloseCommand


type alias Command =
    { relation : Relation
    , commandType : CommandType
    }


type Segment
    = MoveSegment { from : Point, to : Point }
    | LineSegment { from : Point, to : Point }
    | CubicCurveSegment
        { startControl : Point
        , endControl : Point
        , from : Point
        , to : Point
        }
    | QuadraticCurveSegment { control : Point, from : Point, to : Point }
    | ArcSegment
        { radii : Point
        , angle : Float
        , size : ArcSize
        , rotation : ArcRotation
        , from : Point
        , to : Point
        }
    | CloseSegment { from : Point, to : Point }


type alias Component =
    { command : Command
    , segment : Segment
    }
