-- A point is a point in the xy plane, represented by x and y coordinates
-- E.g. (Point 0.0 0.0) is the origin, (Point (-1) (1)) is in the top left
-- quadrant.
data Point = Point Double Double
    deriving (Show, Eq)

-- A line segment is a straight line of finite length, defined by its
-- two end points.   E.g. (LineSegment (Point 0 0) (Point 1 1)) is a
-- line segment from the origin to the coordinate (1, 1)
data LineSegment = LineSegment Point Point 
    deriving (Show, Eq)


-- A Path is a 2D path in the xy-plane.  The idea is that Path can be 
-- extended to support straight lines, curves, and arbitrary paths, 
-- but currently there is only one data constructor for Path: Line.
data Path = 
-- Line represents an infinite straight line defined by its slope a
-- and its y intercept b, ie. by the equation y = ax + b
    Line Double Double
    deriving (Show, Eq)
