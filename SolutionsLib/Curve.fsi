namespace SolutionsLib

module Curve = 
    [<Sealed>]
    type Curve = 
        /// Chain curves
        static member ( + ) : Curve * Curve -> Curve
        /// Scale the curve
        static member ( * ) : float * Curve -> Curve
        /// Rotate the curve
        static member ( |^) : Curve * float -> Curve
        /// Rotate the curve by integer angle
        static member ( |^) : Curve * int -> Curve
        /// Move the curve by given point
        static member (-->) : Curve * (float * float) -> Curve
        /// Reflection relative to line x = a
        static member ( ><) : Curve * float -> Curve
    /// Single point curve
    val point : float * float -> Curve
    /// Reflect the curve relative to y = b
    val verticalReflection : Curve -> float -> Curve
    /// Bounding rectangle for the curve
    val boundingBox : Curve -> (float * float) * (float * float)
    /// The distance between the minimal and maximal x values of the points in the curve
    val width : Curve -> float
    /// The distance between the minimal and maximal y values of the points in the curve
    val height : Curve -> float
    /// The starting point of the curve
    val start : Curve -> (float * float)
    /// Turns the curve into list of points
    val toList : Curve -> (float * float) list