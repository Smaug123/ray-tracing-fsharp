namespace RayTracing

type Sphere<'a> =
    {
        Centre : Point<'a>
        Radius : 'a
    }

[<RequireQualifiedAccess>]
module Sphere =
    let intersections<'a> (num : Num<'a>) (sphere : Sphere<'a>) (ray : Ray<'a>) : Point<'a> list =
        // The sphere is all points P such that Point.normSquared (P - sphere.Centre) = sphere.Radius^2
        // The ray is all ray.Origin + t ray.Vector for any t.
        // So the intersection is all P such that
        //     Point.normSquared (ray.Origin + t ray.Vector - sphere.Centre) = sphere.Radius^2
        // Simplified,
        //     t^2 Point.normSquared ray.Vector
        //       + 2 t Vector.dot ray.Vector (ray.Origin - sphere.Centre)
        //       + Point.normSquared (ray.Origin - sphere.Centre) - sphere.Radius^2
        //     = 0
        // That is:
        let difference = Point.difference num ray.Origin sphere.Centre
        let a = Point.normSquared num ray.Vector
        let b = num.Double (Vector.dot num ray.Vector difference)
        let c = num.Subtract (Point.normSquared num difference) (num.Times sphere.Radius sphere.Radius)
        let discriminant = num.Subtract (num.Times b b) (num.Double (num.Double (num.Times a c)))
        match num.Compare discriminant num.Zero with
        | Comparison.Equal ->
            [
                // One answer
            ]
        | Comparison.Less -> []
        | Comparison.Greater ->
            [
                // Two answers
            ]