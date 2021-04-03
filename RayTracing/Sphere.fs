namespace RayTracing

type Sphere<'a> =
    {
        Centre : Point<'a>
        Radius : 'a
        /// If an incoming ray has the given colour, and hits the
        /// given point (which is guaranteed to be on the surface),
        /// what colour ray does it output and in what direction?
        Reflection : Ray<'a> -> Pixel -> Point<'a> -> Ray<'a> * Pixel
    }

[<RequireQualifiedAccess>]
module Sphere =

    let makePureWhite<'a> (centre : Point<'a>) (radius : 'a) : Sphere<'a> =
        {
            Centre = centre
            Radius = radius
            Reflection =
                fun incomingRay incomingColour strikePoint ->
                    failwith ""
        }

    let liesOn<'a> (num : Num<'a>) (point : Point<'a>) (sphere : Sphere<'a>) : bool =
        num.Equal (Point.normSquared num (Point.difference num sphere.Centre point)) (num.Times sphere.Radius sphere.Radius)

    /// Returns the intersections of this ray with this sphere.
    /// The nearest intersection is returned first, if there are multiple.
    /// Does not return any intersections which are behind us.
    let intersections<'a>
        (num : Num<'a>)
        (sphere : Sphere<'a>)
        (ray : Ray<'a>)
        (incomingColour : Pixel)
        : (Point<'a> * Ray<'a> * Pixel) array
        =
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
        let difference =
            Point.difference num ray.Origin sphere.Centre

        let a = Point.normSquared num ray.Vector

        let b =
            num.Double (Vector.dot num ray.Vector difference)

        let c =
            num.Subtract (Point.normSquared num difference) (num.Times sphere.Radius sphere.Radius)

        let discriminant =
            num.Subtract (num.Times b b) (num.Double (num.Double (num.Times a c)))

        match num.Compare discriminant num.Zero with
        | Comparison.Equal ->
            [|
                num.Negate (num.Divide b (num.Double a))
            |]
        | Comparison.Less -> [||]
        | Comparison.Greater ->
            let intermediate = num.Sqrt discriminant
            let denom = num.Double a
            [|
                num.Divide (num.Add (num.Negate b) intermediate) denom
                num.Divide (num.Add (num.Negate b) (num.Negate intermediate)) denom
            |]
        // Don't return anything that's behind us
        |> Array.filter (fun i -> num.Compare i num.Zero = Greater)
        |> function
            | [||] -> [||]
            | [|x|] -> [|x|]
            | [|x ; y|] ->
                match num.Compare x y with
                | Less -> [|x ; y|]
                | Equal -> failwith "Nooo"
                | Greater -> [|y ; x|]
            | _ -> failwith "Impossible"
        |> Array.map (fun pos ->
            let strikePoint = Ray.walkAlong num ray pos
            let outgoing, colour = sphere.Reflection ray incomingColour strikePoint
            strikePoint, outgoing, colour
        )
