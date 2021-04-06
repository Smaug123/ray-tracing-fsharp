namespace RayTracing

open System

type Sphere<'a> =
    {
        Centre : Point<'a>
        Radius : 'a
        /// If an incoming ray has the given colour, and hits the
        /// given point (which is guaranteed to be on the surface),
        /// what colour ray does it output and in what direction?
        Reflection : Ray<'a> -> Pixel -> Point<'a> -> Ray<'a> option * Pixel
    }

type SphereStyle<'a> =
    /// An emitter of light.
    | LightSource of Pixel
    /// An absorbing black sphere, with a small light-emitting cap.
    | LightSourceCap of Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : 'a * colour : Pixel
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : 'a * colour : Pixel * Random

[<RequireQualifiedAccess>]
module Sphere =

    let normal<'a> (num : Num<'a>) (centre : Point<'a>) (p : Point<'a>) : Ray<'a> =
        {
            Origin = p
            Vector = Point.difference num p centre
        }

    let reflection<'a>
        (num : Num<'a>)
        (style : SphereStyle<'a>)
        (centre : Point<'a>)
        (radius : 'a)
        : Ray<'a> -> Pixel -> Point<'a> -> Ray<'a> option * Pixel
        =
        let normal = normal num centre
        fun incomingRay incomingColour strikePoint ->
            let normal = normal strikePoint

            match style with
            | SphereStyle.LightSource colour ->
                None, Pixel.combine incomingColour colour
            | SphereStyle.LightSourceCap colour ->
                let circleCentreZCoord =
                    match centre with
                    | Point v -> Array.head v
                let zCoordLowerBound = num.Add circleCentreZCoord (num.Subtract radius (num.DivideInteger radius 4))
                let strikeZCoord =
                    match strikePoint with
                    | Point v -> Array.head v
                let colour =
                    match num.Compare strikeZCoord zCoordLowerBound with
                    | Greater ->
                        Pixel.combine colour incomingColour
                    | _ ->
                        Colour.Black
                None, colour

            | SphereStyle.LambertReflection (albedo, colour, rand) ->
                let outgoing =
                    {
                        Origin = strikePoint
                        Vector =
                            let (Point centre) = centre
                            let sphereCentre = Ray.walkAlong num normal num.One
                            let offset = Vector.randomUnit num rand centre.Length
                            let target = Ray.walkAlong num { Origin = sphereCentre ; Vector = offset } num.One
                            Point.difference num target strikePoint
                    }

                let newColour = Pixel.combine incomingColour colour
                Some outgoing, Pixel.darken num newColour albedo

            | SphereStyle.PureReflection (albedo, colour) ->
                let plane =
                    Plane.makeSpannedBy normal incomingRay
                    |> Plane.orthonormalise num
                let outgoing =
                    match plane with
                    | None ->
                        // Incoming ray is directly along the normal
                        {
                            Origin = strikePoint
                            Vector = incomingRay.Vector |> Vector.scale num (num.Negate num.One)
                        }
                    | Some plane ->
                        // Incoming ray is (plane1.ray) plane1 + (plane2.ray) plane2
                        // We want the reflection in the normal, so need (plane1.ray) plane1 - (plane2.ray) plane2
                        let normalComponent = (Vector.dot num plane.V1 incomingRay.Vector)
                        let tangentComponent = num.Negate (Vector.dot num plane.V2 incomingRay.Vector)
                        {
                            Origin = strikePoint
                            Vector =
                                Ray.walkAlong num { Origin = Ray.walkAlong num { Origin = plane.Point ; Vector = plane.V1 } normalComponent ; Vector = plane.V2 } tangentComponent
                                |> Point.difference num strikePoint
                        }

                let newColour = Pixel.combine incomingColour colour
                let darkened = Pixel.darken num newColour albedo
                Some outgoing, darkened

    let make<'a> (num : Num<'a>) (style : SphereStyle<'a>) (centre : Point<'a>) (radius : 'a) : Sphere<'a> =
        {
            Centre = centre
            Radius = radius
            Reflection = reflection num style centre radius
        }

    let liesOn<'a> (num : Num<'a>) (point : Point<'a>) (sphere : Sphere<'a>) : bool =
        num.Equal (Vector.normSquared num (Point.difference num sphere.Centre point)) (num.Times sphere.Radius sphere.Radius)

    /// Returns the intersections of this ray with this sphere.
    /// The nearest intersection is returned first, if there are multiple.
    /// Does not return any intersections which are behind us.
    /// If the sphere is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    let intersections<'a>
        (num : Num<'a>)
        (sphere : Sphere<'a>)
        (ray : Ray<'a>)
        (incomingColour : Pixel)
        : (Point<'a> * Ray<'a> option * Pixel) array
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

        let vector = ray.Vector |> Vector.unitise num |> Option.get
        let a = Vector.normSquared num vector

        let b =
            num.Double (Vector.dot num vector difference)

        let c =
            num.Subtract (Vector.normSquared num difference) (num.Times sphere.Radius sphere.Radius)

        let discriminant =
            num.Subtract (num.Times b b) (num.Double (num.Double (num.Times a c)))

        let ts =
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
        ts
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
