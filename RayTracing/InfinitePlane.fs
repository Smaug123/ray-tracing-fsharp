namespace RayTracing

open System

type InfinitePlaneStyle<'a> =
    /// An emitter of light.
    | LightSource of Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : 'a * colour : Pixel
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : 'a * colour : Pixel * Random

type InfinitePlane<'a> =
    {
        Normal : Vector<'a>
        Point : Point<'a>
        /// If an incoming ray has the given colour, and hits the
        /// given point (which is guaranteed to be on the surface),
        /// what colour ray does it output and in what direction?
        Reflection : Ray<'a> -> Pixel -> Point<'a> -> Ray<'a> option * Pixel
    }

[<RequireQualifiedAccess>]
module InfinitePlane =

    /// Returns the intersections of this ray with this plane.
    /// The nearest intersection is returned first, if there are multiple.
    /// Does not return any intersections which are behind us.
    /// If the plane is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    let intersections<'a>
        (num : Num<'a>)
        (plane : InfinitePlane<'a>)
        (ray : Ray<'a>)
        (incomingColour : Pixel)
        : (Point<'a> * Ray<'a> option * Pixel) array
        =
        // ((ray.Origin - plane.Point) + t ray.Vector) . plane.Normal = 0

        let rayVec = ray.Vector |> Vector.unitise num |> Option.get
        let denominator = Vector.dot num plane.Normal rayVec
        if num.Equal denominator num.Zero then [||]
        else
            let t = num.Divide (Vector.dot num (Point.difference num plane.Point ray.Origin) plane.Normal) denominator
            match num.Compare t num.Zero with
            | Greater ->
                let strikePoint = Ray.walkAlong num { Origin = ray.Origin ; Vector = rayVec } t
                let outgoing, newColour = plane.Reflection ray incomingColour strikePoint
                [| strikePoint, outgoing, newColour |]
            | _ -> [||]

    let reflection<'a>
        (num : Num<'a>)
        (style : InfinitePlaneStyle<'a>)
        (pointOnPlane : Point<'a>)
        (normal : Vector<'a>)
        : Ray<'a> -> Pixel -> Point<'a> -> Ray<'a> option * Pixel
        =
        fun incomingRay incomingColour strikePoint ->
            match style with
            | InfinitePlaneStyle.LightSource colour ->
                None, Pixel.combine incomingColour colour

            | InfinitePlaneStyle.LambertReflection (albedo, colour, rand) ->
                let outgoing =
                    {
                        Origin = strikePoint
                        Vector =
                            let (Point pointOnPlane) = pointOnPlane
                            let sphereCentre = Ray.walkAlong num { Origin = strikePoint ; Vector = normal } num.One
                            let offset = Vector.randomUnit num rand pointOnPlane.Length
                            let target = Ray.walkAlong num { Origin = sphereCentre ; Vector = offset } num.One
                            Point.difference num target strikePoint
                    }

                let newColour = Pixel.combine incomingColour colour
                Some outgoing, Pixel.darken num newColour albedo

            | InfinitePlaneStyle.PureReflection (albedo, colour) ->
                let plane =
                    Plane.makeSpannedBy { Origin = strikePoint ; Vector = normal } incomingRay
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


    let make<'a> (num : Num<'a>) (style : InfinitePlaneStyle<'a>) (pointOnPlane : Point<'a>) (normal : Vector<'a>) : InfinitePlane<'a> =
        {
            Point = pointOnPlane
            Normal = normal
            Reflection = reflection num style pointOnPlane normal
        }

