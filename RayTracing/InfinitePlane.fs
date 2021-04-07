namespace RayTracing

open System

type InfinitePlaneStyle =
    /// An emitter of light.
    | LightSource of Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : float * colour : Pixel
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : float * colour : Pixel * Random

type InfinitePlane =
    {
        Normal : Vector
        Point : Point
        /// If an incoming ray has the given colour, and hits the
        /// given point (which is guaranteed to be on the surface),
        /// what colour ray does it output and in what direction?
        Reflection : Ray -> Pixel -> Point -> Ray option * Pixel
    }

[<RequireQualifiedAccess>]
module InfinitePlane =

    /// Returns the intersections of this ray with this plane.
    /// The nearest intersection is returned first, if there are multiple.
    /// Does not return any intersections which are behind us.
    /// If the plane is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    let intersections
        (plane : InfinitePlane)
        (ray : Ray)
        (incomingColour : Pixel)
        : (Point * Ray option * Pixel) array
        =
        // ((ray.Origin - plane.Point) + t ray.Vector) . plane.Normal = 0

        let rayVec = ray.Vector |> Vector.unitise |> Option.get
        let denominator = Vector.dot plane.Normal rayVec
        if Float.equal denominator 0.0 then [||]
        else
            let t = (Vector.dot (Point.difference plane.Point ray.Origin) plane.Normal) / denominator
            match Float.compare t 0.0 with
            | Greater ->
                let strikePoint = Ray.walkAlong { Origin = ray.Origin ; Vector = rayVec } t
                let outgoing, newColour = plane.Reflection ray incomingColour strikePoint
                [| strikePoint, outgoing, newColour |]
            | _ -> [||]

    let reflection
        (style : InfinitePlaneStyle)
        (pointOnPlane : Point)
        (normal : Vector)
        : Ray -> Pixel -> Point -> Ray option * Pixel
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
                            let sphereCentre = Ray.walkAlong { Origin = strikePoint ; Vector = normal } 1.0
                            let offset = Vector.randomUnit rand pointOnPlane.Length
                            let target = Ray.walkAlong { Origin = sphereCentre ; Vector = offset } 1.0
                            Point.difference target strikePoint
                    }

                let newColour = Pixel.combine incomingColour colour
                Some outgoing, Pixel.darken newColour albedo

            | InfinitePlaneStyle.PureReflection (albedo, colour) ->
                let plane =
                    Plane.makeSpannedBy { Origin = strikePoint ; Vector = normal } incomingRay
                    |> Plane.orthonormalise
                let outgoing =
                    match plane with
                    | None ->
                        // Incoming ray is directly along the normal
                        {
                            Origin = strikePoint
                            Vector = incomingRay.Vector |> Vector.scale -1.0
                        }
                    | Some plane ->
                        // Incoming ray is (plane1.ray) plane1 + (plane2.ray) plane2
                        // We want the reflection in the normal, so need (plane1.ray) plane1 - (plane2.ray) plane2
                        let normalComponent = (Vector.dot plane.V1 incomingRay.Vector)
                        let tangentComponent = - (Vector.dot plane.V2 incomingRay.Vector)
                        {
                            Origin = strikePoint
                            Vector =
                                Ray.walkAlong { Origin = Ray.walkAlong { Origin = plane.Point ; Vector = plane.V1 } normalComponent ; Vector = plane.V2 } tangentComponent
                                |> Point.difference strikePoint
                        }

                let newColour = Pixel.combine incomingColour colour
                let darkened = Pixel.darken newColour albedo
                Some outgoing, darkened


    let make (style : InfinitePlaneStyle) (pointOnPlane : Point) (normal : Vector) : InfinitePlane =
        {
            Point = pointOnPlane
            Normal = normal
            Reflection = reflection style pointOnPlane normal
        }

