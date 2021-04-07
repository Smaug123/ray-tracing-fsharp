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
        Normal : UnitVector
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

        let rayVec = Ray.vector ray
        let denominator = UnitVector.dot plane.Normal rayVec
        if Float.equal denominator 0.0 then [||]
        else
            // TODO I flipped the args in this dot
            let t = (UnitVector.dot' plane.Normal (Point.difference plane.Point (Ray.origin ray))) / denominator
            match Float.compare t 0.0 with
            | Greater ->
                let strikePoint = Ray.walkAlong ray t
                let outgoing, newColour = plane.Reflection ray incomingColour strikePoint
                [| strikePoint, outgoing, newColour |]
            | _ -> [||]

    let reflection
        (style : InfinitePlaneStyle)
        (pointOnPlane : Point)
        (normal : UnitVector)
        : Ray -> Pixel -> Point -> Ray option * Pixel
        =
        fun incomingRay incomingColour strikePoint ->
            match style with
            | InfinitePlaneStyle.LightSource colour ->
                None, Pixel.combine incomingColour colour

            | InfinitePlaneStyle.LambertReflection (albedo, colour, rand) ->
                let outgoing =
                    let (Point pointOnPlane) = pointOnPlane
                    let sphereCentre = Ray.walkAlong (Ray.make strikePoint normal) 1.0
                    let offset = UnitVector.random rand pointOnPlane.Length
                    let target = Ray.walkAlong (Ray.make sphereCentre offset) 1.0
                    Point.difference target strikePoint
                    |> Ray.make' strikePoint

                let newColour = Pixel.combine incomingColour colour
                outgoing, Pixel.darken newColour albedo

            | InfinitePlaneStyle.PureReflection (albedo, colour) ->
                let plane =
                    Plane.makeSpannedBy (Ray.make strikePoint normal) incomingRay
                    |> Plane.orthonormalise
                let outgoing =
                    match plane with
                    | None ->
                        // Incoming ray is directly along the normal
                        Ray.flip incomingRay
                        |> Ray.parallelTo strikePoint
                        |> Some
                    | Some plane ->
                        // Incoming ray is (plane1.ray) plane1 + (plane2.ray) plane2
                        // We want the reflection in the normal, so need (plane1.ray) plane1 - (plane2.ray) plane2
                        let normalComponent = (UnitVector.dot plane.V1 (Ray.vector incomingRay))
                        let tangentComponent = - (UnitVector.dot plane.V2 (Ray.vector incomingRay))
                        Ray.walkAlong (Ray.make (Ray.walkAlong (Ray.make plane.Point plane.V1) normalComponent) plane.V2) tangentComponent
                        |> Point.difference strikePoint
                        |> Ray.make' strikePoint

                let newColour = Pixel.combine incomingColour colour
                let darkened = Pixel.darken newColour albedo
                outgoing, darkened


    let make (style : InfinitePlaneStyle) (pointOnPlane : Point) (normal : UnitVector) : InfinitePlane =
        {
            Point = pointOnPlane
            Normal = normal
            Reflection = reflection style pointOnPlane normal
        }

