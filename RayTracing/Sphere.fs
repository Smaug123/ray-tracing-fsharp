namespace RayTracing

open System

type Sphere =
    {
        Centre : Point
        Radius : float
        /// If an incoming ray has the given colour, and hits the
        /// given point (which is guaranteed to be on the surface),
        /// what colour ray does it output and in what direction?
        Reflection : Ray -> Pixel -> Point -> Ray option * Pixel
    }

type SphereStyle =
    /// An emitter of light.
    | LightSource of Pixel
    /// An absorbing black sphere, with a small light-emitting cap.
    | LightSourceCap of Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : float * colour : Pixel
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : float * colour : Pixel * Random

[<RequireQualifiedAccess>]
module Sphere =

    let normal (centre : Point) (p : Point) : Ray =
        {
            Origin = p
            Vector = Point.difference p centre
        }

    let reflection
        (style : SphereStyle)
        (centre : Point)
        (radius : float)
        : Ray -> Pixel -> Point -> Ray option * Pixel
        =
        let normal = normal centre
        fun incomingRay incomingColour strikePoint ->
            let normal = normal strikePoint

            match style with
            | SphereStyle.LightSource colour ->
                None, Pixel.combine incomingColour colour
            | SphereStyle.LightSourceCap colour ->
                let circleCentreZCoord =
                    match centre with
                    | Point v -> Array.head v
                let zCoordLowerBound = circleCentreZCoord + (radius - (radius / 4.0))
                let strikeZCoord =
                    match strikePoint with
                    | Point v -> Array.head v
                let colour =
                    match Float.compare strikeZCoord zCoordLowerBound with
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
                            let sphereCentre = Ray.walkAlong normal 1.0
                            let offset = Vector.randomUnit rand centre.Length
                            let target = Ray.walkAlong { Origin = sphereCentre ; Vector = offset } 1.0
                            Point.difference target strikePoint
                    }

                let newColour = Pixel.combine incomingColour colour
                Some outgoing, Pixel.darken newColour albedo

            | SphereStyle.PureReflection (albedo, colour) ->
                let plane =
                    Plane.makeSpannedBy normal incomingRay
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
                        let normalComponent = Vector.dot plane.V1 incomingRay.Vector
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

    let make (style : SphereStyle) (centre : Point) (radius : float) : Sphere =
        {
            Centre = centre
            Radius = radius
            Reflection = reflection style centre radius
        }

    let liesOn (point : Point) (sphere : Sphere) : bool =
        Float.equal (Vector.normSquared (Point.difference sphere.Centre point)) (sphere.Radius * sphere.Radius)

    /// Returns the intersections of this ray with this sphere.
    /// The nearest intersection is returned first, if there are multiple.
    /// Does not return any intersections which are behind us.
    /// If the sphere is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    let intersections
        (sphere : Sphere)
        (ray : Ray)
        (incomingColour : Pixel)
        : (Point * Ray option * Pixel) array
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
            Point.difference ray.Origin sphere.Centre

        let vector = ray.Vector |> Vector.unitise |> Option.get
        let a = Vector.normSquared vector

        let b = (Vector.dot vector difference) * 2.0

        let c = (Vector.normSquared difference) - (sphere.Radius * sphere.Radius)

        let discriminant = (b * b) - (4.0 * a * c)

        let ts =
            match Float.compare discriminant 0.0 with
            | Comparison.Equal ->
                [|
                    - (b / (2.0 * a))
                |]
            | Comparison.Less -> [||]
            | Comparison.Greater ->
                let intermediate = sqrt discriminant
                let denom = 2.0 * a
                [|
                    (intermediate - b) / denom
                    - (b + intermediate) / denom
                |]
            // Don't return anything that's behind us
            |> Array.filter (fun i -> Float.compare i 0.0 = Greater)
        ts
        |> function
            | [||] -> [||]
            | [|x|] -> [|x|]
            | [|x ; y|] ->
                match Float.compare x y with
                | Less -> [|x ; y|]
                | Equal -> failwith "Nooo"
                | Greater -> [|y ; x|]
            | _ -> failwith "Impossible"
        |> Array.map (fun pos ->
            let strikePoint = Ray.walkAlong ray pos
            let outgoing, colour = sphere.Reflection ray incomingColour strikePoint
            strikePoint, outgoing, colour
        )
