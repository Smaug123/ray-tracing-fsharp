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
        Ray.make' p (Point.difference p centre)
        |> Option.get

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
                    let (Point centre) = centre
                    let sphereCentre = Ray.walkAlong normal 1.0
                    let offset = UnitVector.random rand centre.Length
                    let target = Ray.walkAlong (Ray.make sphereCentre offset) 1.0
                    Point.difference target strikePoint
                    |> Ray.make' strikePoint

                let newColour = Pixel.combine incomingColour colour
                outgoing, Pixel.darken newColour albedo

            | SphereStyle.PureReflection (albedo, colour) ->
                let plane =
                    Plane.makeSpannedBy normal incomingRay
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
                        let normalComponent = UnitVector.dot plane.V1 (Ray.vector incomingRay)
                        let tangentComponent = - (UnitVector.dot plane.V2 (Ray.vector incomingRay))
                        Ray.walkAlong (Ray.make (Ray.walkAlong (Ray.make plane.Point plane.V1) normalComponent) plane.V2) tangentComponent
                        |> Point.difference strikePoint
                        |> Ray.make' strikePoint

                let newColour = Pixel.combine incomingColour colour
                let darkened = Pixel.darken newColour albedo
                outgoing, darkened

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
        let difference =
            Point.difference (Ray.origin ray) sphere.Centre

        let b = (UnitVector.dot' (Ray.vector ray) difference) * 2.0

        let c = (Vector.normSquared difference) - (sphere.Radius * sphere.Radius)

        let discriminant = (b * b) - (4.0 * c)

        let ts =
            match Float.compare discriminant 0.0 with
            | Comparison.Equal ->
                [|
                    - (b / 2.0)
                |]
            | Comparison.Less -> [||]
            | Comparison.Greater ->
                let intermediate = sqrt discriminant
                [|
                    (intermediate - b) / 2.0 ;
                    - (b + intermediate) / 2.0
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
