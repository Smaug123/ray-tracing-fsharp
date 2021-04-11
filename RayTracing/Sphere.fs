namespace RayTracing

[<Measure>]
type fuzz

/// Index of refraction. Must be greater than or equal to 1.
[<Measure>]
type ior

/// A probability, between 0 and 1.
[<Measure>]
type prob

type Sphere =
    private
        {
            Centre : Point
            Radius : float
            /// If an incoming ray has the given colour, and hits the
            /// given point (which is guaranteed to be on the surface),
            /// what colour ray does it output and in what direction?
            Reflection : Ray -> Pixel -> Point -> Ray option * Pixel
            RadiusSquared : float
        }

type SphereStyle =
    /// An emitter of light.
    | LightSource of Pixel
    /// An absorbing black sphere, with a small light-emitting cap.
    | LightSourceCap of Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : float<albedo> * colour : Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    /// Fuzz must be between 0 (no fuzziness) and 1 (lots of fuzziness)
    | FuzzedReflection of  albedo : float<albedo> * colour : Pixel * fuzz : float<fuzz> * FloatProducer
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : float<albedo> * colour : Pixel * FloatProducer
    /// A refracting sphere with the given ratio `ior` of its index of refraction with that of the surrounding
    /// medium.
    /// The probability is the probability that a ray will refract, so 0 yields a perfectly reflecting sphere.
    | Dielectric of albedo : float<albedo> * colour : Pixel * float<ior> * reflection : float<prob> * FloatProducer

type Orientation =
    | Inside
    | Outside

[<RequireQualifiedAccess>]
module Sphere =

    /// A ray hits the sphere with centre `centre` at point `p`.
    /// This function gives the outward-pointing normal.
    let normal (centre : Point) (p : Point) : Ray =
        Ray.make' p (Point.differenceToThenFrom p centre)
        |> Option.get

    let private liesOn' (centre : Point) (radius : float) : Point -> bool =
        let rSquared = radius * radius
        fun p ->
            Float.equal (Vector.normSquared (Point.differenceToThenFrom p centre)) rSquared

    let reflection
        (style : SphereStyle)
        (centre : Point)
        (radius : float)
        : Ray -> Pixel -> Point -> Ray option * Pixel
        =
        let normal = normal centre
        fun incomingRay incomingColour strikePoint ->
            let normal = normal strikePoint
            // If the incoming ray is on the sphere, then we have to be an internal ray.
            let inside, normal =
                match Float.compare (Vector.normSquared (Point.differenceToThenFrom centre (Ray.origin incomingRay))) (radius * radius) with
                | Equal
                | Less ->
                    // Point is inside or on the sphere so we are coming from within
                    true, Ray.make (Ray.origin normal) (UnitVector.scale -1.0 (Ray.vector normal) |> UnitVector)
                | Greater ->
                    false, normal

            let fuzzedReflection (colour : Pixel) (albedo : float<albedo>) (fuzz : (float<fuzz> * FloatProducer) option) =
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
                        let normalComponent = - UnitVector.dot plane.V1 (Ray.vector incomingRay)
                        let tangentComponent = (UnitVector.dot plane.V2 (Ray.vector incomingRay))
                        let dest = Ray.walkAlong (Ray.make (Ray.walkAlong (Ray.make plane.Point plane.V1) normalComponent) plane.V2) tangentComponent
                        Point.differenceToThenFrom dest strikePoint
                        |> Ray.make' strikePoint

                let outgoing =
                    match outgoing, fuzz with
                    | None, _ -> None
                    | Some outgoing, None -> Some outgoing
                    | Some outgoing, Some (fuzz, rand) ->
                        let offset = UnitVector.random rand (Point.dimension centre)
                        let sphereCentre = Ray.walkAlong outgoing 1.0
                        let target = Ray.walkAlong (Ray.make sphereCentre offset) (fuzz / 1.0<fuzz>)
                        Point.differenceToThenFrom target strikePoint
                        |> Ray.make' strikePoint

                let darkened =
                    Pixel.combine incomingColour colour
                    |> Pixel.darken albedo
                outgoing, darkened

            match style with
            | SphereStyle.LightSource colour ->
                None, Pixel.combine incomingColour colour
            | SphereStyle.LightSourceCap colour ->
                let circleCentreZCoord = Point.xCoordinate centre
                let zCoordLowerBound = circleCentreZCoord + (radius - (radius / 4.0))
                let strikeZCoord = Point.xCoordinate strikePoint
                let colour =
                    match Float.compare strikeZCoord zCoordLowerBound with
                    | Greater ->
                        Pixel.combine colour incomingColour
                    | _ ->
                        Colour.Black
                None, colour

            | SphereStyle.LambertReflection (albedo, colour, rand) ->
                let outgoing =
                    let sphereCentre = Ray.walkAlong normal 1.0
                    let offset = UnitVector.random rand (Point.dimension sphereCentre)
                    let target = Ray.walkAlong (Ray.make sphereCentre offset) 1.0
                    Point.differenceToThenFrom target strikePoint
                    |> Ray.make' strikePoint

                let newColour =
                    Pixel.combine incomingColour colour
                    |> Pixel.darken albedo
                outgoing, newColour

            | SphereStyle.PureReflection (albedo, colour) ->
                fuzzedReflection colour albedo None
            | SphereStyle.FuzzedReflection (albedo, colour, fuzz, random) ->
                fuzzedReflection colour albedo (Some (fuzz, random))

            | SphereStyle.Dielectric (albedo, colour, index, reflectionProb, random) ->
                let newColour =
                    Pixel.combine incomingColour colour
                    |> Pixel.darken albedo

                let rand = random.Get ()
                if LanguagePrimitives.FloatWithMeasure rand > reflectionProb then
                    // reflect!
                    fuzzedReflection colour albedo None
                else
                let index = if inside then 1.0<ior>/index else index / 1.0<ior>
                let plane = Plane.makeSpannedBy normal incomingRay
                let incomingCos = UnitVector.dot (Ray.vector incomingRay) (Ray.vector normal)
                let incomingSin = sqrt (1.0 - incomingCos * incomingCos)
                let outgoingSin = index * incomingSin
                if Float.compare outgoingSin 1.0 = Greater then
                    // override our decision to refract - from this angle, there's no way we could have refracted
                    fuzzedReflection colour albedo None

                else
                let outgoingCos = sqrt (1.0 - outgoingSin * outgoingSin)
                let outgoingPoint =
                    Ray.walkAlong (Ray.make (Ray.walkAlong normal (-outgoingCos)) plane.V2) outgoingSin
                let outgoing = Point.differenceToThenFrom outgoingPoint strikePoint |> Ray.make' strikePoint |> Option.get

                Some outgoing, newColour


    let make (style : SphereStyle) (centre : Point) (radius : float) : Sphere =
        {
            Centre = centre
            Radius = radius
            Reflection = reflection style centre radius
            RadiusSquared = radius * radius
        }

    let liesOn (point : Point) (sphere : Sphere) : bool =
        liesOn' sphere.Centre sphere.Radius point

    /// Returns the distance along this ray at which the nearest intersection of the ray lies with this sphere.
    /// Does not return any intersections which are behind us.
    /// If the sphere is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    let firstIntersection
        (sphere : Sphere)
        (ray : Ray)
        : float voption
        =
        let difference = Point.differenceToThenFrom (Ray.origin ray) sphere.Centre

        let b = (UnitVector.dot' (Ray.vector ray) difference)

        let c = (Vector.normSquared difference) - sphere.RadiusSquared

        let discriminantOverFour = (b * b - c)

        let intersectionPoint =
            match Float.compare discriminantOverFour 0.0 with
            | Comparison.Equal ->
                Some (-b)
            | Comparison.Less -> None
            | Comparison.Greater ->
                let intermediate = sqrt discriminantOverFour
                let i1 = intermediate - b
                let i2 = - (b + intermediate)
                let i1Pos = Float.positive i1
                let i2Pos = Float.positive i2
                if i1Pos && i2Pos then
                    match Float.compare i1 i2 with
                    | Less -> i1
                    | Greater -> i2
                    | Equal -> i1
                    |> Some
                elif i1Pos then Some i1
                elif i2Pos then Some i2
                else None
        match intersectionPoint with
        | None -> ValueNone
        | Some i ->
            // Don't return anything that's behind us
            if Float.positive i then
                ValueSome i
            else ValueNone
