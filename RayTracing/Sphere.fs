namespace RayTracing

[<Measure>]
type fuzz

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
            Reflection : LightRay -> Point -> LightDestination
            RadiusSquared : float
            BoundingBox : BoundingBox
        }

type SphereStyle =
    /// An emitter of light.
    | LightSource of Texture
    /// An absorbing black sphere, with a small light-emitting cap.
    | LightSourceCap of Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : float<albedo> * texture : Texture
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    /// Fuzz must be between 0 (no fuzziness) and 1 (lots of fuzziness)
    | FuzzedReflection of albedo : float<albedo> * texture : Texture * fuzz : float<fuzz> * FloatProducer
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : float<albedo> * texture : Texture * FloatProducer
    /// A refracting sphere with the given ratio `ior` of its index of refraction with that of the medium on
    /// the other side of the surface. The convention is such that a solid sphere, with a light ray
    /// entering from outside, should have index of refraction greater than 1.
    /// The probability is the probability that a ray will refract, so 0 yields a perfectly reflecting sphere.
    | Dielectric of
        albedo : float<albedo> *
        texture : Texture *
        boundaryRefractance : float<ior> *
        refraction : float<prob> *
        FloatProducer
    /// A glass material which uses Schlick's approximation for reflectance probability.
    | Glass of albedo : float<albedo> * texture : Texture * float<ior> * FloatProducer

type Orientation =
    | Inside
    | Outside

[<RequireQualifiedAccess>]
module Sphere =

    /// Parameterisation of a sphere of radius 1 centred on 0,0,0 by points in the box [0, 1] x [0, 1]
    let planeMap (radius : float) (centre : Point) (phi : float) (theta : float) : Point =
        let theta = theta * System.Math.PI
        let phi = phi * System.Math.PI * 2.0 - System.Math.PI

        Point.make (radius * cos phi * sin theta) (-radius * cos theta) (-radius * sin phi * sin theta)
        |> Point.sum centre

    /// Give back the phi and theta (scaled to 0..1 each) that result in this point.
    let planeMapInverse (radius : float) (centre : Point) (p : Point) : struct (float * float) =
        let (Vector (x, y, z)) =
            Point.differenceToThenFrom p centre |> Vector.scale (1.0 / radius)

        let theta = acos (-y)
        let phi = atan2 (-z) x + System.Math.PI
        struct ((phi / (2.0 * System.Math.PI)), theta / System.Math.PI)

    /// A ray hits the sphere with centre `centre` at point `p`.
    /// This function gives the outward-pointing normal.
    let normal (centre : Point) (p : Point) : Ray =
        Ray.make' p (Point.differenceToThenFrom p centre) |> ValueOption.get

    let private liesOn' (centre : Point) (radius : float) (p : Point) : bool =
        let rSquared = radius * radius
        Float.equal (Vector.normSquared (Point.differenceToThenFrom p centre)) rSquared

    let reflection
        (style : SphereStyle)
        (centre : Point)
        (radius : float)
        (radiusSquared : float)
        (flipped : bool)
        (incomingLight : LightRay)
        (strikePoint : Point)
        : LightDestination
        =
        let normal = normal centre strikePoint

        // If the incoming ray is on the sphere, then we have to be an internal ray, so the normal is flipped.
        // But to model a glass shell (not a sphere), we allow negative radius, which contributes a flipping term.
        let inside, normal =
            match
                Float.compare
                    (Vector.normSquared (Point.differenceToThenFrom centre (Ray.origin incomingLight.Ray)))
                    radiusSquared
            with
            | Equal
            | Less ->
                // Point is inside or on the sphere so we are coming from within
                if flipped then
                    false, normal
                else
                    true, Ray.make (Ray.origin normal) (UnitVector.flip (Ray.vector normal))
            | Greater ->
                if flipped then
                    true, Ray.make (Ray.origin normal) (UnitVector.flip (Ray.vector normal))
                else
                    false, normal

        let fuzzedReflection (fuzz : (float<fuzz> * FloatProducer) option) =
            let plane = Plane.makeSpannedBy normal incomingLight.Ray |> Plane.orthonormalise

            let outgoing =
                match plane with
                | ValueNone ->
                    // Incoming ray is directly along the normal
                    Ray.flip incomingLight.Ray |> Ray.parallelTo strikePoint
                | ValueSome plane ->
                    // Incoming ray is (plane1.ray) plane1 + (plane2.ray) plane2
                    // We want the reflection in the normal, so need (plane1.ray) plane1 - (plane2.ray) plane2
                    let normalComponent = -UnitVector.dot plane.V1 (Ray.vector incomingLight.Ray)
                    let tangentComponent = (UnitVector.dot plane.V2 (Ray.vector incomingLight.Ray))

                    let dest =
                        Ray.walkAlong
                            (Ray.make (Ray.walkAlong (Ray.make plane.Point plane.V1) normalComponent) plane.V2)
                            tangentComponent

                    Point.differenceToThenFrom dest strikePoint
                    |> Ray.make' strikePoint
                    // This is safe: it's actually a logic error for this to fail.
                    |> ValueOption.get

            match fuzz with
            | None -> outgoing
            | Some (fuzz, rand) ->
                let mutable answer = Unchecked.defaultof<_>

                while obj.ReferenceEquals (answer, null) do
                    let offset = UnitVector.random rand (Point.dimension centre)
                    let sphereCentre = Ray.walkAlong outgoing 1.0
                    let target = Ray.walkAlong (Ray.make sphereCentre offset) (fuzz / 1.0<fuzz>)

                    let exitPoint =
                        Point.differenceToThenFrom target strikePoint |> Ray.make' strikePoint

                    match exitPoint with
                    | ValueNone -> ()
                    | ValueSome o -> answer <- o

                answer

        let refract (incomingCos : float) (index : float<ior>) =
            let index = if inside then 1.0<ior> / index else index / 1.0<ior>
            let plane = Plane.makeSpannedBy normal incomingLight.Ray |> Plane.orthonormalise

            match plane with
            | ValueNone ->
                // Incoming ray was parallel to normal; pass straight through
                Ray.make strikePoint (Ray.vector incomingLight.Ray)
            | ValueSome plane ->

            let incomingSin = sqrt (1.0 - incomingCos * incomingCos)
            let outgoingSin = incomingSin / index

            if Float.compare outgoingSin 1.0 = Greater then
                // override our decision to refract - from this angle, there's no way we could have refracted
                fuzzedReflection None

            else

            let outgoingCos = sqrt (1.0 - outgoingSin * outgoingSin)

            let outgoingPoint =
                Ray.walkAlong (Ray.make (Ray.walkAlong normal (-outgoingCos)) plane.V2) outgoingSin

            Point.differenceToThenFrom outgoingPoint strikePoint
            |> Ray.make' strikePoint
            // This is safe: it's a logic error for this to fail. It would imply both the
            // cos and the sin outgoing components were 0.
            |> ValueOption.get

        match style with
        | SphereStyle.LightSource texture ->
            texture
            |> Texture.colourAt strikePoint
            |> Pixel.combine incomingLight.Colour
            |> Absorbs
        | SphereStyle.LightSourceCap colour ->
            let circleCentreZCoord = Point.coordinate 0 centre
            let zCoordLowerBound = circleCentreZCoord + (radius - (radius / 4.0))
            let strikeZCoord = Point.coordinate 0 strikePoint

            let colour =
                match Float.compare strikeZCoord zCoordLowerBound with
                | Greater -> Pixel.combine colour incomingLight.Colour
                | _ -> Colour.Black

            Absorbs colour

        | SphereStyle.LambertReflection (albedo, texture, rand) ->
            let outgoing =
                let sphereCentre = Ray.walkAlong normal 1.0
                let mutable answer = Unchecked.defaultof<_>

                while obj.ReferenceEquals (answer, null) do
                    let offset = UnitVector.random rand (Point.dimension sphereCentre)
                    let target = Ray.walkAlong (Ray.make sphereCentre offset) 1.0

                    let outputPoint =
                        Point.differenceToThenFrom target strikePoint |> Ray.make' strikePoint

                    match outputPoint with
                    | ValueSome o -> answer <- o
                    | ValueNone -> ()

                answer

            let newColour =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            Continues
                {
                    Ray = outgoing
                    Colour = newColour
                }

        | SphereStyle.PureReflection (albedo, texture) ->
            let darkened =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            Continues
                {
                    Ray = fuzzedReflection None
                    Colour = darkened
                }

        | SphereStyle.FuzzedReflection (albedo, texture, fuzz, random) ->
            let darkened =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            Continues
                {
                    Ray = fuzzedReflection (Some (fuzz, random))
                    Colour = darkened
                }

        | SphereStyle.Dielectric (albedo, texture, sphereRefractance, refractionProb, random) ->
            let newColour =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            let rand = random.Get ()

            if LanguagePrimitives.FloatWithMeasure rand > refractionProb then
                // reflect!
                Continues
                    {
                        Ray = fuzzedReflection None
                        Colour = newColour
                    }
            else
                let incomingCos = UnitVector.dot (Ray.vector incomingLight.Ray) (Ray.vector normal)

                Continues
                    {
                        Ray = refract incomingCos sphereRefractance
                        Colour = newColour
                    }

        | SphereStyle.Glass (albedo, texture, sphereRefractance, random) ->
            let newColour =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            let incomingCos =
                UnitVector.dot (UnitVector.flip (Ray.vector incomingLight.Ray)) (Ray.vector normal)

            let rand = random.Get ()

            let reflectionProb =
                let sphereRefractance =
                    if inside then
                        1.0<ior * ior> / sphereRefractance
                    else
                        sphereRefractance

                let param = (1.0<ior> - sphereRefractance) / (1.0<ior> + sphereRefractance)
                let param = param * param
                param + (1.0 - param) * ((1.0 - incomingCos) ** 5.0)

            if LanguagePrimitives.FloatWithMeasure rand < reflectionProb then
                // reflect!
                Continues
                    {
                        Ray = fuzzedReflection None
                        Colour = newColour
                    }
            else
                Continues
                    {
                        Ray = refract incomingCos sphereRefractance
                        Colour = newColour
                    }

    let make (style : SphereStyle) (centre : Point) (radius : float) : Sphere =
        let radiusSquared = radius * radius

        {
            Centre = centre
            Radius = radius
            Reflection = reflection style centre radius radiusSquared (Float.compare radius 0.0 = Less)
            RadiusSquared = radiusSquared
            BoundingBox =
                BoundingBox.make
                    (Point.sum centre (Point.make -radius -radius -radius))
                    (Point.sum centre (Point.make radius radius radius))
        }

    let boundingBox (s : Sphere) = s.BoundingBox

    let liesOn (point : Point) (sphere : Sphere) : bool =
        liesOn' sphere.Centre sphere.Radius point

    /// Returns the distance along this ray at which the nearest intersection of the ray lies with this sphere.
    /// Does not return any intersections which are behind us.
    /// If the sphere is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    let firstIntersection (sphere : Sphere) (ray : Ray) : float voption =
        let difference = Point.differenceToThenFrom (Ray.origin ray) sphere.Centre

        let b = (UnitVector.dot' (Ray.vector ray) difference)

        let c = (Vector.normSquared difference) - sphere.RadiusSquared

        let discriminantOverFour = (b * b - c)

        let intersectionPoint =
            match Float.compare discriminantOverFour 0.0 with
            | Comparison.Equal -> Some (-b)
            | Comparison.Less -> None
            | Comparison.Greater ->
                let intermediate = sqrt discriminantOverFour
                let i1 = intermediate - b
                let i2 = -(b + intermediate)
                let i1Pos = Float.positive i1
                let i2Pos = Float.positive i2

                if i1Pos && i2Pos then
                    match Float.compare i1 i2 with
                    | Less -> i1
                    | Greater -> i2
                    | Equal -> i1
                    |> Some
                elif i1Pos then
                    Some i1
                elif i2Pos then
                    Some i2
                else
                    None

        match intersectionPoint with
        | None -> ValueNone
        | Some i ->
            // Don't return anything that's behind us
            if Float.positive i then ValueSome i else ValueNone
