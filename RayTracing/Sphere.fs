namespace RayTracing

[<Measure>]
type fuzz

/// A probability, between 0 and 1.
[<Measure>]
type prob

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

    let private reflectWithoutFuzz normal (strikePoint : Point) (incomingLight : byref<LightRay>) : unit =
        let plane = Plane.makeOrthonormalSpannedBy normal incomingLight.Ray

        match plane with
        | ValueNone ->
            // Incoming ray is directly along the normal
            Ray.flipInPlace incomingLight.Ray
            Ray.translateToIntersect strikePoint incomingLight.Ray
        | ValueSome plane ->
            // Incoming ray is (plane1.ray) plane1 + (plane2.ray) plane2
            // We want the reflection in the normal, so need (plane1.ray) plane1 - (plane2.ray) plane2
            let normalComponent = -UnitVector.dot plane.V1 (Ray.vector incomingLight.Ray)
            let tangentComponent = (UnitVector.dot plane.V2 (Ray.vector incomingLight.Ray))

            let dest =
                Ray.walkAlongRay (Ray.walkAlongRay plane.Point plane.V1 normalComponent) plane.V2 tangentComponent

            Ray.overwriteWithMake strikePoint (Point.differenceToThenFrom dest strikePoint) &incomingLight.Ray
            // This is safe: it's actually a logic error for this to fail.
            |> ignore

    let private addFuzz
        (fuzz : float<fuzz>)
        (rand : FloatProducer)
        (strikePoint : Point)
        (reflected : byref<LightRay>)
        : unit
        =
        let mutable isDone = false

        while not isDone do
            let offset = UnitVector.random rand (Point.dimension strikePoint)
            let sphereCentre = Ray.walkAlong reflected.Ray 1.0
            let target = Ray.walkAlongRay sphereCentre offset (fuzz / 1.0<fuzz>)

            let newDirection = Point.differenceToThenFrom target strikePoint
            isDone <- Ray.overwriteWithMake strikePoint newDirection &reflected.Ray

    /// If there were no refraction at all, the reflected ray would bounce off as `reflectionWithoutFuzz`.
    /// This function adds a refraction term.
    let private refract
        (inside : bool)
        (normal : Ray)
        (strikePoint : Point)
        (incomingCos : float)
        (index : float<ior>)
        (incomingLight : byref<LightRay>)
        : unit
        =
        let index = if inside then 1.0<ior> / index else index / 1.0<ior>
        let plane = Plane.makeOrthonormalSpannedBy normal incomingLight.Ray

        match plane with
        | ValueNone ->
            // Incoming ray was parallel to normal; pass straight through
            let (UnitVector vec) = Ray.vector incomingLight.Ray
            Ray.overwriteWithMake strikePoint vec &incomingLight.Ray |> ignore
        | ValueSome plane ->

        let incomingSin = sqrt (1.0 - incomingCos * incomingCos)
        let outgoingSin = incomingSin / index

        if Float.compare outgoingSin 1.0 = Greater then
            // override our decision to refract - from this angle, there's no way we could have refracted
            reflectWithoutFuzz normal strikePoint &incomingLight

        else

        let outgoingCos = sqrt (1.0 - outgoingSin * outgoingSin)

        let outgoingPoint =
            Ray.walkAlongRay (Ray.walkAlong normal (-outgoingCos)) plane.V2 outgoingSin

        let outgoingLine = Point.differenceToThenFrom outgoingPoint strikePoint

        Ray.overwriteWithMake strikePoint outgoingLine &incomingLight.Ray
        // This is safe: it's a logic error for this to fail. It would imply both the
        // cos and the sin outgoing components were 0.
        |> ignore

    /// If the light ray is absorbed, this returns Some(the colour of light).
    /// Otherwise, returns None and mutates `incomingLight`.
    let reflection
        (style : SphereStyle)
        (centre : Point)
        (radius : float)
        (radiusSquared : float)
        (flipped : bool)
        (incomingLight : byref<LightRay>)
        (strikePoint : Point)
        : Pixel ValueOption
        =
        // If the incoming ray is on the sphere, then we have to be an internal ray, so the normal is flipped.
        // But to model a glass shell (not a sphere), we allow negative radius, which contributes a flipping term.
        let mutable inside = false
        let mutable normal = normal centre strikePoint

        match
            Float.compare
                (Vector.normSquared (Point.differenceToThenFrom centre (Ray.origin incomingLight.Ray)))
                radiusSquared
        with
        | Equal
        | Less ->
            // Point is inside or on the sphere so we are coming from within
            if not flipped then
                inside <- true
                Ray.flipInPlace normal
        | Greater ->
            if flipped then
                inside <- true
                Ray.flipInPlace normal

        let inside = inside
        let normal = normal

        match style with
        | SphereStyle.LightSource texture ->
            texture
            |> Texture.colourAt strikePoint
            |> Pixel.combine incomingLight.Colour
            |> ValueSome
        | SphereStyle.LightSourceCap colour ->
            let circleCentreZCoord = Point.coordinate 0 centre
            let zCoordLowerBound = circleCentreZCoord + (radius - (radius / 4.0))
            let strikeZCoord = Point.coordinate 0 strikePoint

            let colour =
                match Float.compare strikeZCoord zCoordLowerBound with
                | Greater -> Pixel.combine colour incomingLight.Colour
                | _ -> Colour.Black

            ValueSome colour

        | SphereStyle.LambertReflection (albedo, texture, rand) ->
            let newColour =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            incomingLight.Colour <- newColour

            let sphereCentre = Ray.walkAlong normal 1.0
            let mutable isDone = false

            while not isDone do
                let offset = UnitVector.random rand (Point.dimension sphereCentre)
                let target = Ray.walkAlongRay sphereCentre offset 1.0

                let outputVec = Point.differenceToThenFrom target strikePoint

                isDone <- Ray.overwriteWithMake strikePoint outputVec &incomingLight.Ray

            ValueNone

        | SphereStyle.PureReflection (albedo, texture) ->
            let darkened =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            reflectWithoutFuzz normal strikePoint &incomingLight
            incomingLight.Colour <- darkened
            ValueNone

        | SphereStyle.FuzzedReflection (albedo, texture, fuzz, random) ->
            let darkened =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            incomingLight.Colour <- darkened

            reflectWithoutFuzz normal strikePoint &incomingLight
            addFuzz fuzz random strikePoint &incomingLight
            ValueNone

        | SphereStyle.Dielectric (albedo, texture, sphereRefractance, refractionProb, random) ->
            let newColour =
                texture
                |> Texture.colourAt strikePoint
                |> Pixel.combine incomingLight.Colour
                |> Pixel.darken albedo

            let rand = random.Get ()

            if LanguagePrimitives.FloatWithMeasure rand > refractionProb then
                // reflect!
                incomingLight.Colour <- newColour
                reflectWithoutFuzz normal strikePoint &incomingLight
                ValueNone
            else
                let incomingCos = UnitVector.dot (Ray.vector incomingLight.Ray) (Ray.vector normal)

                refract inside normal strikePoint incomingCos sphereRefractance &incomingLight
                incomingLight.Colour <- newColour
                ValueNone

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
                reflectWithoutFuzz normal strikePoint &incomingLight
                incomingLight.Colour <- newColour
                ValueNone
            else
                refract inside normal strikePoint incomingCos sphereRefractance &incomingLight
                incomingLight.Colour <- newColour
                ValueNone

type Sphere =
    private
        {
            Centre : Point
            Radius : float
            RadiusSquared : float
            BoundingBox : BoundingBox
            Style : SphereStyle
        }

    /// If an incoming ray has the given colour, and hits the
    /// given point (which is guaranteed to be on the surface),
    /// does it get absorbed? If not, mutates the input `ray` to hold the new light ray.
    member this.Reflection (ray : byref<LightRay>, strikePoint : Point) : Pixel ValueOption =
        Sphere.reflection
            this.Style
            this.Centre
            this.Radius
            this.RadiusSquared
            (Float.compare this.Radius 0.0 = Less)
            &ray
            strikePoint

    static member make (style : SphereStyle) (centre : Point) (radius : float) : Sphere =
        let radiusSquared = radius * radius

        {
            Style = style
            Centre = centre
            Radius = radius
            RadiusSquared = radiusSquared
            BoundingBox =
                BoundingBox.make
                    (Point.sum centre (Point.make -radius -radius -radius))
                    (Point.sum centre (Point.make radius radius radius))
        }

    static member boundingBox (s : Sphere) = s.BoundingBox

    static member liesOn (point : Point) (sphere : Sphere) : bool =
        let rSquared = sphere.RadiusSquared
        Float.equal (Vector.normSquared (Point.differenceToThenFrom point sphere.Centre)) rSquared

    /// Returns the distance along this ray at which the nearest intersection of the ray lies with this sphere.
    /// Does not return any intersections which are behind us.
    /// If the sphere is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    static member firstIntersection (sphere : Sphere) (ray : Ray) : float voption =
        let difference = Point.differenceToThenFrom (Ray.origin ray) sphere.Centre

        let b = (UnitVector.dot' (Ray.vector ray) difference)

        let c = (Vector.normSquared difference) - sphere.RadiusSquared

        let discriminantOverFour = (b * b - c)

        let intersectionPoint =
            match Float.compare discriminantOverFour 0.0 with
            | Comparison.Equal -> ValueSome (-b)
            | Comparison.Less -> ValueNone
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
                    |> ValueSome
                elif i1Pos then
                    ValueSome i1
                elif i2Pos then
                    ValueSome i2
                else
                    ValueNone

        match intersectionPoint with
        | ValueNone -> ValueNone
        | ValueSome i ->
            // Don't return anything that's behind us
            if Float.positive i then ValueSome i else ValueNone
