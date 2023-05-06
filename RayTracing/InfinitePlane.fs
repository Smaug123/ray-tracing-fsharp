namespace RayTracing

type InfinitePlaneStyle =
    /// An emitter of light.
    | LightSource of Texture
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : float<albedo> * colour : Pixel
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : float<albedo> * colour : Pixel * FloatProducer
    | FuzzedReflection of albedo : float<albedo> * colour : Pixel * fuzz : float<fuzz> * FloatProducer

[<RequireQualifiedAccess>]
module InfinitePlane =

    let pureOutgoing (strikePoint : Point) (normal : UnitVector) (incomingRay : byref<Ray>) : unit =
        let plane = Plane.makeOrthonormalSpannedBy' strikePoint normal incomingRay

        match plane with
        | ValueNone ->
            // Incoming ray is directly along the normal
            Ray.flipInPlace incomingRay
            Ray.translateToIntersect strikePoint incomingRay
        | ValueSome plane ->
            // Incoming ray is (plane1.ray) plane1 + (plane2.ray) plane2
            // We want the reflection in the normal, so need (plane1.ray) plane1 - (plane2.ray) plane2
            let normalComponent = -(UnitVector.dot plane.V1 (Ray.vector incomingRay))
            let tangentComponent = (UnitVector.dot plane.V2 (Ray.vector incomingRay))

            let s =
                // (plane.Point + plane.V1 * normalComponent) + plane.V2 * tangentComponent
                tangentComponent
                |> Ray.walkAlongRay (Ray.walkAlongRay plane.Point plane.V1 normalComponent) plane.V2


            let newVector =
                Point.differenceToThenFrom s strikePoint
                |> Vector.unitise
                // This is definitely safe. It's actually a logic error if this fails.
                |> ValueOption.get

            incomingRay.Origin <- strikePoint
            incomingRay.Vector <- newVector

    let newColour (incomingColour : Pixel) albedo colour =
        Pixel.combine incomingColour colour |> Pixel.darken albedo

    let reflection
        (style : InfinitePlaneStyle)
        (pointOnPlane : Point)
        (normal : UnitVector)
        (incomingRay : byref<LightRay>)
        (strikePoint : Point)
        : Pixel ValueOption
        =
        match style with
        | InfinitePlaneStyle.LightSource texture ->
            texture
            |> Texture.colourAt strikePoint
            |> Pixel.combine incomingRay.Colour
            |> ValueSome

        | InfinitePlaneStyle.FuzzedReflection (albedo, colour, fuzz, rand) ->
            let newColour = newColour incomingRay.Colour albedo colour
            pureOutgoing strikePoint normal &incomingRay.Ray
            // Henceforth `incomingRay` is actually the outgoing ray: we mutated it above.
            let mutable isDone = false

            while not isDone do
                let offset = UnitVector.random rand (Point.dimension pointOnPlane)
                let sphereCentre = Ray.walkAlong incomingRay.Ray 1.0
                let target = Ray.walkAlongRay sphereCentre offset (fuzz / 1.0<fuzz>)
                let outgoing = Point.differenceToThenFrom target strikePoint

                match Vector.unitise outgoing with
                | ValueNone -> ()
                | ValueSome output ->
                    incomingRay.Ray.Vector <- output
                    Ray.translateToIntersect strikePoint incomingRay.Ray
                    isDone <- true

            incomingRay.Colour <- newColour

            ValueNone

        | InfinitePlaneStyle.LambertReflection (albedo, colour, rand) ->
            let outgoing =
                let sphereCentre = Ray.walkAlongRay strikePoint normal 1.0
                let offset = UnitVector.random rand (Point.dimension pointOnPlane)
                let target = Ray.walkAlongRay sphereCentre offset 1.0

                Point.differenceToThenFrom target strikePoint
                |> Ray.make' strikePoint
                |> ValueOption.get

            let newColour = Pixel.combine incomingRay.Colour colour |> Pixel.darken albedo

            incomingRay.Colour <- newColour
            incomingRay.Ray <- outgoing

            ValueNone

        | InfinitePlaneStyle.PureReflection (albedo, colour) ->
            let newColour = newColour incomingRay.Colour albedo colour
            incomingRay.Colour <- newColour
            pureOutgoing strikePoint normal &incomingRay.Ray

            ValueNone

type InfinitePlane =
    {
        Style : InfinitePlaneStyle
        Normal : UnitVector
        Point : Point
    }

    /// If an incoming ray hits the given point (which is guaranteed to be on the surface),
    /// is it absorbed (if so, returns Some(the colour of light)), or does it bounce off
    /// (if so, returns None and mutates the input ray to the new reflected ray)?
    member this.Reflection (ray : byref<LightRay>, strikePoint : Point) : Pixel ValueOption =
        InfinitePlane.reflection this.Style this.Point this.Normal &ray strikePoint

    static member make (style : InfinitePlaneStyle) (pointOnPlane : Point) (normal : UnitVector) : InfinitePlane =
        {
            Point = pointOnPlane
            Style = style
            Normal = normal
        }

    /// Returns the position along this ray where we intersect this plane, or None if none exists or the ray is in the plane.
    /// Does not return any intersections which are behind us.
    /// If the plane is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    static member intersection (plane : InfinitePlane) (ray : Ray) : float voption =
        let rayVec = Ray.vector ray
        let denominator = UnitVector.dot plane.Normal rayVec

        if Float.equal denominator 0.0 then
            ValueNone
        else
            let t =
                (UnitVector.dot' plane.Normal (Point.differenceToThenFrom plane.Point (Ray.origin ray)))
                / denominator

            if Float.positive t then ValueSome t else ValueNone
