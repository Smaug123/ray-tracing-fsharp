namespace RayTracing

type InfinitePlaneStyle =
    /// An emitter of light.
    | LightSource of Pixel
    /// Perfect reflection, as you would see from a smooth flat metal surface.
    /// Albedo must be between 0 and 1.
    | PureReflection of albedo : float<albedo> * colour : Pixel
    /// An ideal matte (diffusely-reflecting) surface: apparent brightness of the
    /// surface is the same regardless of the angle of view.
    /// Albedo must be between 0 and 1.
    | LambertReflection of albedo : float<albedo> * colour : Pixel * FloatProducer
    | FuzzedReflection of albedo : float<albedo> * colour : Pixel * fuzz : float<fuzz> * FloatProducer

type InfinitePlane =
    {
        Normal : UnitVector
        Point : Point
        /// If an incoming ray hits the given point (which is guaranteed to be on the surface),
        /// what colour ray does it output and in what direction?
        Reflection : LightRay -> Point -> LightDestination
    }

[<RequireQualifiedAccess>]
module InfinitePlane =

    /// Returns the position along this ray where we intersect this plane, or None if none exists or the ray is in the plane.
    /// Does not return any intersections which are behind us.
    /// If the plane is made of a material which does not re-emit light, you'll
    /// get a None for the outgoing ray.
    let intersection
        (plane : InfinitePlane)
        (ray : Ray)
        : float voption
        =
        let rayVec = Ray.vector ray
        let denominator = UnitVector.dot plane.Normal rayVec
        if Float.equal denominator 0.0 then ValueNone
        else
            let t = (UnitVector.dot' plane.Normal (Point.differenceToThenFrom plane.Point (Ray.origin ray))) / denominator
            if Float.positive t then
                ValueSome t
            else ValueNone

    let pureOutgoing (strikePoint : Point) (normal : UnitVector) (incomingRay : Ray) : Ray =
        let plane =
            Plane.makeSpannedBy (Ray.make strikePoint normal) incomingRay
            |> Plane.orthonormalise
        match plane with
        | None ->
            // Incoming ray is directly along the normal
            Ray.flip incomingRay
            |> Ray.parallelTo strikePoint
        | Some plane ->
            // Incoming ray is (plane1.ray) plane1 + (plane2.ray) plane2
            // We want the reflection in the normal, so need (plane1.ray) plane1 - (plane2.ray) plane2
            let normalComponent = - (UnitVector.dot plane.V1 (Ray.vector incomingRay))
            let tangentComponent = (UnitVector.dot plane.V2 (Ray.vector incomingRay))
            let s =
                tangentComponent
                |> Ray.walkAlong (Ray.make (Ray.walkAlong (Ray.make plane.Point plane.V1) normalComponent) plane.V2)
            Point.differenceToThenFrom s strikePoint
            |> Ray.make' strikePoint
            // This is definitely safe. It's actually a logic error if this fails.
            |> Option.get

    let newColour (incomingColour : Pixel) albedo colour =
        Pixel.combine incomingColour colour
        |> Pixel.darken albedo

    let reflection
        (style : InfinitePlaneStyle)
        (pointOnPlane : Point)
        (normal : UnitVector)
        : LightRay -> Point -> LightDestination
        =
        fun incomingRay strikePoint ->
            match style with
            | InfinitePlaneStyle.LightSource colour ->
                Absorbs (Pixel.combine incomingRay.Colour colour)

            | InfinitePlaneStyle.FuzzedReflection (albedo, colour, fuzz, rand) ->
                let newColour = newColour incomingRay.Colour albedo colour
                let pureOutgoing = pureOutgoing strikePoint normal incomingRay.Ray
                let mutable outgoing = Unchecked.defaultof<_>
                while obj.ReferenceEquals (outgoing, null) do
                    let offset = UnitVector.random rand (Point.dimension pointOnPlane)
                    let sphereCentre = Ray.walkAlong pureOutgoing 1.0
                    let target = Ray.walkAlong (Ray.make sphereCentre offset) (fuzz / 1.0<fuzz>)
                    let output =
                        Point.differenceToThenFrom target strikePoint
                        |> Ray.make' strikePoint
                    match output with
                    | None -> ()
                    | Some output ->
                        outgoing <- output

                Continues { Ray = outgoing ; Colour = newColour }

            | InfinitePlaneStyle.LambertReflection (albedo, colour, rand) ->
                let outgoing =
                    let sphereCentre = Ray.walkAlong (Ray.make strikePoint normal) 1.0
                    let offset = UnitVector.random rand (Point.dimension pointOnPlane)
                    let target = Ray.walkAlong (Ray.make sphereCentre offset) 1.0
                    Point.differenceToThenFrom target strikePoint
                    |> Ray.make' strikePoint
                    |> Option.get

                let newColour =
                    Pixel.combine incomingRay.Colour colour
                    |> Pixel.darken albedo
                Continues { Ray = outgoing ; Colour = newColour }

            | InfinitePlaneStyle.PureReflection (albedo, colour) ->
                {
                    Ray = pureOutgoing strikePoint normal incomingRay.Ray
                    Colour = newColour incomingRay.Colour albedo colour
                }
                |> Continues

    let make (style : InfinitePlaneStyle) (pointOnPlane : Point) (normal : UnitVector) : InfinitePlane =
        {
            Point = pointOnPlane
            Normal = normal
            Reflection = reflection style pointOnPlane normal
        }
