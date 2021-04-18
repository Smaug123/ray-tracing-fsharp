namespace RayTracing

open System

type Hittable =
    | Sphere of Sphere
    | InfinitePlane of InfinitePlane

    member this.Reflection (incoming : LightRay) (strikePoint : Point) =
        match this with
        | Sphere s -> s.Reflection incoming strikePoint
        | InfinitePlane p -> p.Reflection incoming strikePoint

[<RequireQualifiedAccess>]
module Hittable =
    /// Returns the distance we must walk along this ray before we first hit an object, the
    /// colour the resulting light ray is after the interaction, and the new ray.
    let hits
        (ray : Ray)
        (h : Hittable)
        : float voption
        =
        match h with
        | Sphere s ->
            Sphere.firstIntersection s ray
        | InfinitePlane plane ->
            InfinitePlane.intersection plane ray

type Scene =
    {
        Objects : Hittable array
    }

[<RequireQualifiedAccess>]
module Scene =

    let hitObject
        (s : Scene)
        (ray : Ray)
        : (int * Point) option
        =
        let mutable bestIndex = -1
        let mutable bestLength = nan
        let mutable bestFloat = infinity
        for i in 0..s.Objects.Length - 1 do
            match Hittable.hits ray s.Objects.[i] with
            | ValueNone -> ()
            | ValueSome point ->
                let a = point * point
                match Float.compare a bestFloat with
                | Less ->
                    bestFloat <- a
                    bestIndex <- i
                    bestLength <- point
                | _ -> ()

        if Double.IsNaN bestLength then None else
        Some (bestIndex, Ray.walkAlong ray bestLength)

    let internal traceRayPrinting
        (print : string -> unit)
        (maxCount : int)
        (scene : Scene)
        (ray : LightRay)
        : Pixel
        =
        let rec go (bounces : int) (ray : LightRay) : Pixel =
            let (Point(x, y, z)) = Ray.origin ray.Ray
            let (UnitVector (Vector(a, b, c))) = Ray.vector ray.Ray
            print (sprintf "Ray, colour %i,%i,%i\n  origin (%f, %f, %f)\n  vector (%f, %f, %f)" ray.Colour.Red ray.Colour.Green ray.Colour.Blue x y z a b c)
            if bounces > maxCount then Colour.HotPink else

            let thingsWeHit = hitObject scene ray.Ray
            match thingsWeHit with
            | None ->
                print ">>> No object collision; black."
                // Ray goes off into the distance and is never heard from again
                Colour.Black
            | Some (objectNumber, strikePoint) ->
                let (Point(x, y, z)) = strikePoint
                print (sprintf ">>> collided with object %i at (%f, %f, %f)" objectNumber x y z)
                let outgoingRay = scene.Objects.[objectNumber].Reflection ray strikePoint
                match outgoingRay with
                | Absorbs colour ->
                    print (sprintf ">>>   surface absorbs, yielding colour %i,%i,%i" colour.Red colour.Green colour.Blue)
                    colour
                | Continues outgoingRay ->
                    print ">>>   continuing tracing."
                    go (bounces + 1) outgoingRay

        go 0 ray

    let internal traceRay
        (maxCount : int)
        (scene : Scene)
        (ray : LightRay)
        : Pixel
        =
        let rec go (bounces : int) (ray : LightRay) : Pixel =
            if bounces > maxCount then
                if ray.Colour = Colour.Black then Colour.Black else Colour.HotPink
            else

            let thingsWeHit = hitObject scene ray.Ray
            match thingsWeHit with
            | None ->
                // Ray goes off into the distance and is never heard from again
                Colour.Black
            | Some (objectNumber, strikePoint) ->
                let outgoingRay = scene.Objects.[objectNumber].Reflection ray strikePoint
                match outgoingRay with
                | Absorbs colour ->
                    colour
                | Continues outgoingRay ->
                    go (bounces + 1) outgoingRay

        go 0 ray

    /// Trace a ray to this one pixel, updating the PixelStats with the result.
    /// n.b. not thread safe
    let private traceOnce (print : string -> unit) (scene : Scene) (rand : FloatProducer) (camera : Camera) (maxWidthCoord : int) (maxHeightCoord : int) row col stats =
        let struct(rand1, rand2) = rand.GetTwo ()
        let landingPoint =
            ((float col + rand1) * camera.ViewportWidth) / float maxWidthCoord
        let pointOnXAxis =
            landingPoint
            |> Ray.walkAlong camera.ViewportXAxis
        let toWalkUp = Ray.parallelTo pointOnXAxis camera.ViewportYAxis
        let endPoint =
            ((float row + rand2) * camera.ViewportHeight) / float maxHeightCoord
            |> Ray.walkAlong toWalkUp
        let ray =
            Ray.make' (Ray.origin camera.View) (Point.differenceToThenFrom endPoint (Ray.origin camera.View))
            |> Option.get

        // Here we've hardcoded that the eye is emitting white light through a medium with refractance 1.
        let result = traceRay 150 scene { Ray = ray ; Colour = Colour.White }
        //if result = Colour.HotPink then
        //    print "hi"
        //    traceRayPrinting print 150 scene { Ray = ray ; Colour = Colour.White ; Refractance = 1.0<ior> }
        //    |> ignore
        //    failwith "Stopping."
        PixelStats.add result stats

    let renderPixel (print : string -> unit) (scene : Scene) (rand : FloatProducer) (camera : Camera) maxWidthCoord maxHeightCoord row col =
        // Where does this pixel correspond to, on the imaginary canvas?
        // For the early prototype, we'll just take the upper right quadrant
        // from the camera.
        let stats = PixelStats.empty ()

        let firstTrial = min 5 (camera.SamplesPerPixel / 2)

        for _ in 0..firstTrial do
            traceOnce print scene rand camera maxWidthCoord maxHeightCoord row col stats

        let oldMean = PixelStats.mean stats

        for _ in 1..firstTrial do
            traceOnce print scene rand camera maxWidthCoord maxHeightCoord row col stats

        let newMean = PixelStats.mean stats
        let difference = Pixel.difference newMean oldMean

        if difference = 0 then
           // The mean didn't really change when we added another five samples; assume it's not going to change
           // with more.
           newMean
        else

            for _ in 1..(camera.SamplesPerPixel - 2 * firstTrial - 1) do
                traceOnce print scene rand camera maxWidthCoord maxHeightCoord row col stats

            PixelStats.mean stats

    let render
        (progressIncrement : float<progress> -> unit)
        (print : string -> unit)
        (maxWidthCoord : int)
        (maxHeightCoord : int)
        (camera : Camera)
        (s : Scene)
        : float<progress> * Image
        =
        let rand = FloatProducer (Random ())
        // For each pixel in the output, send a ray from the camera
        // in the direction of that pixel.
        let rowsIter = 2 * maxHeightCoord + 1
        let colsIter = 2 * maxWidthCoord + 1
        1.0<progress> * float (rowsIter * colsIter),
        {
            RowCount = rowsIter
            ColCount = colsIter
            Rows =
                Array.init rowsIter (fun row ->
                    let row = maxHeightCoord - row - 1
                    Array.init colsIter (fun col ->
                        let col = col - maxWidthCoord
                        async {
                            let ret = renderPixel print s rand camera maxWidthCoord maxHeightCoord row col
                            progressIncrement 1.0<progress>
                            return ret
                        }
                    )
                )
        }