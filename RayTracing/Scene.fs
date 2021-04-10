namespace RayTracing

open System

type Hittable =
    | Sphere of Sphere
    | InfinitePlane of InfinitePlane

    member this.Reflection (incoming : Ray) (incomingColour : Pixel) (strikePoint : Point) =
        match this with
        | Sphere s -> s.Reflection incoming incomingColour strikePoint
        | InfinitePlane p -> p.Reflection incoming incomingColour strikePoint

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

    let internal traceRay
        (maxCount : int)
        (scene : Scene)
        (ray : Ray)
        (colour : Pixel)
        : Pixel
        =
        let rec go (bounces : int) (ray : Ray) (colour : Pixel) : Pixel =
            if bounces > maxCount then Colour.HotPink else

            let thingsWeHit = hitObject scene ray
            match thingsWeHit with
            | None ->
                // Ray goes off into the distance and is never heard from again
                Colour.Black
            | Some (objectNumber, strikePoint) ->
                let outgoingRay, colour = scene.Objects.[objectNumber].Reflection ray colour strikePoint
                match outgoingRay with
                | None ->
                    colour
                | Some outgoingRay ->
                    go (bounces + 1) outgoingRay colour

        go 0 ray colour

    let renderPixel (scene : Scene) (rand : FloatProducer) (camera : Camera) maxWidthCoord maxHeightCoord row col =
        // Where does this pixel correspond to, on the imaginary canvas?
        // For the early prototype, we'll just take the upper right quadrant
        // from the camera.
        let stats = PixelStats.empty ()

        // n.b. not thread safe
        let traceOnce () =
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
                Ray.make' (Ray.origin camera.View) (Point.difference { ComeFrom = Ray.origin camera.View ; EndUpAt = endPoint })
                |> Option.get

            let result = traceRay 150 scene ray Colour.White
            PixelStats.add result stats

        for _ in 1..5 do
            traceOnce ()

        let oldMean = PixelStats.mean stats

        for _ in 1..5 do
            traceOnce ()

        let newMean = PixelStats.mean stats
        let difference = Pixel.difference newMean oldMean

        if difference < 2 then
           // The mean didn't really change when we added another five samples; assume it's not going to change
           // with more.
           newMean
        else

            for _ in 1..camera.SamplesPerPixel - 10 do
                traceOnce ()

            PixelStats.mean stats

    let render
        (progressIncrement : float<progress> -> unit)
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
                            let ret = renderPixel s rand camera maxWidthCoord maxHeightCoord row col
                            progressIncrement 1.0<progress>
                            return ret
                        }
                    )
                )
        }