namespace RayTracing

open System

type Hittable =
    | Sphere of Sphere
    | InfinitePlane of InfinitePlane

[<RequireQualifiedAccess>]
module Hittable =
    let hits
        (ray : Ray)
        (incomingColour : Pixel)
        (h : Hittable)
        : (Point * (unit -> Ray option * Pixel)) option
        =
        match h with
        | Sphere s ->
            Sphere.firstIntersection s ray incomingColour
        | InfinitePlane plane ->
            InfinitePlane.intersection plane ray incomingColour

type Scene =
    {
        Objects : Hittable array
    }

[<RequireQualifiedAccess>]
module Scene =

    let hitObject
        (s : Scene)
        (ray : Ray)
        (colour : Pixel)
        : (Point * (unit -> Ray option * Pixel)) option
        =
        let mutable answer = Unchecked.defaultof<_>
        let mutable bestFloat = infinity
        for i in 0..s.Objects.Length - 1 do
            match Hittable.hits ray colour s.Objects.[i] with
            | None -> ()
            | Some (point, g) ->
                let a = Vector.normSquared (Point.difference { EndUpAt = point ; ComeFrom = Ray.origin ray })
                match Float.compare a bestFloat with
                | Less ->
                    bestFloat <- a
                    answer <- point, g
                | _ -> ()

        if Object.ReferenceEquals(answer, null) then None else Some answer

    let internal traceRay
        (maxCount : int)
        (scene : Scene)
        (ray : Ray)
        (colour : Pixel)
        : Pixel
        =
        let rec go (bounces : int) (ray : Ray) (colour : Pixel) : Pixel =
            if bounces > maxCount then Colour.HotPink else

            let thingsWeHit = hitObject scene ray colour
            match thingsWeHit with
            | None ->
                // Ray goes off into the distance and is never heard from again
                Colour.Black
            | Some (_, gen) ->
                let outgoingRay, colour = gen ()
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