namespace RayTracing

open System

type Scene =
    private
        {
            UnboundedObjects : Hittable array
            BoundingBoxes : BoundingBoxTree option
        }

[<RequireQualifiedAccess>]
module Scene =

    let make (objects : Hittable array) =
        let bounded, unbounded =
            objects
            |> Array.map (fun h -> h, Hittable.boundingBox h)
            |> Array.partition (snd >> Option.isSome)

        let bounded = bounded |> Array.map (fun (h, box) -> h, Option.get box)
        let unbounded = unbounded |> Array.map fst
        let tree = bounded |> BoundingBoxTree.make

        {
            UnboundedObjects = unbounded
            BoundingBoxes = tree
        }

    let rec bestCandidate
        (inverseDirections : struct (float * float * float))
        (ray : Ray)
        (bestFloat : float)
        (bestObject : Hittable)
        (bestLength : float)
        (box : BoundingBoxTree)
        : struct (float * Hittable * float)
        =
        match box with
        | BoundingBoxTree.Leaf (object, box) ->
            if BoundingBox.hits inverseDirections ray box then
                match Hittable.hits ray object with
                | ValueNone -> struct (bestFloat, bestObject, bestLength)
                | ValueSome point ->
                    let a = point * point

                    if a < bestFloat then
                        struct (a, object, point)
                    else
                        struct (bestFloat, bestObject, bestLength)
            else
                struct (bestFloat, bestObject, bestLength)
        | BoundingBoxTree.Branch (left, right, all) ->
            if BoundingBox.hits inverseDirections ray all then
                let struct (bestFloat, bestObject, bestLength) =
                    bestCandidate inverseDirections ray bestFloat bestObject bestLength left

                bestCandidate inverseDirections ray bestFloat bestObject bestLength right
            else
                struct (bestFloat, bestObject, bestLength)

    let hitObject (s : Scene) (ray : Ray) : (Hittable * Point) option =
        let mutable best = Unchecked.defaultof<_>
        let mutable bestLength = nan
        let mutable bestFloat = infinity

        match s.BoundingBoxes with
        | None -> ()
        | Some boundingBoxes ->
            let struct (f, o, l) =
                bestCandidate (BoundingBox.inverseDirections ray) ray bestFloat best bestLength boundingBoxes

            bestFloat <- f
            best <- o
            bestLength <- l

        for i in s.UnboundedObjects do
            match Hittable.hits ray i with
            | ValueNone -> ()
            | ValueSome point ->
                let a = point * point

                if Float.compare a bestFloat = Less then
                    bestFloat <- a
                    best <- i
                    bestLength <- point

        if Double.IsNaN bestLength then
            None
        else
            Some (best, Ray.walkAlong ray bestLength)

    let internal traceRay (maxCount : int) (scene : Scene) (ray : LightRay) : Pixel =
        let rec go (bounces : int) (ray : LightRay) : Pixel =
            if bounces > maxCount then
                if ray.Colour = Colour.Black then
                    Colour.Black
                else
                    Colour.HotPink
            else

            let thingsWeHit = hitObject scene ray.Ray

            match thingsWeHit with
            | None ->
                // Ray goes off into the distance and is never heard from again
                Colour.Black
            | Some (object, strikePoint) ->
                let outgoingRay = object.Reflection ray strikePoint

                match outgoingRay with
                | Absorbs colour -> colour
                | Continues outgoingRay -> go (bounces + 1) outgoingRay

        go 0 ray

    /// Trace a ray to this one pixel, updating the PixelStats with the result.
    /// n.b. not thread safe
    let private traceOnce
        (scene : Scene)
        (rand : FloatProducer)
        (camera : Camera)
        (maxWidthCoord : int)
        (maxHeightCoord : int)
        row
        col
        stats
        =
        let struct (rand1, rand2) = rand.GetTwo ()

        let landingPoint =
            ((float col + rand1) * camera.ViewportWidth) / float maxWidthCoord

        let pointOnXAxis = landingPoint |> Ray.walkAlong camera.ViewportXAxis
        let toWalkUp = Ray.parallelTo pointOnXAxis camera.ViewportYAxis

        let endPoint =
            ((float row + rand2) * camera.ViewportHeight) / float maxHeightCoord
            |> Ray.walkAlong toWalkUp

        let ray =
            Ray.make' (Ray.origin camera.View) (Point.differenceToThenFrom endPoint (Ray.origin camera.View))
            |> Option.get

        // Here we've hardcoded that the eye is emitting white light through a medium with refractance 1.
        let result =
            traceRay
                camera.BounceDepth
                scene
                {
                    Ray = ray
                    Colour = Colour.White
                }

        PixelStats.add result stats

    let renderPixel
        (_ : string -> unit)
        (scene : Scene)
        (rand : FloatProducer)
        (camera : Camera)
        maxWidthCoord
        maxHeightCoord
        row
        col
        =
        // Where does this pixel correspond to, on the imaginary canvas?
        // For the early prototype, we'll just take the upper right quadrant
        // from the camera.
        let stats = PixelStats.empty ()

        let firstTrial = min 5 (camera.SamplesPerPixel / 2)

        for _ in 0..firstTrial do
            traceOnce scene rand camera maxWidthCoord maxHeightCoord row col stats

        let oldMean = PixelStats.mean stats

        for _ in 1..firstTrial do
            traceOnce scene rand camera maxWidthCoord maxHeightCoord row col stats

        let newMean = PixelStats.mean stats
        let difference = Pixel.difference newMean oldMean

        if difference = 0 then
            // The mean didn't really change when we added another five samples; assume it's not going to change
            // with more.
            newMean
        else

            for _ in 1 .. (camera.SamplesPerPixel - 2 * firstTrial - 1) do
                traceOnce scene rand camera maxWidthCoord maxHeightCoord row col stats

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
                Array.init
                    rowsIter
                    (fun row ->
                        let row = maxHeightCoord - row - 1

                        Array.init
                            colsIter
                            (fun col ->
                                let col = col - maxWidthCoord

                                async {
                                    let ret = renderPixel print s rand camera maxWidthCoord maxHeightCoord row col
                                    progressIncrement 1.0<progress>
                                    return ret
                                }
                            )
                    )
        }
