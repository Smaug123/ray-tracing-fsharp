namespace RayTracing

open System

type Scene =
    private
        {
            UnboundedObjects : Hittable array
            BoundingBoxes : BoundingBoxTree voption
        }

[<RequireQualifiedAccess>]
module Scene =

    let make (objects : Hittable array) =
        let bounded, unbounded =
            objects
            |> Array.map (fun h -> h, Hittable.boundingBox h)
            |> Array.partition (snd >> ValueOption.isSome)

        let bounded = bounded |> Array.map (fun (h, box) -> h, ValueOption.get box)
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

    let hitObject (s : Scene) (ray : Ray) : struct (Hittable * Point) voption =
        let mutable best = Unchecked.defaultof<_>
        let mutable bestLength = nan
        let mutable bestFloat = infinity

        match s.BoundingBoxes with
        | ValueNone -> ()
        | ValueSome boundingBoxes ->
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
            ValueNone
        else
            ValueSome (struct (best, Ray.walkAlong ray bestLength))

    let internal traceRay (maxCount : int) (scene : Scene) (ray : byref<LightRay>) : Pixel =
        let mutable bounces = 0
        let mutable result = Colour.Black
        let mutable isDone = false

        while bounces <= maxCount && not isDone do
            let thingsWeHit = hitObject scene ray.Ray

            match thingsWeHit with
            | ValueNone ->
                // Ray goes off into the distance and is never heard from again
                isDone <- true
            | ValueSome (object, strikePoint) ->
                let stopWithColour = object.Reflection (&ray, strikePoint)

                match stopWithColour with
                | ValueSome colour ->
                    isDone <- true
                    result <- colour
                | ValueNone -> bounces <- bounces + 1

        if not isDone then Colour.HotPink else result

    /// Trace a ray to this one pixel, updating the PixelStats with the result.
    /// n.b. not thread safe
    let private traceOnce
        (scene : Scene)
        (rand : FloatProducer)
        (camera : Camera)
        (maxWidthCoord : int)
        (maxHeightCoord : int)
        (row : int)
        (col : int)
        (stats : PixelStats)
        : unit
        =
        let struct (rand1, rand2) = rand.GetTwo ()

        let landingPoint =
            ((float col + rand1) * camera.ViewportWidth) / float maxWidthCoord

        let pointOnXAxis = Ray.walkAlong camera.ViewportXAxis landingPoint

        let walkDistance =
            ((float row + rand2) * camera.ViewportHeight) / float maxHeightCoord

        let endPoint =
            Ray.walkAlongRay pointOnXAxis camera.ViewportYAxis.Vector walkDistance

        let ray =
            Ray.make' (Ray.origin camera.View) (Point.differenceToThenFrom endPoint (Ray.origin camera.View))
            |> ValueOption.get

        let mutable initialRay =
            {
                Ray = ray
                Colour = Colour.White
            }

        // Here we've hardcoded that the eye is emitting white light through a medium with refractance 1.
        let result = traceRay camera.BounceDepth scene &initialRay

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

        for _ = 0 to firstTrial do
            traceOnce scene rand camera maxWidthCoord maxHeightCoord row col stats

        let oldMean = PixelStats.mean stats

        for _ = 1 to firstTrial do
            traceOnce scene rand camera maxWidthCoord maxHeightCoord row col stats

        let newMean = PixelStats.mean stats
        let difference = Pixel.difference newMean oldMean

        if difference = 0 then
            // The mean didn't really change when we added another five samples; assume it's not going to change
            // with more.
            newMean
        else

            for _ = 1 to (camera.SamplesPerPixel - 2 * firstTrial - 1) do
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
                Seq.init
                    rowsIter
                    (fun row ->
                        let row = maxHeightCoord - row - 1

                        async {
                            return
                                Array.init
                                    colsIter
                                    (fun col ->
                                        let col = col - maxWidthCoord

                                        let ret = renderPixel print s rand camera maxWidthCoord maxHeightCoord row col
                                        progressIncrement 1.0<progress>
                                        ret
                                    )
                        }
                    )
        }
