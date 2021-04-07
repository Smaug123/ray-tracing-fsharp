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
        : (Point * Ray option * Pixel) option
        =
        match h with
        | Sphere s ->
            Sphere.intersections s ray incomingColour
            |> Array.tryHead
        | InfinitePlane plane ->
            InfinitePlane.intersections plane ray incomingColour
            |> Array.tryHead

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
        : (Point * Ray option * Pixel) array
        =
        s.Objects
        |> Array.choose (Hittable.hits ray colour)
        |> Float.sortInPlaceBy (fun (a, _, _) -> Vector.normSquared (Point.difference a ray.Origin))

    let internal traceRay
        (maxCount : int)
        (scene : Scene)
        (ray : Ray)
        (colour : Pixel)
        : Pixel
        =
        let rec go (bounces : int) (ray : Ray) (colour : Pixel) : Pixel =
            if bounces > maxCount then Colour.Black else

            let thingsWeHit = hitObject scene ray colour
            match thingsWeHit with
            | [||] ->
                // Ray goes off into the distance and is never heard from again
                Colour.Black
            | arr ->
                let _strikePoint, outgoingRay, colour = arr.[0]
                match outgoingRay with
                | None ->
                    colour
                | Some outgoingRay ->
                    go (bounces + 1) { outgoingRay with Vector = Vector.unitise outgoingRay.Vector |> Option.get } colour

        go 0 ray colour

    let render
        (progressIncrement : float<progress> -> unit)
        (maxWidthCoord : int)
        (maxHeightCoord : int)
        (camera : Camera)
        (s : Scene)
        : float<progress> * Image Async
        =
        let rand = Random ()
        // For each pixel in the output, send a ray from the camera
        // in the direction of that pixel.
        let rowsIter = 2 * maxHeightCoord + 1
        let colsIter = 2 * maxWidthCoord + 1
        1.0<progress> * float (rowsIter * colsIter), async {
            return
                Array.init rowsIter (fun row ->
                    let row = row - maxHeightCoord
                    Array.Parallel.init colsIter (fun col ->
                        let col = col - maxWidthCoord
                        // Where does this pixel correspond to, on the imaginary canvas?
                        // For the early prototype, we'll just take the upper right quadrant
                        // from the camera.
                        let ret =
                            Array.init camera.SamplesPerPixel (fun _ ->
                                // TODO make this be deterministic
                                let pointOnXAxis =
                                    ((float col * camera.ViewportWidth) + (Float.random rand)) / float maxWidthCoord
                                    |> Ray.walkAlong camera.ViewportXAxis
                                let toWalkUp = Ray.parallelTo pointOnXAxis camera.ViewportYAxis
                                let endPoint =
                                    ((float row * camera.ViewportHeight) + (Float.random rand)) / float maxHeightCoord
                                    |> Ray.walkAlong toWalkUp
                                let ray = Ray.between camera.View.Origin endPoint

                                let result = traceRay 50 s ray Colour.White
                                result
                            )
                            |> Pixel.average
                        progressIncrement 1.0<progress>
                        ret
                    )
                )
                |> Array.rev
                |> Image
        }