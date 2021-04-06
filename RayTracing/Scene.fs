namespace RayTracing

open System

type Hittable<'a> =
    | Sphere of Sphere<'a>
    | InfinitePlane of InfinitePlane<'a>

[<RequireQualifiedAccess>]
module Hittable =
    let hits<'a>
        (num : Num<'a>)
        (ray : Ray<'a>)
        (incomingColour : Pixel)
        (h : Hittable<'a>)
        : (Point<'a> * Ray<'a> option * Pixel) option
        =
        match h with
        | Sphere s ->
            Sphere.intersections num s ray incomingColour
            |> Array.tryHead
        | InfinitePlane plane ->
            InfinitePlane.intersections num plane ray incomingColour
            |> Array.tryHead

type Scene<'a> =
    {
        Objects : Hittable<'a> array
    }

[<RequireQualifiedAccess>]
module Scene =

    let hitObject<'a>
        (num : Num<'a>)
        (s : Scene<'a>)
        (ray : Ray<'a>)
        (colour : Pixel)
        : (Point<'a> * Ray<'a> option * Pixel) array
        =
        s.Objects
        |> Array.choose (Hittable.hits num ray colour)
        |> Num.sortInPlaceBy num (fun (a, _, _) -> Vector.normSquared num (Point.difference num a ray.Origin))

    let internal traceRay<'a>
        (maxCount : int)
        (num : Num<'a>)
        (scene : Scene<'a>)
        (ray : Ray<'a>)
        (colour : Pixel)
        : Pixel
        =
        let rec go (bounces : int) (ray : Ray<'a>) (colour : Pixel) : Pixel =
            if bounces > maxCount then Colour.Black else

            let thingsWeHit = hitObject num scene ray colour
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
                    go (bounces + 1) { outgoingRay with Vector = Vector.unitise num outgoingRay.Vector |> Option.get } colour

        go 0 ray colour

    let render<'a>
        (progressIncrement : float<progress> -> unit)
        (maxWidthCoord : int)
        (maxHeightCoord : int)
        (camera : Camera<'a>)
        (s : Scene<'a>)
        : float<progress> * Image Async
        =
        let rand = Random ()
        let num = camera.Num
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
                                    num.DivideInteger (num.Add (num.TimesInteger col camera.ViewportWidth) (num.RandomBetween01 rand)) maxWidthCoord
                                    |> Ray.walkAlong num camera.ViewportXAxis
                                let toWalkUp = Ray.parallelTo pointOnXAxis camera.ViewportYAxis
                                let endPoint =
                                    num.DivideInteger (num.Add (num.TimesInteger row camera.ViewportHeight) (num.RandomBetween01 rand)) maxHeightCoord
                                    |> Ray.walkAlong num toWalkUp
                                let ray = Ray.between num camera.View.Origin endPoint

                                let result = traceRay 50 num s ray Colour.White
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