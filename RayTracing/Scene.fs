namespace RayTracing

open System

type Hittable<'a> =
    | Sphere of Sphere<'a>

[<RequireQualifiedAccess>]
module Hittable =
    let hits<'a>
        (num : Num<'a>)
        (ray : Ray<'a>)
        (incomingColour : Pixel)
        (h : Hittable<'a>)
        : (Point<'a> * Ray<'a> * Pixel) option
        =
        match h with
        | Sphere s ->
            Sphere.intersections num s ray incomingColour
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
        : (Point<'a> * Ray<'a> * Pixel) array
        =
        s.Objects
        |> Array.choose (Hittable.hits num ray colour)

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
                    Array.init colsIter (fun col ->
                        let col = col - maxWidthCoord
                        // Where does this pixel correspond to, on the imaginary canvas?
                        // For the early prototype, we'll just take the upper right quadrant
                        // from the camera.
                        let ret =
                            Array.init camera.SamplesPerPixel (fun sample ->
                                // TODO make this be deterministic
                                let pointOnXAxis =
                                    num.DivideInteger (num.Add (num.TimesInteger col camera.ViewportWidth) (num.RandomBetween01 rand)) maxWidthCoord
                                    |> Ray.walkAlong num camera.ViewportXAxis
                                let toWalkUp = Ray.parallelTo pointOnXAxis camera.ViewportYAxis
                                let endPoint =
                                    num.DivideInteger (num.Add (num.TimesInteger row camera.ViewportHeight) (num.RandomBetween01 rand)) maxHeightCoord
                                    |> Ray.walkAlong num toWalkUp
                                let ray = Ray.between num camera.View.Origin endPoint

                                let thingsWeHit = hitObject num s ray Pixel.White
                                match thingsWeHit with
                                | [||] -> { Red = 0uy ; Green = 0uy ; Blue = 0uy }
                                | arr ->
                                    let object = arr.[0]
                                    { Red = 255uy ; Green = 255uy ; Blue = 255uy }
                            )
                            |> Pixel.average
                        progressIncrement 1.0<progress>
                        ret
                    )
                )
                |> Array.rev
                |> Image
        }