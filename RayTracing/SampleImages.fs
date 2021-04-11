namespace RayTracing

open System

type SampleImages =
    | Gradient
    | Spheres

[<RequireQualifiedAccess>]
module SampleImages =

    let gradient (progressIncrement : float<progress> -> unit) : float<progress> * Image Async =
        let pixelAt height width =
            {
                Red = (byte width)
                Green = 255uy - (byte height)
                Blue = 63uy
            }

        256.0<progress>,
        async {
            return
                Array.init
                    256
                    (fun height ->
                        let output = Array.init 256 (pixelAt height)
                        progressIncrement 1.0<progress>
                        output
                    )
                |> Image
        }

    let spheres (progressIncrement : float<progress> -> unit) : float<progress> * Image Async =
        let random = Random ()
        let aspectRatio = 16.0 / 9.0
        let camera =
            Camera.makeBasic 4.0 aspectRatio (Point [| 0.0 ; 0.0 ; 0.0 |])
        let pixels = 200
        {
            Objects =
                [|
                    Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (1.0, { Red = 255uy ; Green = 255uy ; Blue = 0uy }, random)) (Point [| 0.0 ; 0.0 ; 9.0 |]) 1.0)
                    Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0, { Red = 0uy ; Green = 255uy ; Blue = 255uy })) (Point [| 1.5 ; 0.5 ; 8.0 |]) 0.5)
                    Hittable.Sphere (Sphere.make (SphereStyle.LightSource Colour.Blue) (Point [| -1.5 ; 1.5 ; 8.0 |]) 0.5)

                    // Side mirror
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (1.0, { Colour.White with Green = 240uy })) (Point [| 0.1 ; 0.0 ; 16.0 |]) (Vector [| -2.0 ; 0.0 ; -1.0 |] |> Vector.unitise |> Option.get))

                    // Floor mirror
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.4, Colour.White)) (Point [| 0.0 ; -1.0 ; 0.0 |]) (Vector [| 0.0 ; 1.0 ; 0.0 |] |> Vector.unitise |> Option.get))

                    // Back plane
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.6, Colour.White)) (Point [| 0.0 ; 0.0 ; 16.0 |]) (Vector [| 0.0 ; 0.0 ; -1.0 |] |> Vector.unitise |> Option.get))

                    // Light pad behind us
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.LightSource Colour.White) (Point [| 0.0 ; 1.0 ; -1.0 |]) (Vector [| 0.0 ; -1.0 ; 1.0 |] |> Vector.unitise |> Option.get))
                |]
        }
        |> Scene.render progressIncrement (aspectRatio * (float pixels) |> int) pixels camera

    let get (s : SampleImages) : (float<progress> -> unit) -> float<progress> * Image Async =
        match s with
        | Gradient -> gradient
        | Spheres -> spheres

