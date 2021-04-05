namespace RayTracing

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
        let aspectRatio = 16.0 / 9.0
        let camera =
            Camera.makeBasic Num.float 1.0 aspectRatio (Point [| 0.0 ; 0.0 ; 0.0 |])
        {
            Objects =
                [|
                    Hittable.Sphere (Sphere.make Num.float SphereStyle.PureReflection (Point [| 0.0 ; 0.0 ; 1.0 |]) 0.5)
                    Hittable.Sphere (Sphere.make Num.float SphereStyle.PureReflection (Point [| 1.5 ; 1.0 ; 2.0 |]) 0.25)
                    // Light source: a large sphere which emits light from the top
                    Hittable.Sphere (Sphere.make Num.float SphereStyle.WhiteLightSourceAtTop (Point [| 0.0 ; 0.0 ; 0.0 |]) 100.)
                |]
        }
        |> Scene.render progressIncrement (aspectRatio * 200.0 |> int) 200 camera

    let get (s : SampleImages) : (float<progress> -> unit) -> float<progress> * Image Async =
        match s with
        | Gradient -> gradient
        | Spheres -> spheres
