namespace RayTracing

open System

type SampleImages =
    | Gradient
    | Spheres
    | ShinyFloor
    | FuzzyFloor
    static member Parse (s : string) =
        match s with
        | "spheres" -> SampleImages.Spheres
        | "gradient" -> SampleImages.Gradient
        | "shiny-floor" -> SampleImages.ShinyFloor
        | "fuzzy-floor" -> SampleImages.FuzzyFloor
        | s -> failwithf "Unrecognised arg: %s" s

[<RequireQualifiedAccess>]
module SampleImages =

    let gradient (progressIncrement : float<progress> -> unit) : float<progress> * Image =
        let pixelAt height width =
            {
                Red = (byte width)
                Green = 255uy - (byte height)
                Blue = 63uy
            }

        256.0<progress>,
        {
            RowCount = 256
            ColCount = 256
            Rows =
                Array.init
                    256
                    (fun height ->
                        let output = Array.init 256 (fun i -> async { return pixelAt height i })
                        progressIncrement 1.0<progress>
                        output
                    )
        }

    let shinyPlane (progressIncrement : float<progress> -> unit) : float<progress> * Image =
        let aspectRatio = 16.0 / 9.0
        let origin = Point [| 0.0 ; 0.0 ; 0.0 |]
        let camera =
            Camera.makeBasic 2.0 aspectRatio origin (Vector [| 0.0 ; 0.0 ; 1.0 |] |> Vector.unitise |> Option.get)
        let pixels = 400
        {
            Objects =
                [|
                    Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Red = 0uy ; Green = 255uy ; Blue = 255uy }) (Point [| 1.5 ; 0.5 ; 8.0 |]) 0.5)
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.5<albedo>, Colour.White)) (Point [| 0.0 ; -1.0 ; 0.0 |]) (Vector [| 0.0 ; 1.0 ; 0.0 |] |> Vector.unitise |> Option.get)) // Floor rug
                |]
        }
        |> Scene.render progressIncrement (aspectRatio * (float pixels) |> int) pixels camera

    let fuzzyPlane (progressIncrement : float<progress> -> unit) : float<progress> * Image =
        let random = Random ()
        let aspectRatio = 16.0 / 9.0
        let origin = Point [| 0.0 ; 0.0 ; 0.0 |]
        let camera =
            Camera.makeBasic 2.0 aspectRatio origin (Vector [| 0.0 ; 0.0 ; 1.0 |] |> Vector.unitise |> Option.get)
        let pixels = 400
        {
            Objects =
                [|
                    Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Red = 0uy ; Green = 255uy ; Blue = 255uy }) (Point [| 1.5 ; 0.5 ; 8.0 |]) 0.5)
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.FuzzyReflection (1.0<albedo>, Colour.White, 0.75<fuzz>, random)) (Point [| 0.0 ; -1.0 ; 0.0 |]) (Vector [| 0.0 ; 1.0 ; 0.0 |] |> Vector.unitise |> Option.get)) // Floor rug
                |]
        }
        |> Scene.render progressIncrement (aspectRatio * (float pixels) |> int) pixels camera

    let spheres (progressIncrement : float<progress> -> unit) : float<progress> * Image =
        let random = Random ()
        let aspectRatio = 16.0 / 9.0
        let origin = Point [| 0.0 ; 0.0 ; 0.0 |]
        let camera =
            Camera.makeBasic 7.0 aspectRatio origin (Vector [| 0.0 ; 0.0 ; 1.0 |] |> Vector.unitise |> Option.get)
        let pixels = 200
        {
            Objects =
                [|
                    Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (0.95<albedo>, { Red = 255uy ; Green = 255uy ; Blue = 0uy }, random)) (Point [| 0.0 ; 0.0 ; 9.0 |]) 1.0)
                    Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 0uy ; Green = 255uy ; Blue = 255uy })) (Point [| 1.5 ; 0.5 ; 8.0 |]) 0.5)
                    Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Colour.White with Red = 200uy ; Green = 220uy } ) (Point [| -1.5 ; 1.0 ; 8.0 |]) 0.5)
                    Hittable.Sphere (Sphere.make (SphereStyle.FuzzedReflection (0.2<fuzz>, random, 1.0<albedo>, { Red = 255uy ; Green = 100uy ; Blue = 0uy }) ) (Point [| -0.4 ; 1.5 ; 10.0 |]) 0.25)

                    // Left side mirror
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.8<albedo>, Colour.White)) (Point [| 0.0 ; 0.0 ; 12.0 |]) (Vector [| 1.0 ; 0.0 ; -1.0 |] |> Vector.unitise |> Option.get))

                    // Floor rug
                    // Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.FuzzyReflection (0.6<albedo>, Colour.White, 0.8<fuzz>, random)) (Point [| 0.0 ; 5.0 ; 0.0 |]) (Vector [| 0.0 ; -1.0 ; 0.0 |] |> Vector.unitise |> Option.get)) // Floor rug
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.FuzzyReflection (0.85<albedo>, { Red = 255uy ; Green = 100uy ; Blue = 100uy }, 0.8<fuzz>, random)) (Point [| 0.0 ; -1.0 ; 0.0 |]) (Vector [| 0.0 ; 1.0 ; 0.0 |] |> Vector.unitise |> Option.get))

                    // Right side mirror
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.95<albedo>, Colour.White)) (Point [| 0.0 ; 0.0 ; 12.0 |]) (Vector [| -1.0 ; 0.0 ; -1.0 |] |> Vector.unitise |> Option.get))

                    // Light pad behind us
                    Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.LightSource { Red = 15uy ; Green = 15uy ; Blue = 15uy }) (Point [| 0.0 ; 1.0 ; -1.0 |]) (Vector [| 0.0 ; 0.0 ; 1.0 |] |> Vector.unitise |> Option.get))
                |]
        }
        |> Scene.render progressIncrement (aspectRatio * (float pixels) |> int) pixels camera

    let get (s : SampleImages) : (float<progress> -> unit) -> float<progress> * Image =
        match s with
        | Gradient -> gradient
        | Spheres -> spheres
        | ShinyFloor -> shinyPlane
        | FuzzyFloor -> fuzzyPlane
