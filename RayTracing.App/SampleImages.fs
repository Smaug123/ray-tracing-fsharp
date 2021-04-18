namespace RayTracing

open System

type SampleImages =
    | Gradient
    | Spheres
    | RandomSpheres
    | ShinyFloor
    | FuzzyFloor
    | InsideSphere
    | TotalRefraction
    | GlassSphere
    | MovedCamera

    static member Parse (s : string) =
        match s with
        | "spheres" -> SampleImages.Spheres
        | "gradient" -> SampleImages.Gradient
        | "shiny-floor" -> SampleImages.ShinyFloor
        | "fuzzy-floor" -> SampleImages.FuzzyFloor
        | "inside-sphere" -> SampleImages.InsideSphere
        | "total-refraction" -> SampleImages.TotalRefraction
        | "moved-camera" -> SampleImages.MovedCamera
        | "glass" -> SampleImages.GlassSphere
        | "random-spheres" -> SampleImages.RandomSpheres
        | s -> failwithf "Unrecognised arg: %s" s

[<RequireQualifiedAccess>]
module SampleImages =

    let systemRandom = Random ()

    let gradient (progressIncrement : float<progress> -> unit) (_ : string -> unit) : float<progress> * Image =
        let pixelAt height width =
            {
                Red = (byte width)
                Green = 255uy - (byte height)
                Blue = 63uy
            }

        let image =
            Array.init
                256
                (fun height ->
                    let output = Array.init 256 (fun i -> async { return pixelAt height i })
                    progressIncrement 1.0<progress>
                    output
                )
            |> Image.make 256 256

        256.0<progress>, image

    let shinyPlane (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let aspectRatio = 16.0 / 9.0
        let origin = Point.make 0.0 0.0 0.0
        let camera =
            Camera.makeBasic 50 2.0 aspectRatio origin (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 400
        [|
            Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Red = 0uy ; Green = 255uy ; Blue = 255uy }) (Point.make 1.5 0.5 8.0) 0.5)
            Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.5<albedo>, Colour.White)) (Point.make 0.0 -1.0 0.0) (Vector.make 0.0 1.0 0.0 |> Vector.unitise |> Option.get)) // Floor rug
        |]
        |> Scene.make (Random ())
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let fuzzyPlane (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let random = Random () |> FloatProducer
        let aspectRatio = 16.0 / 9.0
        let origin = Point.make 0.0 0.0 0.0
        let camera =
            Camera.makeBasic 50 2.0 aspectRatio origin (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 400
        [|
            Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Red = 0uy ; Green = 255uy ; Blue = 255uy }) (Point.make 1.5 0.5 8.0) 0.5)
            Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.FuzzedReflection (1.0<albedo>, Colour.White, 0.75<fuzz>, random)) (Point.make 0.0 -1.0 0.0) (Vector.make 0.0 1.0 0.0 |> Vector.unitise |> Option.get)) // Floor rug
        |]
        |> Scene.make systemRandom
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let spheres (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let random1 = Random () |> FloatProducer
        let random2 = Random () |> FloatProducer
        let random3 = Random () |> FloatProducer
        let aspectRatio = 16.0 / 9.0
        let origin = Point.make 0.0 0.0 0.0
        let camera =
            Camera.makeBasic 50 7.0 aspectRatio origin (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 200
        [|
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (0.95<albedo>, { Red = 255uy ; Green = 255uy ; Blue = 0uy }, random1)) (Point.make 0.0 0.0 9.0) 1.0)
            Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 0uy ; Green = 255uy ; Blue = 255uy })) (Point.make 1.5 0.5 8.0) 0.5)
            Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Colour.White with Red = 200uy ; Green = 220uy } ) (Point.make -1.5 1.0 8.0) 0.5)
            Hittable.Sphere (Sphere.make (SphereStyle.FuzzedReflection (1.0<albedo>, { Red = 255uy ; Green = 100uy ; Blue = 0uy }, 0.2<fuzz>, random2) ) (Point.make -0.4 1.5 10.0) 0.25)

            // Left side mirror
            Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.8<albedo>, Colour.White)) (Point.make 0.0 0.0 12.0) (Vector.make 1.0 0.0 -1.0 |> Vector.unitise |> Option.get))

            // Floor rug
            Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.FuzzedReflection (0.85<albedo>, { Red = 255uy ; Green = 100uy ; Blue = 100uy }, 0.8<fuzz>, random3)) (Point.make 0.0 -1.0 0.0) (Vector.make 0.0 1.0 0.0 |> Vector.unitise |> Option.get))

            // Right side mirror
            Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.PureReflection (0.95<albedo>, Colour.White)) (Point.make 0.0 0.0 12.0) (Vector.make -1.0 0.0 -1.0 |> Vector.unitise |> Option.get))

            // Light pad behind us
            Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.LightSource { Red = 15uy ; Green = 15uy ; Blue = 15uy }) (Point.make 0.0 1.0 -1.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get))
        |]
        |> Scene.make systemRandom
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let insideSphere (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let random1 = Random () |> FloatProducer
        let random2 = Random () |> FloatProducer
        let random3 = Random () |> FloatProducer
        let random4 = Random () |> FloatProducer
        let aspectRatio = 16.0 / 9.0
        let origin = Point.make 0.0 0.0 0.0
        let camera =
            Camera.makeBasic 50 7.0 aspectRatio origin (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 1200
        [|
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (0.95<albedo>, { Red = 255uy ; Green = 255uy ; Blue = 0uy }, random1)) (Point.make 0.0 0.0 9.0) 1.0)
            Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 0uy ; Green = 255uy ; Blue = 255uy })) (Point.make 1.5 0.5 8.0) 0.5)
            Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 255uy ; Green = 20uy ; Blue = 20uy })) (Point.make -1.8 0.8 8.0) 0.5)
            Hittable.Sphere (Sphere.make (SphereStyle.LightSource Colour.White) (Point.make -10.0 8.0 0.0) 9.0)
            Hittable.Sphere (Sphere.make (SphereStyle.FuzzedReflection (1.0<albedo>, { Red = 255uy ; Green = 100uy ; Blue = 0uy }, 0.2<fuzz>, random2) ) (Point.make 1.4 1.5 10.0) 0.25)
            Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (0.9<albedo>, { Red = 255uy ; Green = 255uy ; Blue = 255uy })) (Point.make 0.0 10.0 20.0) 8.0)

            Hittable.Sphere (Sphere.make (SphereStyle.FuzzedReflection (0.6<albedo>, { Red = 200uy ; Green = 50uy ; Blue = 255uy }, 0.4<fuzz>, random3)) (Point.make 0.0 -76.0 9.0) 75.0 )
            Hittable.Sphere (Sphere.make (SphereStyle.FuzzedReflection (0.4<albedo>, { Red = 200uy ; Green = 200uy ; Blue = 200uy }, 0.0<fuzz>, random4)) (Point.make 0.0 0.0 20.0) 100.0)
            // Light pad behind us
            Hittable.InfinitePlane (InfinitePlane.make (InfinitePlaneStyle.LightSource { Red = 80uy ; Green = 80uy ; Blue = 150uy }) (Point.make 0.0 0.0 -5.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get))
        |]
        |> Scene.make systemRandom
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let totalRefraction (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let random1 = Random () |> FloatProducer
        let random2 = Random () |> FloatProducer
        let random3 = Random () |> FloatProducer
        let aspectRatio = 16.0 / 9.0
        let origin = Point.make 0.0 0.0 0.0
        let camera =
            Camera.makeBasic 50 1.0 aspectRatio origin (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 300
        [|
            // Floor
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (0.5<albedo>, { Red = 204uy ; Green = 204uy ; Blue = 0uy }, random1)) (Point.make 0.0 -100.5 1.0) 100.0)

            // Right sphere
            Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 204uy ; Green = 153uy ; Blue = 51uy })) (Point.make 1.0 0.0 1.0) 0.5)
            // Middle sphere
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (1.0<albedo>, { Red = 25uy ; Green = 50uy ; Blue = 120uy }, random2)) (Point.make 0.0 0.0 1.0) 0.5)
            // Left sphere
            Hittable.Sphere (Sphere.make (SphereStyle.Dielectric (1.0<albedo>, Colour.White, 1.5<ior>, 1.0<prob>, random3)) (Point.make -1.0 0.0 1.0) 0.5)

            // Light around us
            Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Red = 80uy ; Green = 80uy ; Blue = 150uy }) (Point.make 0.0 0.0 0.0) 200.0)
        |]
        |> Scene.make systemRandom
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let glassSphere (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let random1 = Random () |> FloatProducer
        let random2 = Random () |> FloatProducer
        let random3 = Random () |> FloatProducer
        let aspectRatio = 16.0 / 9.0
        let origin = Point.make 0.0 0.0 0.0
        let camera =
            Camera.makeBasic 50 1.0 aspectRatio origin (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 200
        [|
            // Floor
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (0.5<albedo>, { Red = 204uy ; Green = 204uy ; Blue = 0uy }, random1)) (Point.make 0.0 -100.5 1.0) 100.0)

            // Right sphere
            Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 204uy ; Green = 153uy ; Blue = 51uy })) (Point.make 1.0 0.0 1.0) 0.5)
            // Middle sphere
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (1.0<albedo>, { Red = 25uy ; Green = 50uy ; Blue = 120uy }, random2)) (Point.make 0.0 0.0 1.0) 0.5)
            // Left sphere
            Hittable.Sphere (Sphere.make (SphereStyle.Glass (0.9<albedo>, Colour.White, 1.5<ior>, random3)) (Point.make -1.0 0.0 1.0) 0.5)

            // Light around us
            Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Red = 80uy ; Green = 80uy ; Blue = 150uy }) (Point.make 0.0 0.0 0.0) 200.0)
        |]
        |> Scene.make systemRandom
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let movedCamera (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let random1 = Random () |> FloatProducer
        let random2 = Random () |> FloatProducer
        let random3 = Random () |> FloatProducer
        let random4 = Random () |> FloatProducer
        let aspectRatio = 16.0 / 9.0
        let origin = Point.make -2.0 2.0 -1.0
        let camera =
            Camera.makeBasic 50 10.0 aspectRatio origin (Point.differenceToThenFrom (Point.make -1.0 0.0 1.0) origin |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 300
        [|
            // Floor
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (0.5<albedo>, { Red = 204uy ; Green = 204uy ; Blue = 0uy }, random1)) (Point.make 0.0 -100.5 1.0) 100.0)

            // Right sphere
            Hittable.Sphere (Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 204uy ; Green = 153uy ; Blue = 51uy })) (Point.make 1.0 0.0 1.0) 0.5)
            // Middle sphere
            Hittable.Sphere (Sphere.make (SphereStyle.LambertReflection (1.0<albedo>, { Red = 25uy ; Green = 50uy ; Blue = 120uy }, random2)) (Point.make 0.0 0.0 1.0) 0.5)
            // Left sphere
            Hittable.Sphere (Sphere.make (SphereStyle.Glass (1.0<albedo>, Colour.White, 1.5<ior>, random3)) (Point.make -1.0 0.0 1.0) 0.5)
            Hittable.Sphere (Sphere.make (SphereStyle.Glass (1.0<albedo>, Colour.White, 1.0<ior> / 1.5, random4)) (Point.make -1.0 0.0 1.0) -0.45)

            // Light around us
            Hittable.Sphere (Sphere.make (SphereStyle.LightSource { Red = 130uy ; Green = 130uy ; Blue = 200uy }) (Point.make 0.0 0.0 0.0) 200.0)
        |]
        |> Scene.make systemRandom
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let randomSpheres (progressIncrement : float<progress> -> unit) (log : string -> unit) : float<progress> * Image =
        let aspectRatio = 3.0 / 2.0
        let origin = Point.make 13.0 2.0 -3.0
        let camera =
            Camera.makeBasic 500 10.0 aspectRatio origin (Point.differenceToThenFrom (Point.make 0.0 0.0 0.0) origin |> Vector.unitise |> Option.get) (Vector.make 0.0 1.0 0.0)
        let pixels = 100

        let spheres =
            [|
                for a in -11..10 do
                    for b in -11..10 do
                        let rand = Random ()
                        let floatProducer = FloatProducer rand
                        let materialChoice = floatProducer.Get ()
                        let centre = Point.make (float a + 0.9 * floatProducer.Get ()) 0.2 (float b + 0.9 * floatProducer.Get ())

                        if Vector.normSquared (Point.differenceToThenFrom centre (Point.make 4.0 0.2 0.0)) > 0.9 * 0.9 then
                            if Float.compare materialChoice 0.8 = Less then
                                // diffuse
                                let albedo = floatProducer.Get () * floatProducer.Get () * 1.0<albedo>
                                yield Sphere.make (SphereStyle.LambertReflection (albedo, Colour.random rand, floatProducer)) centre 0.2
                            elif Float.compare materialChoice 0.95 = Less then
                                // metal
                                let albedo = floatProducer.Get () / 2.0 * 1.0<albedo> + 0.5<albedo>
                                let fuzz = floatProducer.Get () / 2.0 * 1.0<fuzz>
                                yield Sphere.make (SphereStyle.FuzzedReflection (albedo, Colour.random rand, fuzz, floatProducer)) centre 0.2
                            else
                                // glass
                                yield Sphere.make (SphereStyle.Glass (1.0<albedo>, Colour.White, 1.5<ior>, floatProducer)) centre 0.2


                let rand = Random ()
                let floatProducer = FloatProducer rand
                yield Sphere.make (SphereStyle.Glass (1.0<albedo>, Colour.White, 1.5<ior>, floatProducer)) (Point.make 0.0 1.0 0.0) 1.0

                let rand = Random ()
                let floatProducer = FloatProducer rand
                yield Sphere.make (SphereStyle.LambertReflection (1.0<albedo>, { Red = 80uy ; Green = 40uy ; Blue = 20uy }, floatProducer)) (Point.make -4.0 1.0 0.0) 1.0

                yield Sphere.make (SphereStyle.PureReflection (1.0<albedo>, { Red = 180uy ; Green = 150uy ; Blue = 128uy })) (Point.make 4.0 1.0 0.0) 1.0

                // Ceiling
                yield Sphere.make (SphereStyle.LightSource { Colour.White with Red = 200uy ; Green = 200uy }) (Point.make 0.0 0.0 0.0) 2000.0

                // Floor
                let rand = Random ()
                let floatProducer = FloatProducer rand
                yield Sphere.make (SphereStyle.LambertReflection (0.5<albedo>, Colour.White, floatProducer)) (Point.make 0.0 -1000.0 0.0) 1000.0
            |]

        spheres
        |> Array.map Hittable.Sphere
        |> Scene.make systemRandom
        |> Scene.render progressIncrement log (aspectRatio * (float pixels) |> int) pixels camera

    let get (s : SampleImages) : (float<progress> -> unit) -> (string -> unit) -> float<progress> * Image =
        match s with
        | Gradient -> gradient
        | Spheres -> spheres
        | ShinyFloor -> shinyPlane
        | FuzzyFloor -> fuzzyPlane
        | InsideSphere -> insideSphere
        | TotalRefraction -> totalRefraction
        | GlassSphere -> glassSphere
        | MovedCamera -> movedCamera
        | RandomSpheres -> randomSpheres
