namespace RayTracing

[<RequireQualifiedAccess>]
module SampleImages =

    let gradient (progressIncrement : float<progress> -> unit) : float<progress> * Image Async =
        let pixelAt i j =
            {
                Red = (byte i)
                Green = (byte j)
                Blue = 64uy
            }

        256.0<progress>,
        async {
            return Array.init 256 (fun i ->
                let output = Array.init 256 (pixelAt i)
                progressIncrement 1.0<progress>
                output
            )
            |> Image
        }