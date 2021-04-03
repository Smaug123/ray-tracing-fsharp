namespace RayTracing

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
            return Array.init 256 (fun height ->
                let output = Array.init 256 (pixelAt height)
                progressIncrement 1.0<progress>
                output
            )
            |> Image
        }