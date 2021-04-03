namespace RayTracing

open System.IO
open System.IO.Abstractions

[<RequireQualifiedAccess>]
module PixelOutput =
    let toPpm (pixel : Pixel) : string =
        sprintf "%i %i %i" pixel.Red pixel.Green pixel.Blue


[<RequireQualifiedAccess>]
module ImageOutput =

    let toPpm
        (progressIncrement : float<progress> -> unit)
        (image : Image)
        (file : IFileInfo)
        : float<progress> * Async<unit>
        =
        (float (Image.rowCount image)) * 1.0<progress>,
        async {
            use outputStream = file.OpenWrite ()
            use writer = new StreamWriter (outputStream)
            writer.Write "P3\n"
            writer.Write (sprintf "%i %i\n" (Image.colCount image) (Image.rowCount image))
            writer.Write "255\n"

            match image with
            | Image arr ->
                for row in arr do
                    for pixel in row do
                        writer.Write (PixelOutput.toPpm pixel)
                        writer.Write " "
                    writer.Write "\n"
                    progressIncrement 1.0<progress>
        }
