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
                let writeRow (row : Pixel []) =
                    for pixel in 0..row.Length - 2 do
                        writer.Write (PixelOutput.toPpm row.[pixel])
                        writer.Write " "
                    writer.Write (PixelOutput.toPpm row.[row.Length - 1])
                    progressIncrement 1.0<progress>

                for row in 0..arr.Length - 2 do
                    writeRow arr.[row]
                    writer.Write "\n"

                writeRow arr.[arr.Length - 1]
                progressIncrement 1.0<progress>
        }
