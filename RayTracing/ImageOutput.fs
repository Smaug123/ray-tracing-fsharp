namespace RayTracing

open System
open System.IO
open System.IO.Abstractions

[<RequireQualifiedAccess>]
module PixelOutput =
    let correct (b : byte) : byte =
        float b / 255.0
        |> sqrt
        |> (*) 255.0
        |> Math.Round
        |> int
        |> fun i -> if i = 256 then 255 else i
        |> byte

    let toPpm (pixel : Pixel) : string =
        // Gamma correct!
        let red = correct pixel.Red
        let green = correct pixel.Green
        let blue = correct pixel.Blue
        sprintf "%i %i %i" red green blue


[<RequireQualifiedAccess>]
module ImageOutput =

    let convert (progress : IFileInfo) (output : IFileInfo) : Async<unit> =
        async {
            let! pixels =
                progress.FileSystem.File.ReadAllLinesAsync progress.FullName
                |> Async.AwaitTask
            let outputMap =
                pixels
                |> Array.map (fun line ->
                    match line.Split ':' with
                    | [| coords ; pixel |] ->
                        let row, col =
                            match coords.Split ',' with
                            | [| row ; col |] -> row, col
                            | _ -> failwithf "Malformed progress file, expected single comma in coordinates section '%s' of line '%s'" coords line
                        let r, g, b =
                            match pixel.Split ',' with
                            | [| r ; g ; b |] -> r, g, b
                            | _ -> failwithf "Malformed progress file, expected single comma in pixels section '%s' of line '%s'" pixel line

                        ((Int32.Parse row, Int32.Parse col), { Red = Byte.Parse r ; Green = Byte.Parse g ; Blue = Byte.Parse b })
                    | _ ->
                        failwithf "Malformed progress file, expected single colon in line: '%s'" line
                )
                |> Map.ofArray
            let keys = outputMap |> Map.toSeq |> Seq.map fst |> Set.ofSeq
            let maxRow, maxCol = keys.MaximumElement
            let pixels =
                Array.init (maxRow + 1) (fun row ->
                    Array.init (maxCol + 1) (fun col ->
                        outputMap.[row, col]
                    )
                )

            use output = output.OpenWrite ()
            use writer = new StreamWriter (output)

            writer.Write "P3\n"
            writer.Write (sprintf "%i %i\n" maxCol maxRow)
            writer.Write "255\n"

            let writeRow (row : int) =
                for col in 0..pixels.[row].Length - 2 do
                    let pixel = pixels.[row].[col]
                    writer.Write (PixelOutput.toPpm pixel)
                    writer.Write " "

                let pixel = pixels.[row].[pixels.[row].Length - 1]
                writer.Write (PixelOutput.toPpm pixel)

            for row in 0..pixels.Length - 2 do
                writeRow row
                writer.Write "\n"
            writeRow (pixels.Length - 1)
        }

    /// Write out this image to a temporary file, flushing intermediate work as quickly as possible.
    /// Await the async to know when the entire image is complete.
    /// Then use `ImageOutput.convert` to convert this temporary file into an actual .ppm file.
    let toPpm
        (progressIncrement : float<progress> -> unit)
        (image : Image)
        (fs : IFileSystem)
        : float<progress> * IFileInfo * Async<unit>
        =
        let tempFile = fs.Path.GetTempFileName () |> fs.FileInfo.FromFileName
        (float (Image.rowCount image * Image.colCount image) + 1.0) * 1.0<progress>,
        tempFile,
        async {
            progressIncrement 1.0<progress>
            use outputStream = tempFile.OpenWrite ()
            use writer = new StreamWriter (outputStream)

            // Kick off everything!
            return!
                image.Rows
                |> Array.mapi (fun rowNum row ->
                    row
                    |> Array.mapi (fun colNum pixel ->
                        async {
                            let! pixel = pixel
                            lock writer (fun () ->
                                writer.WriteLine (sprintf "%i,%i:%i,%i,%i" rowNum colNum pixel.Red pixel.Green pixel.Blue)
                            )
                            progressIncrement 1.0<progress>
                            return pixel
                        }
                    )
                    |> Async.Parallel
                )
                |> Async.Parallel
                |> Async.Ignore
        }
