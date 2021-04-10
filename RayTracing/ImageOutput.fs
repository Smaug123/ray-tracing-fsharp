namespace RayTracing

open System
open System.Collections.Generic
open System.Collections.Immutable
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

    let readPixelMap (incrementProgress : float<progress> -> unit) (progress : IFileInfo) : Async<IReadOnlyDictionary<_, _>> =
        let rec go (dict : Dictionary<_, _>) (reader : StreamReader) =
            async {
                let! line = reader.ReadLineAsync () |> Async.AwaitTask
                let rowCol =
                    match line.Split ',' with
                    | [| row ; col |] -> Some struct(row, col)
                    | _ ->
                        None
                match rowCol with
                | None -> return dict
                | Some (row, col) ->

                let r = reader.Read ()
                if r = -1 then
                    // end of stream
                    return dict
                else
                let g = reader.Read ()
                if g = -1 then return dict else
                let b = reader.Read ()
                if b = -1 then return dict else
                let newline = reader.Read ()
                if newline > 0 && newline <> 10 then // '\n'
                    failwithf "Malformed progress file, expected newline after three bytes, got character '%i'" newline

                incrementProgress 1.0<progress>
                dict.Add ((Int32.Parse row, Int32.Parse col), { Red = byte r ; Green = byte g ; Blue = byte b })

                if not reader.EndOfStream then
                    return! go dict reader
                else
                    return dict
            }
        async {
            use stream = progress.FileSystem.File.OpenRead progress.FullName
            use reader = new StreamReader(stream)
            let result = Dictionary<int * int, _> ()
            let! result = go result reader
            return result :> IReadOnlyDictionary<_,_>
        }

    let resume (incrementProgress : float<progress> -> unit) (soFar : IReadOnlyDictionary<int * int, Pixel>) (image : Image) (fs : IFileSystem) : IFileInfo * Async<unit> =
        let rec go (writer : StreamWriter) (rowNum : int) (rowEnum : IEnumerator<Pixel Async []>) =
            async {
                if not (rowEnum.MoveNext ()) then
                    return ()
                else
                let row = rowEnum.Current
                do!
                    row
                    |> Array.mapi (fun colNum pixel ->
                        async {
                            let! pixel =
                                match soFar.TryGetValue ((rowNum, colNum)) with
                                | false, _ -> pixel
                                | true, v -> async { return v }
                            lock writer (fun () ->
                                writer.WriteLine (sprintf "%i,%i" rowNum colNum)
                                writer.Write pixel.Red
                                writer.Write pixel.Green
                                writer.Write pixel.Blue
                                writer.Write '\n'
                            )
                            incrementProgress 1.0<progress>
                            return ()
                        }
                    )
#if DEBUG
                    |> Async.Sequential
#else
                    |> Async.Parallel
#endif
                    |> Async.Ignore
                return! go writer (rowNum + 1) rowEnum
            }

        let tempFile = fs.Path.GetTempFileName () |> fs.FileInfo.FromFileName
        tempFile,
        async {
            use outputStream = tempFile.OpenWrite ()
            use writer = new StreamWriter (outputStream)
            use enumerator = image.Rows.GetEnumerator ()
            return! go writer 0 enumerator
        }

    let toArray (incrementProgress : float<progress> -> unit) (pixels : IReadOnlyDictionary<int * int, Pixel>) : Async<Pixel [] []> =
        async {
            let maxRow, maxCol = pixels |> Seq.map (fun (KeyValue(k, _)) -> k) |> Seq.max
            let pixels =
#if DEBUG
                Array.init (maxRow + 1) (fun row ->
#else
                Array.Parallel.init (maxRow + 1) (fun row ->
#endif
                    Array.init (maxCol + 1) (fun col ->
                        incrementProgress 1.0<progress>
                        pixels.[row, col]
                    )
                )
            return pixels
        }

    let writePpm (incrementProgress : float<progress> -> unit) (pixels : Pixel [] []) (output : IFileInfo) : Async<unit> =
        let maxRow = pixels.Length
        let maxCol = pixels.[0].Length
        async {
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
                    incrementProgress 1.0<progress>

                let pixel = pixels.[row].[pixels.[row].Length - 1]
                writer.Write (PixelOutput.toPpm pixel)
                incrementProgress 1.0<progress>

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
        : IFileInfo * Async<unit>
        =
        resume progressIncrement ImmutableDictionary.Empty image fs
