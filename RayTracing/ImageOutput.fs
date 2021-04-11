namespace RayTracing

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.IO.Abstractions
open System.Text

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

    let toPpm (gammaCorrect : bool) (pixel : Pixel) : string =
        if gammaCorrect then
            let red = correct pixel.Red
            let green = correct pixel.Green
            let blue = correct pixel.Blue
            sprintf "%i %i %i" red green blue
        else
            let red = pixel.Red
            let green = pixel.Green
            let blue = pixel.Blue
            sprintf "%i %i %i" red green blue


[<RequireQualifiedAccess>]
module ImageOutput =

    // Read in an integer. After execution, the stream has consumed an extra character
    // after that integer.
    let consumeAsciiInteger (s : Stream) : int voption =
        let mutable answer = 0
        let mutable keepGoing = true
        let mutable toRet = ValueNone
        while keepGoing do
            let i = s.ReadByte ()
            if i < 0 then keepGoing <- false else
            // '0' is 48
            // '9' is 57
            if 48 <= i && i <= 57 then
                answer <- (10 * answer + (i - 48))
            else
                toRet <- ValueSome answer
                keepGoing <- false
        toRet

    let readPixelMap (incrementProgress : float<progress> -> unit) (progress : IFileInfo) (numRows : int) (numCols : int) : Async<Pixel ValueOption [] []> =
        let rec go (dict : _ [] []) (reader : Stream) =
            let row = consumeAsciiInteger reader
            match row with
            | ValueNone -> dict
            | ValueSome row ->

            let col = consumeAsciiInteger reader
            match col with
            | ValueNone -> dict
            | ValueSome col ->

            let r = reader.ReadByte ()
            if r < 0 then dict else
            let g = reader.ReadByte ()
            if g = -1 then dict else
            let b = reader.ReadByte ()
            if b = -1 then dict else

            incrementProgress 1.0<progress>
            dict.[row].[col] <- ValueSome { Red = byte r ; Green = byte g ; Blue = byte b }

            go dict reader

        async {
            use stream = progress.FileSystem.File.OpenRead progress.FullName
            let result = Array.init numRows (fun _ -> Array.zeroCreate numCols)
            let result = go result stream
            return result
        }

    let resume (incrementProgress : float<progress> -> unit) (soFar : IReadOnlyDictionary<int * int, Pixel>) (image : Image) (fs : IFileSystem) : IFileInfo * Async<unit> =
        let rec go (writer : Stream) (rowNum : int) (rowEnum : IEnumerator<Pixel Async []>) =
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
                            let toWrite = ASCIIEncoding.Default.GetBytes (sprintf "%i,%i" rowNum colNum)
                            lock writer (fun () ->
                                writer.Write (toWrite, 0, toWrite.Length)
                                writer.WriteByte 10uy // '\n'
                                writer.WriteByte pixel.Red
                                writer.WriteByte pixel.Green
                                writer.WriteByte pixel.Blue
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
            use enumerator = image.Rows.GetEnumerator ()
            return! go outputStream 0 enumerator
        }

    let writePpm (gammaCorrect : bool) (incrementProgress : float<progress> -> unit) (pixels : Pixel [] []) (output : IFileInfo) : Async<unit> =
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
                    writer.Write (PixelOutput.toPpm gammaCorrect pixel)
                    writer.Write " "
                    incrementProgress 1.0<progress>

                let pixel = pixels.[row].[pixels.[row].Length - 1]
                writer.Write (PixelOutput.toPpm gammaCorrect pixel)
                incrementProgress 1.0<progress>

            for row in 0..pixels.Length - 2 do
                writeRow row
                writer.Write "\n"
            writeRow (pixels.Length - 1)
        }

    let assertComplete (image : Pixel ValueOption [] []) : Pixel [] [] =
        image
        |> Array.map (Array.map ValueOption.get)

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
