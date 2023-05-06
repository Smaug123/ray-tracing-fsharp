namespace RayTracing.App

open System.IO
open RayTracing
open System.IO.Abstractions
open Spectre.Console

module Program =

    type ProgressTask with

        member this.Increment (progress : float<progress>) =
            this.Increment (progress / 1.0<progress>)

    let go (sample : SampleImages) (pngOutput : IFileInfo) (ctx : ProgressContext) =
        let renderTask = ctx.AddTask "[green]Generating image[/]"
        let writeUnorderedTask = ctx.AddTask "[green]Writing unordered pixels[/]"
        let writeTask = ctx.AddTask "[green]Writing PPM file[/]"

        let logFile =
            pngOutput.FileSystem.Path.GetTempFileName ()
            |> pngOutput.FileSystem.FileInfo.FromFileName

        use stream = logFile.OpenWrite ()
        use writer = new StreamWriter (stream)
        writer.AutoFlush <- true
        let lockObj = obj ()

        let write (s : string) =
            lock lockObj (fun () -> writer.WriteLine s)

        printfn "Log output, if any, to '%s'" logFile.FullName

        let maxProgress, image = SampleImages.get sample renderTask.Increment write
        renderTask.MaxValue <- maxProgress / 1.0<progress>
        writeUnorderedTask.MaxValue <- maxProgress / 1.0<progress>
        writeTask.MaxValue <- maxProgress / 1.0<progress>

        let tempOutput, await =
            ImageOutput.toPpm writeUnorderedTask.Increment image pngOutput.FileSystem

        AnsiConsole.WriteLine (sprintf "Temporary output being written eagerly to '%s'" tempOutput.FullName)

        async {
            do! Async.AwaitTask await

            let! pixelMap =
                ImageOutput.readPixelMap tempOutput (Image.rowCount image) (Image.colCount image)

            let pixelMap = ImageOutput.assertComplete pixelMap
            do! Png.write true writeTask.Increment pixelMap pngOutput
            tempOutput.Delete ()
            return ()
        }
        |> Async.RunSynchronously

        printfn "%s" pngOutput.FullName

    [<EntryPoint>]
    let main (argv : string[]) : int =
        let fs = FileSystem ()

        let sample, output =
            match argv with
            | [| name |] ->
                SampleImages.Parse name,
                fs.Path.GetTempFileName ()
                |> fun i -> fs.Path.ChangeExtension (i, ".png") |> fs.FileInfo.FromFileName
            | [| name ; output |] -> SampleImages.Parse name, fs.FileInfo.FromFileName output
            | _ -> failwithf "Expected two args 'sample name' 'output file', got %+A" argv

        let progress =
            AnsiConsole
                .Progress()
                .Columns (
                    TaskDescriptionColumn (),
                    ProgressBarColumn (),
                    PercentageColumn (),
                    RemainingTimeColumn (),
                    SpinnerColumn ()
                )

        progress.HideCompleted <- false
        progress.AutoClear <- false

        progress.Start (go sample output)
        0
