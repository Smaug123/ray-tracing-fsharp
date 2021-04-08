namespace RayTracing.App

open RayTracing
open System.IO.Abstractions
open Spectre.Console

module Program =

    type ProgressTask with

        member this.Increment (prog : float<progress>) = this.Increment (prog / 1.0<progress>)

    let go (sample : SampleImages) (ctx : ProgressContext) =
        let fs = FileSystem ()

        let output =
            fs.Path.GetTempFileName ()
            |> fun s -> fs.Path.ChangeExtension (s, ".ppm")
            |> fs.FileInfo.FromFileName

        let task = ctx.AddTask "[green]Generating and writing image[/]"
        let maxProgress, image = SampleImages.get sample task.Increment
        task.MaxValue <- maxProgress / 1.0<progress>

        let _, writer, await = ImageOutput.toPpm ignore image fs
        printfn "Temporary output being written eagerly to '%s'" writer.FullName

        async {
            do! await
            return! ImageOutput.convert writer output
        }
        |> Async.RunSynchronously

        printfn "%s" output.FullName

    [<EntryPoint>]
    let main (argv : string []) : int =
        let sample =
            argv
            |> Array.exactlyOne
            |> SampleImages.Parse

        let prog =
            AnsiConsole
                .Progress()
                .Columns (
                    TaskDescriptionColumn (),
                    ProgressBarColumn (),
                    PercentageColumn (),
                    RemainingTimeColumn (),
                    SpinnerColumn ()
                )

        prog.HideCompleted <- false
        prog.AutoClear <- false

        prog.Start (go sample)
        0
