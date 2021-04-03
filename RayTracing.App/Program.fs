namespace RayTracing.App

open RayTracing
open System.IO.Abstractions
open Spectre.Console

module Program =

    type ProgressTask with
        member this.Increment (prog : float<progress>) = this.Increment (prog / 1.0<progress>)

    let go (ctx : ProgressContext) =
        let fs = FileSystem()
        let output =
            fs.Path.GetTempFileName ()
            |> fun s -> fs.Path.ChangeExtension (s, ".ppm")
            |> fs.FileInfo.FromFileName

        let task = ctx.AddTask "[green]Generating image[/]"
        let maxProgress, image = SampleImages.gradient task.Increment
        task.MaxValue <- maxProgress / 1.0<progress>

        let image = image |> Async.RunSynchronously

        let outputTask = ctx.AddTask "[green]Writing image[/]"
        let maxProgress, writer = ImageOutput.toPpm outputTask.Increment image output
        outputTask.MaxValue <- maxProgress / 1.0<progress>

        writer |> Async.RunSynchronously

        printfn "%s" output.FullName

    [<EntryPoint>]
    let main (_ : string []) : int =
        let prog =
            AnsiConsole.Progress()
                .Columns(
                    TaskDescriptionColumn(),
                    ProgressBarColumn(),
                    PercentageColumn(),
                    RemainingTimeColumn(),
                    SpinnerColumn()
                )
        prog.HideCompleted <- false
        prog.AutoClear <- false

        prog.Start go
        0
