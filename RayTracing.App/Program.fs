namespace RayTracing.App

open RayTracing
open System.IO.Abstractions
open Spectre.Console

module Program =

    type ProgressTask with

        member this.Increment (prog : float<progress>) = this.Increment (prog / 1.0<progress>)

    let go (sample : SampleImages) (ctx : ProgressContext) =
        let fs = FileSystem ()

        let ppmOutput =
            fs.Path.GetTempFileName ()
            |> fun s -> fs.Path.ChangeExtension (s, ".ppm")
            |> fs.FileInfo.FromFileName

        let renderTask = ctx.AddTask "[green]Generating image[/]"
        let writeUnorderedTask = ctx.AddTask "[green]Writing unordered pixels[/]"
        let readTask = ctx.AddTask "[green]Reading in serialised pixels[/]"
        let arrangeTask = ctx.AddTask "[green]Rearranging pixels into correct order[/]"
        let writeTask = ctx.AddTask "[green]Writing PPM file[/]"

        let maxProgress, image = SampleImages.get sample renderTask.Increment
        renderTask.MaxValue <- maxProgress / 1.0<progress>
        writeUnorderedTask.MaxValue <- maxProgress / 1.0<progress>
        readTask.MaxValue <- maxProgress / 1.0<progress>
        arrangeTask.MaxValue <- maxProgress / 1.0<progress>
        writeTask.MaxValue <- maxProgress / 1.0<progress>

        let tempOutput, await = ImageOutput.toPpm writeUnorderedTask.Increment image fs
        AnsiConsole.WriteLine (sprintf "Temporary output being written eagerly to '%s'" tempOutput.FullName)

        async {
            do! await
            let! pixelMap = ImageOutput.readPixelMap readTask.Increment tempOutput
            let! arr = ImageOutput.toArray arrangeTask.Increment pixelMap
            do! ImageOutput.writePpm writeTask.Increment arr ppmOutput
            return ()
        }
        |> Async.RunSynchronously

        printfn "%s" ppmOutput.FullName

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
