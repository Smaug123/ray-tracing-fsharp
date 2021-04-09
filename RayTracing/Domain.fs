namespace RayTracing

open RayTracing

[<Measure>]
type progress

type Image =
    private
        {
            Rows : Pixel Async [] seq
            RowCount : int
            ColCount : int
        }

[<RequireQualifiedAccess>]
module Image =
    let rowCount i : int = i.RowCount

    let colCount i = i.ColCount

    let render (i : Image) : (Pixel * Async<unit>) [] seq =
        i.Rows
        |> Seq.map (fun imageRow ->
            let outputRow = Array.zeroCreate<Pixel * Async<unit>> i.ColCount
            let doIt =
                imageRow
                |> Array.mapi (fun i p -> async {
                    let! pixel = p
                    let _, a = outputRow.[i]
                    outputRow.[i] <- pixel, a
                })
            for k in 0..i.ColCount - 1 do
                outputRow.[k] <- Unchecked.defaultof<_>, doIt.[k]
            outputRow
        )

    let make (rowCount : int) (colCount : int) (pixels : Async<Pixel>[] seq) : Image =
        {
            RowCount = rowCount
            ColCount = colCount
            Rows = pixels
        }