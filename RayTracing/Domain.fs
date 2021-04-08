namespace RayTracing

open RayTracing

[<Measure>]
type progress

type Image =
    {
        Rows : Pixel Async [] []
        RowCount : int
        ColCount : int
    }

[<RequireQualifiedAccess>]
module Image =
    let rowCount i : int = i.RowCount

    let colCount i = i.ColCount

    let render (i : Image) : (Pixel * Async<unit>) [] [] =
        Array.init i.RowCount (fun rowNum ->
            let row = Array.zeroCreate<Pixel * Async<unit>> i.ColCount
            let doIt =
                i.Rows.[rowNum]
                |> Array.mapi (fun i p -> async {
                    let! pixel = p
                    let _, a = row.[i]
                    row.[i] <- pixel, a
                })
            for k in 0..i.ColCount - 1 do
                row.[k] <- Unchecked.defaultof<_>, doIt.[k]
            row
        )

    let make (pixels : Async<Pixel>[][]) : Image =
        {
            RowCount = pixels.Length
            ColCount = pixels.[0].Length
            Rows = pixels
        }