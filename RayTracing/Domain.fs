namespace RayTracing

open System.Threading.Tasks
open RayTracing

[<Measure>]
type progress

type Image =
    private
        {
            Rows : Async<Pixel []> seq
            RowCount : int
            ColCount : int
        }

[<RequireQualifiedAccess>]
module Image =
    let rowCount i : int = i.RowCount

    let colCount i = i.ColCount

    let render (i : Image) : Pixel [] [] Task =
        i.Rows
        |> Async.Parallel
        |> Async.StartAsTask

    let make (rowCount : int) (colCount : int) (pixels : Async<Pixel[]> seq) : Image =
        {
            RowCount = rowCount
            ColCount = colCount
            Rows = pixels
        }
