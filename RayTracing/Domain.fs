namespace RayTracing

open RayTracing

[<Measure>]
type progress

type Image =
    private
        {
            Rows : Pixel [] seq
            RowCount : int
            ColCount : int
        }

[<RequireQualifiedAccess>]
module Image =
    let rowCount i : int = i.RowCount

    let colCount i = i.ColCount

    let render (i : Image) : Pixel [] [] =
        i.Rows
        |> Seq.toArray

    let make (rowCount : int) (colCount : int) (pixels : Pixel[] seq) : Image =
        {
            RowCount = rowCount
            ColCount = colCount
            Rows = pixels
        }
