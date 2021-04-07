namespace RayTracing

open System
open RayTracing

[<Measure>]
type progress

type Image = Image of Pixel [] []

[<RequireQualifiedAccess>]
module Image =
    let rowCount (Image i) : int = i.Length

    let colCount (Image i) : int = i.[0].Length

