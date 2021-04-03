namespace RayTracing

[<Measure>]
type progress

[<Struct>]
type Pixel =
    {
        Red : byte
        Green : byte
        Blue : byte
    }

type Image =
    | Image of Pixel [] []

[<RequireQualifiedAccess>]
module Image =
    let rowCount (Image i) : int =
        i.Length

    let colCount (Image i) : int =
        i.[0].Length

