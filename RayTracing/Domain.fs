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

type Ray<'a> =
    {
        Origin : Point<'a>
        Vector : Point<'a>
    }

[<RequireQualifiedAccess>]
module Ray =
    let walkAlong<'a> (num : Num<'a>) (ray : Ray<'a>) (magnitude : 'a) : Point<'a> =
        let (Point origin) = ray.Origin
        let (Point vector) = ray.Vector

        Array.zip origin vector
        |> Array.map (fun (originCoord, directionCoord) ->
            num.Add originCoord (num.Times directionCoord magnitude)
        )
        |> Point

[<RequireQualifiedAccess>]
module Vector =
    let dot<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : 'a =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.fold (fun s (i, j) -> num.Add s (num.Times i j)) num.Zero