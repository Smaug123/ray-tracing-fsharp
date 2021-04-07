namespace RayTracing

open System

[<Struct>]
type Pixel =
    {
        Red : byte
        Green : byte
        Blue : byte
    }

[<RequireQualifiedAccess>]
module Colour =
    let Black =
        {
            Red = 0uy
            Green = 0uy
            Blue = 0uy
        }
    let White =
        {
            Red = 255uy
            Green = 255uy
            Blue = 255uy
        }
    let Red =
        {
            Red = 255uy
            Green = 0uy
            Blue = 0uy
        }
    let Green =
        {
            Red = 0uy
            Green = 255uy
            Blue = 0uy
        }
    let Blue =
        {
            Red = 0uy
            Green = 0uy
            Blue = 255uy
        }

[<RequireQualifiedAccess>]
module Pixel =
    let average (s : Pixel seq) : Pixel =
        use e = s.GetEnumerator ()
        if not (e.MoveNext ()) then failwith "Input sequence was empty when averaging pixels"
        let mutable count = 1
        let mutable r = e.Current.Red |> float
        let mutable g = e.Current.Green |> float
        let mutable b = e.Current.Blue |> float
        while e.MoveNext () do
            count <- count + 1
            r <- r + float e.Current.Red
            g <- g + float e.Current.Green
            b <- b + float e.Current.Blue
        let count = float count
        {
            Red = byte (Math.Round (r / count))
            Green = byte (Math.Round (g / count))
            Blue = byte (Math.Round (b / count))
        }

    let combine (p1 : Pixel) (p2 : Pixel) : Pixel =
        {
            Red = (int p1.Red * int p2.Red) / 255 |> byte
            Green = (int p1.Green * int p2.Green) / 255 |> byte
            Blue = (int p1.Blue * int p2.Blue) / 255 |> byte
        }

    /// albedo should be between 0 and 1.
    let darken (p : Pixel) (albedo : float) : Pixel =
        {
            Red = (float p.Red) * albedo |> Math.Round |> byte
            Green = (float p.Green) * albedo |> Math.Round |> byte
            Blue = (float p.Blue) * albedo |> Math.Round |> byte
        }