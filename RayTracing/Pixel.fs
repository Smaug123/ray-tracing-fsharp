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
    let average (s : Pixel []) : Pixel =
        let mutable r = s.[0].Red |> float
        let mutable g = s.[0].Green |> float
        let mutable b = s.[0].Blue |> float
        for i in 1..s.Length - 1 do
            r <- r + float s.[i].Red
            g <- g + float s.[i].Green
            b <- b + float s.[i].Blue
        let count = s.Length |> float
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