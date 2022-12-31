namespace RayTracing

open System
open System.Runtime.CompilerServices

[<Measure>]
type albedo

[<Struct ; IsReadOnly>]
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

    let Yellow =
        {
            Red = 255uy
            Green = 255uy
            Blue = 0uy
        }

    let HotPink =
        {
            Red = 205uy
            Green = 105uy
            Blue = 180uy
        }

    let random (rand : Random) =
        let buffer = Array.zeroCreate<byte> 3
        rand.NextBytes buffer

        {
            Red = buffer.[0]
            Green = buffer.[1]
            Blue = buffer.[2]
        }

type PixelStats =
    private
        {
            mutable Count : int
            mutable SumRed : int
            mutable SumGreen : int
            mutable SumBlue : int
        }

[<RequireQualifiedAccess>]
module PixelStats =
    let empty () =
        {
            Count = 0
            SumRed = 0
            SumGreen = 0
            SumBlue = 0
        }

    let add (p : Pixel) (stats : PixelStats) : unit =
        stats.Count <- stats.Count + 1
        stats.SumRed <- stats.SumRed + int p.Red
        stats.SumGreen <- stats.SumGreen + int p.Green
        stats.SumBlue <- stats.SumBlue + int p.Blue

    let mean (stats : PixelStats) : Pixel =
        {
            Red = stats.SumRed / stats.Count |> byte
            Green = stats.SumGreen / stats.Count |> byte
            Blue = stats.SumBlue / stats.Count |> byte
        }

[<RequireQualifiedAccess>]
module Pixel =

    let difference (p1 : Pixel) (p2 : Pixel) : int =
        abs (int p1.Red - int p2.Red)
        + abs (int p1.Green - int p2.Green)
        + abs (int p1.Blue - int p2.Blue)

    let average (s : Pixel[]) : Pixel =
        let mutable r = s.[0].Red |> float
        let mutable g = s.[0].Green |> float
        let mutable b = s.[0].Blue |> float

        for i in 1 .. s.Length - 1 do
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
    let darken (albedo : float<albedo>) (p : Pixel) : Pixel =
        let albedo = albedo / 1.0<albedo>

        {
            Red = (float p.Red) * albedo |> Math.Round |> byte
            Green = (float p.Green) * albedo |> Math.Round |> byte
            Blue = (float p.Blue) * albedo |> Math.Round |> byte
        }
