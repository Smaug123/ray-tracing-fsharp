namespace RayTracing.Test

open RayTracing
open FsCheck
open NUnit.Framework

[<TestFixture>]
module TestPixel =

    [<Test>]
    let ``Average of one pixel`` () =
        let property (p1 : byte) (p2 : byte) (p3 : byte) : bool =
            let pixel = { Red = p1 ; Green = p2 ; Blue = p3 }
            Pixel.average [ pixel ]
            |> (=) pixel

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Average of a few pixels`` () =
        let property (fst : byte * byte * byte) (values : (byte * byte * byte) list) : bool =
            let values = fst :: values
            let pixels =
                values
                |> List.map (fun (a, b, c) -> { Pixel.Red = a ; Green = b ; Blue = c })

            let avg = Pixel.average pixels

            avg.Green = (pixels |> List.map (fun i -> float i.Green) |> List.average |> byte)
            && avg.Red = (pixels |> List.map (fun i -> float i.Red) |> List.average |> byte)
            && avg.Blue = (pixels |> List.map (fun i -> float i.Blue) |> List.average |> byte)

        Check.QuickThrowOnFailure property
