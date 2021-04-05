namespace RayTracing.Test

open RayTracing
open FsCheck
open FsUnitTyped
open NUnit.Framework
open System

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
    let ``Average of a few pixels, case 1`` () =
        let pixels =
            [|
                (0uy, 234uy, 0uy)
                (0uy, 212uy, 0uy); (0uy, 59uy, 0uy); (0uy, 225uy, 0uy); (0uy, 132uy, 0uy);
                (0uy, 69uy, 0uy); (0uy, 207uy, 0uy); (0uy, 212uy, 0uy); (0uy, 30uy, 0uy);
                (0uy, 0uy, 0uy); (0uy, 179uy, 0uy); (0uy, 234uy, 0uy); (0uy, 54uy, 0uy);
                (0uy, 43uy, 0uy)
            |]
            |> Array.map (fun (r, g, b) -> { Red = r ; Green = g ; Blue = b })

        let avg = Pixel.average pixels

        avg.Green |> shouldEqual (pixels |> Seq.map (fun i -> float i.Green) |> Seq.average |> Math.Round |> byte)
        avg.Red |> shouldEqual (pixels |> Seq.map (fun i -> float i.Red) |> Seq.average |> Math.Round |> byte)
        avg.Blue |> shouldEqual (pixels |> Seq.map (fun i -> float i.Blue) |> Seq.average |> Math.Round |> byte)

    [<Test>]
    let ``Average of a few pixels, case 2`` () =
        let pixels =
            [|
                (0uy, 0uy, 136uy)
                (0uy, 0uy, 90uy); (0uy, 0uy, 109uy); (0uy, 0uy, 204uy); (0uy, 0uy, 209uy);
                (0uy, 0uy, 31uy); (0uy, 0uy, 244uy); (0uy, 0uy, 67uy); (0uy, 0uy, 139uy);
                (0uy, 0uy, 161uy); (0uy, 0uy, 179uy); (0uy, 0uy, 173uy); (0uy, 0uy, 100uy);
                (0uy, 0uy, 109uy); (0uy, 0uy, 122uy); (0uy, 0uy, 27uy); (0uy, 0uy, 249uy);
                (0uy, 0uy, 54uy)
            |]
            |> Array.map (fun (r, g, b) -> { Red = r ; Green = g ; Blue = b })

        let avg = Pixel.average pixels

        avg.Green |> shouldEqual (pixels |> Seq.map (fun i -> float i.Green) |> Seq.average |> Math.Round |> byte)
        avg.Red |> shouldEqual (pixels |> Seq.map (fun i -> float i.Red) |> Seq.average |> Math.Round |> byte)
        avg.Blue |> shouldEqual (pixels |> Seq.map (fun i -> float i.Blue) |> Seq.average |> Math.Round |> byte)

    [<Test>]
    let ``Average of a few pixels, case 3`` () =
        let pixels =
            [|
                (0uy, 0uy, 0uy)
                (0uy, 0uy, 123uy)
            |]
            |> Array.map (fun (r, g, b) -> { Red = r ; Green = g ; Blue = b })

        let avg = Pixel.average pixels

        avg.Green |> shouldEqual (pixels |> Seq.map (fun i -> float i.Green) |> Seq.average |> Math.Round |> byte)
        avg.Red |> shouldEqual (pixels |> Seq.map (fun i -> float i.Red) |> Seq.average |> Math.Round |> byte)
        avg.Blue |> shouldEqual (pixels |> Seq.map (fun i -> float i.Blue) |> Seq.average |> Math.Round |> byte)

    [<Test>]
    let ``Average of a few pixels`` () =
        let property (fst : byte * byte * byte) (values : (byte * byte * byte) list) : bool =
            let values = fst :: values
            let pixels =
                values
                |> List.map (fun (a, b, c) -> { Pixel.Red = a ; Green = b ; Blue = c })

            let avg = Pixel.average pixels

            avg.Green = (pixels |> List.map (fun i -> float i.Green) |> List.average |> Math.Round |> byte)
            && avg.Red = (pixels |> List.map (fun i -> float i.Red) |> List.average |> Math.Round |> byte)
            && avg.Blue = (pixels |> List.map (fun i -> float i.Blue) |> List.average |> Math.Round |> byte)

        Check.QuickThrowOnFailure property
