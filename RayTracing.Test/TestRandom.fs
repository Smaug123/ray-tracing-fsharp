namespace RayTracing.Test

open System
open NUnit.Framework
open FsUnitTyped
open RayTracing

[<TestFixture>]
module TestRandom =

    [<TestCase 1>]
    [<TestCase 2>]
    [<TestCase 3>]
    let ``Random floats are in the right range`` (outputNum : int) =
        let check (r : float) : unit =
            Double.IsNormal r |> shouldEqual true
            r < 0.0 |> shouldEqual false
            r > 1.0 |> shouldEqual false

        let rand = Random () |> FloatProducer

        for _ in 1..100 do
            match outputNum with
            | 1 ->
                let r = rand.Get ()
                check r
            | 2 ->
                let struct (r, s) = rand.GetTwo ()
                check r
                check s
            | 3 ->
                let struct (r, s, t) = rand.GetThree ()
                check r
                check s
                check t
            | _ -> failwithf "unknown: %i" outputNum

    [<Test>]
    let ``Random floats are distributed over the whole range`` () =
        let rand = Random () |> FloatProducer
        let randoms = Array.init 100 (fun _ -> rand.Get ()) |> Array.sort

        for i in 0..9 do
            let contains =
                randoms |> Array.exists (fun j -> float i * 0.1 < j && j < float (i + 1) * 0.1)

            if not contains then
                failwithf "%i: %+A" i randoms

            contains |> shouldEqual true

    [<TestCase 1>]
    [<TestCase 2>]
    [<TestCase 3>]
    let ``Floats aren't obviously correlated`` (inputNum : int) =
        let rand = Random () |> FloatProducer

        match inputNum with
        | 1 ->
            let r1 = rand.Get ()
            let r2 = rand.Get ()
            r1 |> shouldNotEqual r2
        | 2 ->
            let struct (r1, r2) = rand.GetTwo ()
            let struct (r3, r4) = rand.GetTwo ()
            Set.ofList [ r1 ; r2 ; r3 ; r4 ] |> Set.count |> shouldEqual 4
        | 3 ->
            let struct (r1, r2, r3) = rand.GetThree ()
            let struct (r4, r5, r6) = rand.GetThree ()
            Set.ofList [ r1 ; r2 ; r3 ; r4 ; r5 ; r6 ] |> Set.count |> shouldEqual 6
        | _ -> failwithf "unrecognised: %i" inputNum
