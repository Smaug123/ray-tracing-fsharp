namespace RayTracing.Test

open System.Numerics
open RayTracing
open System.IO
open System.Reflection
open FsCheck

[<RequireQualifiedAccess>]
module TestUtils =

    type Dummy =
        class
        end

    let getEmbeddedResource (filename : string) : string =
        let filename =
            Assembly
                .GetAssembly(typeof<Dummy>)
                .GetManifestResourceNames ()
            |> Seq.filter (fun s -> s.EndsWith filename)
            |> Seq.exactlyOne

        use stream =
            Assembly
                .GetAssembly(typeof<Dummy>)
                .GetManifestResourceStream filename

        use reader = new StreamReader (stream)
        reader.ReadToEnd().Replace ("\r\n", "\n")

    let rationalGen : Gen<Rational> =
        gen {
            let! i = Gen.choose (-100, 100)
            let! sign = Gen.choose (0, 1)
            let! j = Gen.choose (1, 100)
            return Rational.Make (BigInteger i) (if sign = 0 then BigInteger j else BigInteger(-j))
        }

    let rec algebraicGen () : Gen<Algebraic> =
        [
            rationalGen |> Gen.map Algebraic.ofRational
            Gen.two (algebraicGen ()) |> Gen.map Algebraic.Sum
            Gen.two (algebraicGen ()) |> Gen.map Algebraic.Times
            // TODO make this nonnegative
            algebraicGen () |> Gen.map Algebraic.Sqrt
            // TODO make this nonzero
            algebraicGen () |> Gen.map Algebraic.Reciprocal
            // TODO more of these
        ]
        |> Gen.oneof

    let floatGen = Arb.generate<NormalFloat> |> Gen.map NormalFloat.op_Explicit

    let pointGen =
        Gen.three Arb.generate<NormalFloat>
        |> Gen.map (fun (i, j, k) -> Point [| i.Get ; j.Get ; k.Get |])

    let vectorGen =
        Gen.three Arb.generate<NormalFloat>
        |> Gen.map (fun (i, j, k) -> Vector [| i.Get ; j.Get ; k.Get |])

    let rayGen : Gen<Ray<float>> =
        gen {
            let! origin = pointGen
            let! direction = vectorGen
            return { Origin = origin ; Vector = direction }
        }

    let planeGen =
        gen {
            let! origin = pointGen
            let! v1 = vectorGen
            let! v2 = vectorGen
            return Plane.makeSpannedBy { Origin = origin ; Vector = v1 } { Origin = origin ; Vector = v2 }
        }
