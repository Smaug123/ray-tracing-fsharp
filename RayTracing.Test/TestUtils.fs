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

    let floatGen = Arb.generate<NormalFloat> |> Gen.map NormalFloat.op_Explicit

    let pointGen =
        Gen.three Arb.generate<NormalFloat>
        |> Gen.map (fun (i, j, k) -> Point [| i.Get ; j.Get ; k.Get |])

    let vectorGen =
        Gen.three Arb.generate<NormalFloat>
        |> Gen.map (fun (i, j, k) -> Vector [| i.Get ; j.Get ; k.Get |])

    let rayGen : Gen<Ray> =
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
