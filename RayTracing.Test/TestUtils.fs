namespace RayTracing.Test

open RayTracing
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp

[<RequireQualifiedAccess>]
module TestUtils =

    type Dummy = class end

    let getEmbeddedResource (filename : string) : string =
        let filename =
            Assembly.GetAssembly(typeof<Dummy>).GetManifestResourceNames ()
            |> Seq.filter (fun s -> s.EndsWith filename)
            |> Seq.exactlyOne

        use stream = Assembly.GetAssembly(typeof<Dummy>).GetManifestResourceStream filename

        use reader = new StreamReader (stream)
        reader.ReadToEnd().Replace ("\r\n", "\n")

    let floatGen =
        ArbMap.defaults
        |> ArbMap.generate<NormalFloat>
        |> Gen.map NormalFloat.op_Explicit

    let pointGen =
        Gen.three (ArbMap.defaults |> ArbMap.generate<NormalFloat>)
        |> Gen.map (fun (i, j, k) -> Point.make i.Get j.Get k.Get)

    let vectorGen =
        Gen.three (ArbMap.defaults |> ArbMap.generate<NormalFloat>)
        |> Gen.map (fun (i, j, k) -> Vector.make i.Get j.Get k.Get)

    let unitVectorGen =
        vectorGen
        |> Gen.filter (fun i -> Vector.normSquared i > 0.0)
        |> Gen.map Vector.unitise
        |> Gen.map ValueOption.get

    let rayGen : Gen<Ray> =
        gen {
            let! origin = pointGen
            let! direction = unitVectorGen
            return Ray.make origin direction
        }

    let planeGen =
        gen {
            let! origin = pointGen
            let! v1 = unitVectorGen
            let! v2 = unitVectorGen
            return Plane.makeSpannedBy (Ray.make origin v1) (Ray.make origin v2)
        }
