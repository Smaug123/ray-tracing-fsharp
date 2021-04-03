namespace RayTracing.Test

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
            return Rational.Make i (if sign = 0 then j else -j)
        }
