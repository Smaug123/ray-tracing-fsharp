namespace TestRayTracing

open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module TestUtils =

    type Dummy = class end

    let getEmbeddedResource (filename : string) : string =
        let filename =
            Assembly.GetAssembly(typeof<Dummy>).GetManifestResourceNames ()
            |> Seq.filter (fun s -> s.EndsWith filename)
            |> Seq.exactlyOne
        use stream =
            Assembly.GetAssembly(typeof<Dummy>).GetManifestResourceStream filename

        use reader = new StreamReader (stream)
        reader.ReadToEnd().Replace("\r\n", "\n")