namespace RayTracing.App

open System.Reflection
open SkiaSharp

[<RequireQualifiedAccess>]
module LoadImage =

    let fromResource (name : string) : SKBitmap =
        let assy = Assembly.GetExecutingAssembly ()

        use resource =
            assy.GetManifestResourceNames ()
            |> Seq.filter (fun i -> i.EndsWith name)
            |> Seq.head
            |> assy.GetManifestResourceStream

        SKBitmap.Decode resource
