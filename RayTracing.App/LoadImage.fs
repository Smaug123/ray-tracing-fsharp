namespace RayTracing.App

open System.Drawing
open System.Reflection

[<RequireQualifiedAccess>]
module LoadImage =

    let fromResource (name : string) : Bitmap =
        let assy = Assembly.GetExecutingAssembly ()

        use resource =
            assy.GetManifestResourceNames ()
            |> Seq.filter (fun i -> i.EndsWith name)
            |> Seq.head
            |> assy.GetManifestResourceStream

        let b = new Bitmap (resource)
        b
