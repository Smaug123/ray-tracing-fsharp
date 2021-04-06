namespace RayTracing.Test

open RayTracing
open NUnit.Framework
open FsUnitTyped
open System.IO.Abstractions.TestingHelpers

[<TestFixture>]
module TestRayTracing =

    [<Test>]
    let ``Wikipedia example of PPM output`` () =
        let fs = MockFileSystem ()

        let expected =
            TestUtils.getEmbeddedResource "PpmOutputExample.txt"

        let image =
            [|
                [|
                    { Red = 255uy; Blue = 0uy; Green = 0uy }
                    { Red = 0uy; Blue = 0uy; Green = 255uy }
                    { Red = 0uy; Blue = 255uy; Green = 0uy }
                |]
                [|
                    {
                        Red = 255uy
                        Blue = 0uy
                        Green = 255uy
                    }
                    {
                        Red = 255uy
                        Blue = 255uy
                        Green = 255uy
                    }
                    { Red = 0uy; Blue = 0uy; Green = 0uy }
                |]
            |]
            |> Image

        let outputFile =
            fs.Path.GetTempFileName ()
            |> fs.FileInfo.FromFileName

        let _, writer =
            ImageOutput.toPpm ignore image outputFile

        writer |> Async.RunSynchronously

        fs.File.ReadAllText outputFile.FullName
        |> shouldEqual expected
