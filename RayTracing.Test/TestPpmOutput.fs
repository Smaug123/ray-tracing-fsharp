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
                    async { return Colour.Red }
                    async { return Colour.Green }
                    async { return Colour.Blue }
                |]
                [|
                    async {
                        return {
                            Red = 255uy
                            Blue = 0uy
                            Green = 255uy
                        }
                    }
                    async { return Colour.Black }
                    async { return Colour.White }
                |]
            |]
            |> Image.make

        let outputFile =
            fs.Path.GetTempFileName ()
            |> fs.FileInfo.FromFileName

        let _, writer =
            ImageOutput.toPpm ignore image outputFile

        writer |> Async.RunSynchronously

        fs.File.ReadAllText outputFile.FullName
        |> shouldEqual expected
