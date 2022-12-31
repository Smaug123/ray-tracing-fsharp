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

        let expected = TestUtils.getEmbeddedResource "PpmOutputExample.txt"

        let image =
            [|
                [|
                    async { return Colour.Red }
                    async { return Colour.Green }
                    async { return Colour.Blue }
                |]
                [|
                    async {
                        return
                            {
                                Red = 255uy
                                Blue = 0uy
                                Green = 255uy
                            }
                    }
                    async { return Colour.White }
                    async { return Colour.Black }
                |]
            |]
            |> Image.make 2 3

        let outputFile = fs.Path.GetTempFileName () |> fs.FileInfo.FromFileName

        let tempOutput, await = ImageOutput.toPpm ignore image fs

        async {
            do! await
            let! pixelMap = ImageOutput.readPixelMap ignore tempOutput (Image.rowCount image) (Image.colCount image)
            let arr = ImageOutput.assertComplete pixelMap
            do! ImageOutput.writePpm false ignore arr outputFile
            return ()
        }
        |> Async.RunSynchronously

        fs.File.ReadAllText outputFile.FullName |> shouldEqual expected
