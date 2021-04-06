namespace RayTracing.Test

open RayTracing
open NUnit.Framework
open FsCheck

[<TestFixture>]
module TestSphere =

    [<Test>]
    let ``Point at distance r from centre lies on sphere`` () =
        let property (centre : Point<float>, radius : float, point : Point<float>) : bool =
            let radius = abs radius
            let sphere = Sphere.make Num.float (SphereStyle.PureReflection (1.0, Colour.White)) centre radius
            Sphere.liesOn Num.float point sphere


        let gen : Gen<Point<float> * float * Point<float>> =
            gen {
                let! centre = TestUtils.pointGen
                let! radius = Arb.generate<NormalFloat> |> Gen.map NormalFloat.op_Explicit
                let! theta =
                    Arb.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit
                    |> Gen.map Radian
                let! phi =
                    Arb.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit
                    |> Gen.map Radian

                let surfacePoint =
                    [|
                        radius * Num.float.Cos phi * Num.float.Sin theta
                        radius * Num.float.Sin phi * Num.float.Sin theta
                        radius * Num.float.Cos theta
                    |]
                    |> Point
                    |> Point.difference Num.float centre
                    |> fun (Vector v) -> Point v
                return centre, radius, surfacePoint
            }

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure