namespace RayTracing.Test

open RayTracing
open NUnit.Framework
open FsCheck

[<TestFixture>]
module TestSphere =

    [<Test>]
    let ``Point at distance r from centre lies on sphere`` () =
        let property (centre : Point, radius : float, point : Point) : bool =
            let radius = abs radius
            let sphere = Sphere.make (SphereStyle.PureReflection (1.0<albedo>, Colour.White)) centre radius
            Sphere.liesOn point sphere


        let gen : Gen<Point * float * Point> =
            gen {
                let! centre = TestUtils.pointGen
                let! radius = Arb.generate<NormalFloat> |> Gen.map NormalFloat.op_Explicit
                let! theta =
                    Arb.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit
                let! phi =
                    Arb.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit

                let surfacePoint =
                    Point.make
                        (radius * cos phi * sin theta)
                        (radius * sin phi * sin theta)
                        (radius * cos theta)
                    |> fun p -> Point.sum centre p
                return centre, radius, surfacePoint
            }

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure