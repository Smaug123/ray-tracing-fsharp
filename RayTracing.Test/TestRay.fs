namespace RayTracing.Test

open RayTracing
open NUnit.Framework
open FsCheck

[<TestFixture>]
module TestRay =

    [<Test>]
    let ``Walking along two parallel rays maintains the same vector difference`` () =
        let property
            (
                (((originX : float, originY : float, originZ : float),
                  (origin2X : float, origin2Y : float, origin2Z : float)),
                 vector : UnitVector),
                magnitude : float
            )
            : bool
            =
            let origin1 = Point.make originX originY originZ

            let origin2 =
                Point.make origin2X origin2Y origin2Z

            let output = Ray.walkAlong (Ray.make origin1 vector) magnitude

            let output2 = Ray.walkAlong (Ray.make origin2 vector) magnitude

            let actual =
                Point.differenceToThenFrom output output2

            let expected =
                Point.differenceToThenFrom origin1 origin2

            Vector.equal actual expected

        let gen : Gen<float> =
            Arb.generate<NormalFloat>
            |> Gen.map NormalFloat.op_Explicit

        let gen =
            Gen.zip (Gen.zip (Gen.two (Gen.three gen)) TestUtils.unitVectorGen) gen

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``walkAlong walks the right distance`` () =
        let property (ray : Ray, distance : float) =
            let walked = Ray.walkAlong ray distance
            Point.differenceToThenFrom walked (Ray.origin ray)
            |> Vector.normSquared
            |> Float.equal (distance * distance)

        property
        |> Prop.forAll (Arb.fromGen (Gen.zip TestUtils.rayGen (Arb.generate<NormalFloat> |> Gen.map NormalFloat.op_Explicit)))
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``walkAlong stays on the ray`` () =
        let property (ray : Ray, distance : float) =
            let walked = Ray.walkAlong ray distance
            Ray.liesOn walked ray

        property
        |> Prop.forAll (Arb.fromGen (Gen.zip TestUtils.rayGen (Arb.generate<NormalFloat> |> Gen.map NormalFloat.op_Explicit)))
        |> Check.QuickThrowOnFailure
