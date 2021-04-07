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
                ((originX : float, originY : float, originZ : float),
                 (origin2X : float, origin2Y : float, origin2Z : float),
                 (rayX : float, rayY : float, rayZ : float)),
                magnitude : float
            )
            : bool
            =
            let origin1 = [| originX; originY; originZ |] |> Point

            let origin2 =
                [| origin2X; origin2Y; origin2Z |] |> Point

            let vector = Vector [| rayX; rayY; rayZ |]
            let ray = { Origin = origin1; Vector = vector }
            let ray2 = { Origin = origin2; Vector = vector }
            let output = Ray.walkAlong ray magnitude

            let output2 =
                Ray.walkAlong ray2 magnitude

            let actual =
                Point.difference output output2

            let expected =
                Point.difference origin1 origin2

            Vector.equal actual expected

        let gen : Gen<float> =
            Arb.generate<NormalFloat>
            |> Gen.map NormalFloat.op_Explicit

        let gen =
            Gen.zip (Gen.three (Gen.three gen)) gen

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``walkAlong walks the right distance`` () =
        let property (ray : Ray, distance : float) =
            let walked = Ray.walkAlong ray distance
            Point.difference walked ray.Origin
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
