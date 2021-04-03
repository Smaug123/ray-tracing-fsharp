namespace RayTracing.Test

open RayTracing
open NUnit.Framework
open FsCheck

[<TestFixture>]
module TestRay =

    [<Test>]
    let ``Walking along two parallel rays maintains the same vector difference`` () =
        let property
            (num : Num<'a>)
            (
                ((originX : 'a, originY : 'a, originZ : 'a),
                 (origin2X : 'a, origin2Y : 'a, origin2Z : 'a),
                 (rayX : 'a, rayY : 'a, rayZ : 'a)),
                magnitude : 'a
            )
            : bool
            =
            let origin1 = [| originX; originY; originZ |] |> Point

            let origin2 =
                [| origin2X; origin2Y; origin2Z |] |> Point

            let vector = Point [| rayX; rayY; rayZ |]
            let ray = { Origin = origin1; Vector = vector }
            let ray2 = { Origin = origin2; Vector = vector }
            let output = Ray.walkAlong num ray magnitude

            let output2 =
                Ray.walkAlong num ray2 magnitude

            let actual =
                Point.difference num output output2

            let expected =
                Point.difference num origin1 origin2

            Point.equal num actual expected

        let gen : Gen<float> =
            Arb.generate<NormalFloat>
            |> Gen.map NormalFloat.op_Explicit

        let gen =
            Gen.zip (Gen.three (Gen.three gen)) gen

        property Num.float
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure
