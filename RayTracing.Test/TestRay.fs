namespace RayTracing.Test

open RayTracing
open NUnit.Framework
open FsUnitTyped
open FsCheck

[<TestFixture>]
module TestRay =

    [<Test>]
    let ``Walking along two parallel rays maintains the same vector difference`` () =
        let property
            ((
            (originX : Rational,
             originY : Rational,
             originZ : Rational),
            (origin2X : Rational,
             origin2Y : Rational,
             origin2Z : Rational),
            (rayX : Rational,
             rayY : Rational,
             rayZ : Rational)
            ),
            magnitude : Rational)
            : bool
            =
            let origin1 = [| originX ; originY ; originZ |] |> Point
            let origin2 = [| origin2X ; origin2Y ; origin2Z |] |> Point
            let vector = Point [| rayX ; rayY ; rayZ |]
            let ray = { Origin = origin1 ; Vector = vector }
            let ray2 = { Origin = origin2 ; Vector = vector }
            let output = Ray.walkAlong Num.rational ray magnitude
            let output2 = Ray.walkAlong Num.rational ray2 magnitude
            let actual = Point.difference Num.rational output output2
            let expected = Point.difference Num.rational origin1 origin2
            actual = expected

        let gen = Gen.zip (Gen.three (Gen.three TestUtils.rationalGen)) TestUtils.rationalGen

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure

