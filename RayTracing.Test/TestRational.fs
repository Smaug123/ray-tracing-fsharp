namespace RayTracing.Test

open NUnit.Framework
open FsCheck
open RayTracing

[<TestFixture>]
module TestRational =

    [<Test>]
    let ``ofInt compares correctly`` () =
        let property (i : int) (j : int) : bool =
            let i1 = Rational.ofInt i
            let j1 = Rational.ofInt j

            if i1 < j1 then i < j
            elif i1 > j1 then i > j
            else i = j

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Addition preserves comparison`` () =
        let property (i : Rational, j : Rational, k : Rational) : bool =
            if i < j then
                Rational.add i k < Rational.add j k
            elif i = j then
                Rational.add i k = Rational.add j k
            else
                Rational.add i k > Rational.add j k

        property
        |> Prop.forAll (Arb.fromGen (Gen.three TestUtils.rationalGen))
        |> Check.QuickThrowOnFailure
