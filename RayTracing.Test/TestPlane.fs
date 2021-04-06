namespace RayTracing.Test

open NUnit.Framework
open FsCheck
open RayTracing

[<TestFixture>]
module TestPlane =

    [<Test>]
    let ``Orthogonalise does make orthogonal vectors`` () =
        let property (p : Plane<float>) : bool =
            let orth = Plane.orthonormalise Num.float p |> Option.get
            let v1, v2 = Plane.basis orth
            Num.float.Equal (Vector.dot Num.float v1.Vector v2.Vector) Num.float.Zero
            && Num.float.Equal (Vector.dot Num.float v1.Vector v1.Vector) Num.float.One
            && Num.float.Equal (Vector.dot Num.float v2.Vector v2.Vector) Num.float.One

        property
        |> Prop.forAll (Arb.fromGen TestUtils.planeGen)
        |> Check.QuickThrowOnFailure