namespace RayTracing.Test

open NUnit.Framework
open FsCheck
open RayTracing

[<TestFixture>]
module TestPlane =

    [<Test>]
    let ``Orthogonalise does make orthogonal vectors`` () =
        let property (p : Plane) : bool =
            let orth = Plane.orthonormalise p |> Option.get
            let v1, v2 = Plane.basis orth
            Float.equal (Vector.dot v1.Vector v2.Vector) 0.0
            && Float.equal (Vector.dot v1.Vector v1.Vector) 1.0
            && Float.equal (Vector.dot v2.Vector v2.Vector) 1.0

        property
        |> Prop.forAll (Arb.fromGen TestUtils.planeGen)
        |> Check.QuickThrowOnFailure