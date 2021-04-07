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
            Float.equal (UnitVector.dot (Ray.vector v1) (Ray.vector v2)) 0.0
            && Float.equal (UnitVector.dot (Ray.vector v1) (Ray.vector v1)) 1.0
            && Float.equal (UnitVector.dot (Ray.vector v2) (Ray.vector v2)) 1.0

        property
        |> Prop.forAll (Arb.fromGen TestUtils.planeGen)
        |> Check.QuickThrowOnFailure