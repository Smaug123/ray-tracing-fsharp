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
            let v1, v2 = Plane.basis (Vector.make 0.0 1.0 0.0) orth
            let dotVectors = UnitVector.dot (Ray.vector v1) (Ray.vector v2)
            let v1Length = UnitVector.dot (Ray.vector v1) (Ray.vector v1)
            let v2Length = UnitVector.dot (Ray.vector v2) (Ray.vector v2)

            Float.equal dotVectors 0.0
            && Float.equal v1Length 1.0
            && Float.equal v2Length 1.0

        property
        |> Prop.forAll (Arb.fromGen TestUtils.planeGen)
        |> Check.QuickThrowOnFailure
