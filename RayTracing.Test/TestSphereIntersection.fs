namespace RayTracing.Test

open NUnit.Framework
open FsCheck
open RayTracing

[<TestFixture>]
module TestSphereIntersection =

    [<Test>]
    let ``Intersection of sphere and ray does lie on both`` () =
        let property (ray : Ray<float>, sphere : Sphere<float>) : bool =
            let intersections = Sphere.intersections Num.float sphere ray
            intersections
            |> Seq.forall (fun p ->
                let rayOk = Ray.liesOn Num.float p ray
                let sphereOk = Sphere.liesOn Num.float p sphere
                rayOk && sphereOk
            )

        let point =
            Gen.three Arb.generate<NormalFloat>
            |> Gen.map (fun (i, j, k) -> Point [| i.Get ; j.Get ; k.Get |])

        let ray : Gen<Ray<float>> =
            gen {
                let! origin = point
                let! direction = point
                return { Origin = origin ; Vector = direction }
            }

        let sphere : Gen<Sphere<float>> =
            gen {
                let! origin = point
                let! radius = Arb.generate<NormalFloat>
                return { Centre = origin ; Radius = abs radius.Get }
            }

        property
        |> Prop.forAll (Arb.fromGen (Gen.zip ray sphere))
        |> Check.QuickThrowOnFailure