namespace RayTracing.Test

open NUnit.Framework
open FsCheck
open FsUnitTyped
open RayTracing

[<TestFixture>]
module TestSphereIntersection =


    let sphere : Gen<Sphere<float>> =
        gen {
            let! origin = TestUtils.pointGen
            let! radius = Arb.generate<NormalFloat>
            return Sphere.make Num.float SphereStyle.WhiteLightSource origin radius.Get
        }

    [<Test>]
    let ``Intersection of sphere and ray does lie on both`` () =
        let property (ray : Ray<float>, sphere : Sphere<float>) : bool =
            let intersections = Sphere.intersections Num.float sphere ray Pixel.White
            intersections
            |> Seq.forall (fun (p, _, _) ->
                let rayOk = Ray.liesOn Num.float p ray
                let sphereOk = Sphere.liesOn Num.float p sphere
                rayOk && sphereOk
            )
            &&
            intersections
            |> Array.map (fun (intersection, _, _) -> Point.normSquared Num.float (Point.difference Num.float ray.Origin intersection))
            |> Seq.pairwise
            |> Seq.forall (fun (i, j) -> Num.float.Compare i j = Less)

        property
        |> Prop.forAll (Arb.fromGen (Gen.zip TestUtils.rayGen sphere))
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Intersection of sphere and ray does lie on both, case 1`` () =
        let ray =
            {
                Origin = Point [|1.462205539; -4.888279676; 7.123293244|]
                Vector = Point [|-9.549697616; 4.400018428; 10.41024923|]
            }
        let sphere = Sphere.make Num.float SphereStyle.PureReflection (Point [|-5.688391601; -5.360125644; 9.074300761|]) 8.199747973

        let intersections = Sphere.intersections Num.float sphere ray Pixel.White

        intersections
        |> Array.map (fun (intersection, _, _) -> Point.normSquared Num.float (Point.difference Num.float ray.Origin intersection))
        |> Seq.pairwise
        |> Seq.forall (fun (i, j) -> Num.float.Compare i j = Less)
        |> shouldEqual true

        intersections
        |> Seq.forall (fun (p, _, _) -> Ray.liesOn Num.float p ray)
        |> shouldEqual true

        intersections
        |> Seq.forall (fun (p, _, _) -> Sphere.liesOn Num.float p sphere)
        |> shouldEqual true