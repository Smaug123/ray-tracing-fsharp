namespace RayTracing.Test

open NUnit.Framework
open FsCheck
open FsUnitTyped
open RayTracing

[<TestFixture>]
module TestSphereIntersection =


    let sphere : Gen<Sphere> =
        gen {
            let! origin = TestUtils.pointGen
            let! radius = Arb.generate<NormalFloat>
            return Sphere.make (SphereStyle.LightSource (Texture.Colour Colour.White)) origin radius.Get
        }

    [<Test>]
    let ``Intersection of sphere and ray does lie on both`` () =
        let property (ray : Ray, sphere : Sphere) : bool =
            let intersection = Sphere.firstIntersection sphere ray
            intersection
            |> ValueOption.map (fun distance ->
                let p = Ray.walkAlong ray distance
                Sphere.liesOn p sphere
            )
            |> ValueOption.defaultValue true

        property
        |> Prop.forAll (Arb.fromGen (Gen.zip TestUtils.rayGen sphere))
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Intersection of sphere and ray does lie on both, case 1`` () =
        let ray =
            Ray.make' (Point.make 1.462205539 -4.888279676 7.123293244) (Vector.make -9.549697616 4.400018428 10.41024923)
            |> Option.get
        let sphere = Sphere.make (SphereStyle.PureReflection (1.0<albedo>, Texture.Colour Colour.White)) (Point.make -5.688391601 -5.360125644 9.074300761) 8.199747973

        let intersection = Sphere.firstIntersection sphere ray

        intersection
        |> ValueOption.map (fun distance ->
            let p = Ray.walkAlong ray distance
            Sphere.liesOn p sphere
        )
        |> ValueOption.defaultValue true
        |> shouldEqual true
