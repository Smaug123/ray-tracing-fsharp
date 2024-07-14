namespace RayTracing.Test

open System
open RayTracing
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open FsUnitTyped

[<TestFixture>]
module TestSphere =

    [<Test>]
    let ``Point at distance r from centre lies on sphere`` () =
        let property (centre : Point, radius : float, point : Point) : bool =
            let radius = abs radius

            let sphere =
                Sphere.make (SphereStyle.PureReflection (1.0<albedo>, Texture.Colour Colour.White)) centre radius

            Sphere.liesOn point sphere


        let gen : Gen<Point * float * Point> =
            gen {
                let! centre = TestUtils.pointGen

                let! radius =
                    ArbMap.defaults
                    |> ArbMap.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit

                let! theta =
                    ArbMap.defaults
                    |> ArbMap.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit

                let! phi =
                    ArbMap.defaults
                    |> ArbMap.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit

                let surfacePoint =
                    Point.make (radius * cos phi * sin theta) (radius * sin phi * sin theta) (radius * cos theta)
                    |> fun p -> Point.sum centre p

                return centre, radius, surfacePoint
            }

        property |> Prop.forAll (Arb.fromGen gen) |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Glass sphere perfectly reflects against the edge`` () =
        let rand = Random () |> FloatProducer

        let ray =
            Ray.make (Point.make 0.0 0.0 0.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> ValueOption.get)

        let strikePoint = Point.make 0.0 0.0 1.0

        let mutable incoming =
            {
                LightRay.Ray = ray
                Colour = Colour.White
            }

        let destination =
            Sphere.reflection
                (SphereStyle.Glass (1.0<albedo>, Texture.Colour Colour.Green, 1.5<ior>, rand))
                (Point.make 0.0 1.0 1.0)
                1.0
                1.0
                false
                &incoming
                strikePoint

        match destination with
        | ValueNone ->
            incoming.Colour |> shouldEqual Colour.Green
            Point.equal (Ray.origin incoming.Ray) strikePoint |> shouldEqual true

            Vector.equal (Ray.vector incoming.Ray |> UnitVector.scale 1.0) (Ray.vector ray |> UnitVector.scale 1.0)
            |> shouldEqual true
        | ValueSome colour -> failwithf "Absorbed: %+A" colour

    [<Test>]
    let ``Glass sphere perfectly refracts through the middle`` () =
        let rand = Random () |> FloatProducer

        let ray =
            Ray.make (Point.make 0.0 0.0 0.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> ValueOption.get)

        let strikePoint = Point.make 0.0 0.0 1.0

        let mutable incoming =
            {
                LightRay.Ray = ray
                Colour = Colour.White
            }

        let destination =
            Sphere.reflection
                (SphereStyle.Glass (1.0<albedo>, Texture.Colour Colour.Green, 1.5<ior>, rand))
                (Point.make 0.0 0.0 2.0)
                1.0
                1.0
                false
                &incoming
                strikePoint

        match destination with
        | ValueNone ->
            incoming.Colour |> shouldEqual Colour.Green
            Point.equal (Ray.origin incoming.Ray) strikePoint |> shouldEqual true

            Vector.equal (Ray.vector incoming.Ray |> UnitVector.scale 1.0) (Ray.vector ray |> UnitVector.scale 1.0)
            |> shouldEqual true
        | ValueSome colour -> failwithf "Absorbed: %+A" colour

    [<Test>]
    let ``Dielectric sphere refracts when incoming ray `` () =
        let rand = Random () |> FloatProducer

        let ray =
            Ray.make (Point.make 0.0 0.0 0.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> ValueOption.get)

        let strikePoint = Point.make 0.0 0.0 1.0

        let mutable incoming =
            {
                LightRay.Ray = ray
                Colour = Colour.White
            }

        let destination =
            Sphere.reflection
                (SphereStyle.Dielectric (1.0<albedo>, Texture.Colour Colour.Green, 1.5<ior>, 1.0<prob>, rand))
                (Point.make 0.0 0.0 2.0)
                1.0
                1.0
                false
                &incoming
                strikePoint

        match destination with
        | ValueNone ->
            incoming.Colour |> shouldEqual Colour.Green
            Point.equal (Ray.origin incoming.Ray) strikePoint |> shouldEqual true

            Vector.equal (Ray.vector incoming.Ray |> UnitVector.scale 1.0) (Ray.vector ray |> UnitVector.scale 1.0)
            |> shouldEqual true
        | ValueSome colour -> failwithf "Absorbed: %+A" colour

    [<Test>]
    let ``Test planeMap`` () =
        // theta required to be in 0,1
        // phi required to be in 0,1
        let property
            (
                ((oX : NormalFloat, oY : NormalFloat, oZ : NormalFloat), (radius : NormalFloat)),
                (theta : float),
                (phi : float)
            )
            : bool
            =
            let centre = Point.make oX.Get oY.Get oZ.Get
            let radius = abs radius.Get
            let point = Sphere.planeMap radius centre theta phi

            let struct (remainingTheta, remainingPhi) =
                Sphere.planeMapInverse radius centre point

            let result = Float.equal theta remainingTheta && Float.equal phi remainingPhi
            result

        let boundedFloat upperBound =
            ArbMap.defaults
            |> ArbMap.generate<NormalFloat>
            |> Gen.map (fun i -> abs i.Get)
            |> Gen.filter (fun i -> i <= upperBound)

        let arb =
            gen {
                let! oX = ArbMap.defaults |> ArbMap.generate<NormalFloat>
                let! oY = ArbMap.defaults |> ArbMap.generate<NormalFloat>
                let! oZ = ArbMap.defaults |> ArbMap.generate<NormalFloat>
                let! radius = ArbMap.defaults |> ArbMap.generate<NormalFloat>
                let! theta = boundedFloat 1.0
                let! phi = boundedFloat 1.0
                return ((oX, oY, oZ), radius), theta, phi
            }
            |> Arb.fromGen

        property |> Prop.forAll arb |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Specific planeMapInverses`` () =
        let sphere = Sphere.planeMapInverse 1.0 (Point.make 0.0 0.0 0.0)
        sphere (Point.make 1.0 0.0 0.0) |> shouldEqual (0.5, 0.5)
        sphere (Point.make -1.0 0.0 0.0) |> shouldEqual (0.0, 0.5)
        sphere (Point.make 0.0 1.0 0.0) |> shouldEqual (0.5, 1.0)
        sphere (Point.make 0.0 -1.0 0.0) |> shouldEqual (0.5, 0.0)
        sphere (Point.make 0.0 0.0 1.0) |> shouldEqual (0.25, 0.5)
        sphere (Point.make 0.0 0.0 -1.0) |> shouldEqual (0.75, 0.5)

    [<Test>]
    let ``Specific planeMaps`` () =
        let sphere = Sphere.planeMap 1.0 (Point.make 0.0 0.0 0.0)
        sphere 0.5 0.5 |> Point.equal (Point.make 1.0 0.0 0.0) |> shouldEqual true
        sphere 0.0 0.5 |> Point.equal (Point.make -1.0 0.0 0.0) |> shouldEqual true
        sphere 0.5 1.0 |> Point.equal (Point.make 0.0 1.0 0.0) |> shouldEqual true
        sphere 0.5 0.0 |> Point.equal (Point.make 0.0 -1.0 0.0) |> shouldEqual true
        sphere 0.25 0.5 |> Point.equal (Point.make 0.0 0.0 1.0) |> shouldEqual true
        sphere 0.75 0.5 |> Point.equal (Point.make 0.0 0.0 -1.0) |> shouldEqual true
