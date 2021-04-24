namespace RayTracing.Test

open System
open RayTracing
open NUnit.Framework
open FsCheck
open FsUnitTyped

[<TestFixture>]
module TestSphere =

    [<Test>]
    let ``Point at distance r from centre lies on sphere`` () =
        let property (centre : Point, radius : float, point : Point) : bool =
            let radius = abs radius
            let sphere = Sphere.make (SphereStyle.PureReflection (1.0<albedo>, Texture.Colour Colour.White)) centre radius
            Sphere.liesOn point sphere


        let gen : Gen<Point * float * Point> =
            gen {
                let! centre = TestUtils.pointGen
                let! radius = Arb.generate<NormalFloat> |> Gen.map NormalFloat.op_Explicit
                let! theta =
                    Arb.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit
                let! phi =
                    Arb.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit

                let surfacePoint =
                    Point.make
                        (radius * cos phi * sin theta)
                        (radius * sin phi * sin theta)
                        (radius * cos theta)
                    |> fun p -> Point.sum centre p
                return centre, radius, surfacePoint
            }

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Glass sphere perfectly reflects against the edge`` () =
        let rand = Random () |> FloatProducer
        let ray = Ray.make (Point.make 0.0 0.0 0.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get)
        let strikePoint = Point.make 0.0 0.0 1.0
        let destination =
            Sphere.reflection
                (SphereStyle.Glass (1.0<albedo>, Texture.Colour Colour.Green, 1.5<ior>, rand))
                (Point.make 0.0 1.0 1.0)
                1.0
                1.0
                false
                { LightRay.Ray = ray ; Colour = Colour.White }
                strikePoint

        match destination with
        | Continues onward ->
            onward.Colour |> shouldEqual Colour.Green
            Point.equal (Ray.origin onward.Ray) strikePoint |> shouldEqual true
            Vector.equal (Ray.vector onward.Ray |> UnitVector.scale 1.0) (Ray.vector ray |> UnitVector.scale 1.0) |> shouldEqual true
        | Absorbs colour ->
            failwithf "Absorbed: %+A" colour

    [<Test>]
    let ``Glass sphere perfectly refracts through the middle`` () =
        let rand = Random () |> FloatProducer
        let ray = Ray.make (Point.make 0.0 0.0 0.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get)
        let strikePoint = Point.make 0.0 0.0 1.0
        let destination =
            Sphere.reflection
                (SphereStyle.Glass (1.0<albedo>, Texture.Colour Colour.Green, 1.5<ior>, rand))
                (Point.make 0.0 0.0 2.0)
                1.0
                1.0
                false
                { LightRay.Ray = ray ; Colour = Colour.White }
                strikePoint

        match destination with
        | Continues onward ->
            onward.Colour |> shouldEqual Colour.Green
            Point.equal (Ray.origin onward.Ray) strikePoint |> shouldEqual true
            Vector.equal (Ray.vector onward.Ray |> UnitVector.scale 1.0) (Ray.vector ray |> UnitVector.scale 1.0) |> shouldEqual true
        | Absorbs colour ->
            failwithf "Absorbed: %+A" colour

    [<Test>]
    let ``Dielectric sphere refracts when incoming ray `` () =
        let rand = Random () |> FloatProducer
        let ray = Ray.make (Point.make 0.0 0.0 0.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get)
        let strikePoint = Point.make 0.0 0.0 1.0
        let destination =
            Sphere.reflection
                (SphereStyle.Dielectric (1.0<albedo>, Texture.Colour Colour.Green, 1.5<ior>, 1.0<prob>, rand))
                (Point.make 0.0 0.0 2.0)
                1.0
                1.0
                false
                { LightRay.Ray = ray ; Colour = Colour.White }
                strikePoint

        match destination with
        | Continues onward ->
            onward.Colour |> shouldEqual Colour.Green
            Point.equal (Ray.origin onward.Ray) strikePoint |> shouldEqual true
            Vector.equal (Ray.vector onward.Ray |> UnitVector.scale 1.0) (Ray.vector ray |> UnitVector.scale 1.0) |> shouldEqual true
        | Absorbs colour ->
            failwithf "Absorbed: %+A" colour

    [<Test>]
    let ``Test planeMap`` () =
        // theta required to be in 0,1
        // phi required to be in 0,1
        let property (((oX : NormalFloat, oY : NormalFloat, oZ : NormalFloat), (radius : NormalFloat)), (theta : float), (phi : float)) : bool =
            let centre = Point.make oX.Get oY.Get oZ.Get
            let radius = abs radius.Get
            let point = Sphere.planeMap radius centre theta phi
            let struct(remainingTheta, remainingPhi) = Sphere.planeMapInverse radius centre point
            let result = Float.equal theta remainingTheta && Float.equal phi remainingPhi
            result

        let boundedFloat upperBound =
            Arb.generate<NormalFloat> |> Gen.map (fun i -> abs i.Get) |> Gen.filter (fun i -> i <= upperBound)

        let arb =
            gen {
                let! oX = Arb.generate<NormalFloat>
                let! oY = Arb.generate<NormalFloat>
                let! oZ = Arb.generate<NormalFloat>
                let! radius = Arb.generate<NormalFloat>
                let! theta = boundedFloat 1.0
                let! phi = boundedFloat 1.0
                return ((oX, oY, oZ), radius), theta, phi
            }
            |> Arb.fromGen

        property
        |> Prop.forAll arb
        |> Check.QuickThrowOnFailure