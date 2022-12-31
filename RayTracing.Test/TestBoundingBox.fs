namespace RayTracing.Test

open FsCheck
open RayTracing
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestBoundingBox =

    let delta = 0.00000001

    let sort (x1 : float) (x2 : float) =
        min x1 x2, (if x1 = x2 then x1 + (delta / 2.0) else max x1 x2)

    [<TestCase true>]
    [<TestCase false>]
    let ``Bounding box on the left doesn't intersect ray to the right`` (negate : bool) =
        let ray =
            Ray.make
                (Point.make ((if negate then (fun x -> -x) else id) delta) 0.0 0.0)
                (Vector.make (if negate then -1.0 else 1.0) 0.0 0.0
                 |> Vector.unitise
                 |> Option.get)

        let property
            (x : NormalFloat)
            (y : NormalFloat)
            (z : NormalFloat)
            (x' : NormalFloat)
            (y' : NormalFloat)
            (z' : NormalFloat)
            : bool
            =
            let x1, x2 =
                sort (if negate then abs x.Get else -abs x.Get) (if negate then abs x'.Get else -abs x'.Get)

            let y1, y2 = sort y.Get y'.Get
            let z1, z2 = sort z.Get z'.Get
            let box = BoundingBox.make (Point.make x1 y1 z1) (Point.make x2 y2 z2)
            BoundingBox.hits (BoundingBox.inverseDirections ray) ray box = false

        Check.QuickThrowOnFailure property

    [<TestCase true>]
    [<TestCase false>]
    let ``Bounding box on the top doesn't intersect ray to the bottom`` (negate : bool) =
        let ray =
            Ray.make
                (Point.make 0.0 ((if negate then (fun x -> -x) else id) delta) 0.0)
                (Vector.make 0.0 (if negate then -1.0 else 1.0) 0.0
                 |> Vector.unitise
                 |> Option.get)

        let property
            (x : NormalFloat)
            (y : NormalFloat)
            (z : NormalFloat)
            (x' : NormalFloat)
            (y' : NormalFloat)
            (z' : NormalFloat)
            : bool
            =
            let y1, y2 =
                sort (if negate then abs y.Get else -abs y.Get) (if negate then abs y'.Get else -abs y'.Get)

            let x1, x2 = sort x.Get x'.Get
            let z1, z2 = sort z.Get z'.Get
            let box = BoundingBox.make (Point.make x1 y1 z1) (Point.make x2 y2 z2)
            BoundingBox.hits (BoundingBox.inverseDirections ray) ray box = false

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Bounding box forward, ray going backward, case 1`` () =
        let ray =
            Ray.make (Point.make 0.0 0.0 delta) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get)

        let z1, z2 = sort (-abs 0.0) (-abs 0.0)
        let x1, x2 = sort 0.0 0.0
        let y1, y2 = sort 0.0 1.0
        let box = BoundingBox.make (Point.make x1 y1 z1) (Point.make x2 y2 z2)
        let result = BoundingBox.hits (BoundingBox.inverseDirections ray) ray box
        result |> shouldEqual false

    [<TestCase true>]
    [<TestCase false>]
    let ``Bounding box forward doesn't intersect ray going backward`` (negate : bool) =
        let ray =
            Ray.make
                (Point.make 0.0 0.0 ((if negate then (fun x -> -x) else id) delta))
                (Vector.make 0.0 0.0 (if negate then -1.0 else 1.0)
                 |> Vector.unitise
                 |> Option.get)

        let property
            (x : NormalFloat)
            (y : NormalFloat)
            (z : NormalFloat)
            (x' : NormalFloat)
            (y' : NormalFloat)
            (z' : NormalFloat)
            : bool
            =
            let z1, z2 =
                sort (if negate then abs z.Get else -abs z.Get) (if negate then abs z'.Get else -abs z'.Get)

            let x1, x2 = sort x.Get x'.Get
            let y1, y2 = sort y.Get y'.Get
            let box = BoundingBox.make (Point.make x1 y1 z1) (Point.make x2 y2 z2)
            let result = BoundingBox.hits (BoundingBox.inverseDirections ray) ray box
            result = false

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Bounding box forward does intersect ray going forward`` () =
        let ray =
            Ray.make (Point.make 0.0 0.0 0.0) (Vector.make 0.0 0.0 1.0 |> Vector.unitise |> Option.get)

        let box = BoundingBox.make (Point.make -1.0 -1.0 -1.0) (Point.make 1.0 1.0 1.0)

        BoundingBox.hits (BoundingBox.inverseDirections ray) ray box |> shouldEqual true
