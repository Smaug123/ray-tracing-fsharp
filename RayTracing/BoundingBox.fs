namespace RayTracing

[<Struct ; NoComparison ; NoEquality>]
type BoundingBox =
    {
        Min : Point
        Max : Point
    }

[<RequireQualifiedAccess>]
module BoundingBox =

    let volume (box : BoundingBox) =
        (Point.coordinate 0 box.Max - Point.coordinate 0 box.Min) *
        (Point.coordinate 1 box.Max - Point.coordinate 1 box.Min) *
        (Point.coordinate 2 box.Max - Point.coordinate 2 box.Min)

    let make (min : Point) (max : Point) =
        {
            Min = min
            Max = max
        }


    let inverseDirections (ray : Ray) =
        struct(1.0 / (Ray.vector ray |> UnitVector.coordinate 0), 1.0 / (Ray.vector ray |> UnitVector.coordinate 1), 1.0 / (Ray.vector ray |> UnitVector.coordinate 2))

    let hits (struct(invX, invY, invZ)) { Ray.Origin = Point (x, y, z) ; Vector = UnitVector (Vector (dx, dy, dz))} (box : BoundingBox) : bool =
        // The line is (x, y, z) + t (dx, dy, dz)
        // The line goes through the cuboid iff it passes through the interval in each component:
        //   there is t such that boxMin.X <= x + t dx <= boxMax.X,
        // and moreover the acceptable t brackets all overlap.
        // That is,
        //  boxMin.X - x <= t dx <= boxMax.X - x
        let mutable tMin = -infinity
        let mutable tMax = infinity

        let bailOut =
            let mutable t0 = (Point.coordinate 0 box.Min - x) * invX
            let mutable t1 = (Point.coordinate 0 box.Max - x) * invX
            if invX < 0.0 then
                let tmp = t1
                t1 <- t0
                t0 <- tmp

            tMin <- if t0 > tMin then t0 else tMin
            tMax <- if t1 < tMax then t1 else tMax

            tMax < tMin || 0.0 >= tMax

        if bailOut then false else

        let bailOut =
            let mutable t0 = (Point.coordinate 1 box.Min - y) * invY
            let mutable t1 = (Point.coordinate 1 box.Max - y) * invY

            if invY < 0.0 then
                let tmp = t1
                t1 <- t0
                t0 <- tmp

            tMin <- if t0 > tMin then t0 else tMin
            tMax <- if t1 < tMax then t1 else tMax

            tMax < tMin || 0.0 >= tMax

        if bailOut then false else

        let mutable t0 = (Point.coordinate 2 box.Min - z) * invZ
        let mutable t1 = (Point.coordinate 2 box.Max - z) * invZ

        if invZ < 0.0 then
            let tmp = t1
            t1 <- t0
            t0 <- tmp

        tMin <- if t0 > tMin then t0 else tMin
        tMax <- if t1 < tMax then t1 else tMax
        tMax >= tMin && tMax >= 0.0

    let mergeTwo (i : BoundingBox) (j : BoundingBox) : BoundingBox =
        {
            Min =
                Point.make
                    (min (Point.coordinate 0 i.Min) (Point.coordinate 0 j.Min))
                    (min (Point.coordinate 1 i.Min) (Point.coordinate 1 j.Min))
                    (min (Point.coordinate 2 i.Min) (Point.coordinate 2 j.Min))
            Max =
                Point.make
                    (max (Point.coordinate 0 i.Max) (Point.coordinate 0 j.Max))
                    (max (Point.coordinate 1 i.Max) (Point.coordinate 1 j.Max))
                    (max (Point.coordinate 2 i.Max) (Point.coordinate 2 j.Max))
        }

    let merge (boxes : BoundingBox []) : BoundingBox option =
        if boxes.Length = 0 then None else
        boxes
        |> Array.reduce mergeTwo
        |> Some