namespace RayTracing

type Ray =
    {
        mutable Origin : Point
        mutable Vector : UnitVector
    }

[<RequireQualifiedAccess>]
module Ray =
    let overwriteWithMake (origin : Point) (vector : Vector) (ray : byref<Ray>) : bool =
        let dot = Vector.dot vector vector

        if Float.equal dot 0.0 then
            false
        else

        ray.Origin <- origin

        ray.Vector <-
            let factor = 1.0 / sqrt dot
            Vector.scale factor vector |> UnitVector

        true

    let make' (origin : Point) (vector : Vector) : Ray ValueOption =
        match Vector.unitise vector with
        | ValueNone -> ValueNone
        | ValueSome v ->
            {
                Origin = origin
                Vector = v
            }
            |> ValueSome

    let make (origin : Point) (vector : UnitVector) : Ray =
        {
            Origin = origin
            Vector = vector
        }

    let walkAlong (ray : Ray) (magnitude : float) : Point =
        let (Point (oX, oY, oZ)) = ray.Origin
        let (UnitVector (Vector (vX, vY, vZ))) = ray.Vector

        Point.make (oX + (vX * magnitude)) (oY + (vY * magnitude)) (oZ + (vZ * magnitude))

    let parallelTo (p1 : Point) (ray : Ray) : Ray =
        {
            Vector = ray.Vector
            Origin = p1
        }

    let translateToIntersect (p1 : Point) (ray : Ray) : unit = ray.Origin <- p1

    let liesOn (point : Point) (ray : Ray) : bool =
        match point, ray.Origin, ray.Vector with
        | Point (p1, p2, p3), Point (o1, o2, o3), UnitVector (Vector (r1, r2, r3)) ->
            let t = (p1 - o1) / r1
            let t2 = (p2 - o2) / r2

            if Float.equal t t2 then
                let t3 = (p3 - o3) / r3
                Float.equal t t3
            else
                false

    let inline vector r = r.Vector
    let inline origin r = r.Origin

    let flip (r : Ray) =
        {
            Origin = r.Origin
            Vector =
                let (UnitVector v) = r.Vector
                Vector.scale -1.0 v |> UnitVector
        }

    let flipInPlace (r : Ray) : unit =
        let (UnitVector v) = r.Vector
        r.Vector <- Vector.scale -1.0 v |> UnitVector
