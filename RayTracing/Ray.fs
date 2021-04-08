namespace RayTracing

type Ray =
    private
        {
            Origin : Point
            Vector : UnitVector
        }

[<RequireQualifiedAccess>]
module Ray =
    let make' (origin : Point) (vector : Vector) : Ray option =
        match Vector.unitise vector with
        | None -> None
        | Some v ->
            {
                Origin = origin
                Vector = v
            }
            |> Some

    let make (origin : Point) (vector : UnitVector) : Ray =
        {
            Origin = origin
            Vector = vector
        }

    let walkAlong (ray : Ray) (magnitude : float) : Point =
        let (Point origin) = ray.Origin
        let (UnitVector (Vector vector)) = ray.Vector

        Array.init origin.Length (fun i ->
            origin.[i] + (vector.[i] * magnitude)
        )
        |> Point

    let parallelTo (p1 : Point) (ray : Ray) : Ray =
        {
            Vector = ray.Vector
            Origin = p1
        }

    let liesOn (point : Point) (ray : Ray) : bool =
        match point, ray.Origin, ray.Vector with
        | Point x, Point y, UnitVector (Vector ray) ->
            let rec go (state : float option) (i : int) =
                if i >= x.Length then state else
                let d = x.[i]
                let x = y.[i]
                let r = ray.[i]
                match state with
                | None -> go (Some ((d - x) / r)) (i + 1)
                | Some prevT ->
                    let t = (d - x) / r
                    if Float.equal prevT t then go (Some prevT) (i + 1) else None

            go None 0
            |> Option.isSome

    let vector r = r.Vector
    let origin r = r.Origin

    let flip (r : Ray) =
        {
            Origin = r.Origin
            Vector =
                let (UnitVector v) = r.Vector
                Vector.scale -1.0 v
                |> UnitVector
        }
