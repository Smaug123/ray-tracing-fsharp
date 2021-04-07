namespace RayTracing

type Ray =
    {
        Origin : Point
        Vector : Vector
    }

[<RequireQualifiedAccess>]
module Ray =
    let walkAlong (ray : Ray) (magnitude : float) : Point =
        let (Point origin) = ray.Origin
        let (Vector vector) = ray.Vector |> Vector.unitise |> Option.get

        Array.zip origin vector
        |> Array.map (fun (originCoord, directionCoord) -> originCoord + (directionCoord * magnitude))
        |> Point

    let between (p1 : Point) (p2 : Point) : Ray =
        {
            Origin = p1
            Vector = Point.difference p2 p1
        }

    let parallelTo (p1 : Point) (ray : Ray) : Ray =
        {
            Vector = ray.Vector
            Origin = p1
        }

    let liesOn (point : Point) (ray : Ray) : bool =
        match point, ray.Origin, ray.Vector with
        | Point x, Point y, Vector ray ->
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
