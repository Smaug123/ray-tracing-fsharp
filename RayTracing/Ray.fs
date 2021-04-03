namespace RayTracing

type Ray<'a> =
    {
        Origin : Point<'a>
        Vector : Point<'a>
    }

[<RequireQualifiedAccess>]
module Ray =
    let walkAlong<'a> (num : Num<'a>) (ray : Ray<'a>) (magnitude : 'a) : Point<'a> =
        let (Point origin) = ray.Origin
        let (Point vector) = ray.Vector

        Array.zip origin vector
        |> Array.map (fun (originCoord, directionCoord) -> num.Add originCoord (num.Times directionCoord magnitude))
        |> Point

    let between<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : Ray<'a> =
        {
            Origin = p1
            Vector = Point.difference num p2 p1
        }

    let parallelTo<'a> (p1 : Point<'a>) (ray : Ray<'a>) : Ray<'a> =
        {
            Vector = ray.Vector
            Origin = p1
        }

    let liesOn<'a> (num : 'a Num) (point : Point<'a>) (ray : Ray<'a>) : bool =
        match point, ray.Origin, ray.Vector with
        | Point x, Point y, Point ray ->
            let rec go (state : 'a option) (i : int) =
                if i >= x.Length then state else
                let d = x.[i]
                let x = y.[i]
                let r = ray.[i]
                match state with
                | None -> go (Some (num.Divide (num.Subtract d x) r)) (i + 1)
                | Some prevT ->
                    let t = num.Divide (num.Subtract d x) r
                    if num.Equal prevT t then go (Some prevT) (i + 1) else None

            go None 0
            |> Option.isSome
