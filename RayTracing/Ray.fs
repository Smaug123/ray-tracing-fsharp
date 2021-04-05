namespace RayTracing

type Ray<'a> =
    {
        Origin : Point<'a>
        Vector : Vector<'a>
    }

[<RequireQualifiedAccess>]
module Ray =
    let walkAlong<'a> (num : Num<'a>) (ray : Ray<'a>) (magnitude : 'a) : Point<'a> =
        let (Point origin) = ray.Origin
        let (Vector vector) = ray.Vector |> Vector.unitise num |> Option.get

        Array.zip origin vector
        |> Array.map (fun (originCoord, directionCoord) -> num.Add originCoord (num.Times directionCoord magnitude))
        |> Point

    let between<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : Ray<'a> =
        {
            Origin = p1
            Vector = Point.difference num p2 p1
        }

    /// Given two rays from the same point, what is the angle between them?
    let angle<'a> (num : Num<'a>) (r1 : Ray<'a>) (r2 : Ray<'a>) : 'a Radian =
        // a.b = |a| |b| cos theta
        let v1 = walkAlong num { r1 with Origin = r2.Origin } num.One
        let v2 = walkAlong num r2 num.One
        let (Radian answer) = num.ArcCos (Vector.dot num (Point.difference num v1 r2.Origin) (Point.difference num v2 r2.Origin))
        match num.Compare (num.Double answer) num.Pi with
        | Greater ->
            num.Subtract num.Pi answer
        | _ ->
            answer
        |> Radian

    let parallelTo<'a> (p1 : Point<'a>) (ray : Ray<'a>) : Ray<'a> =
        {
            Vector = ray.Vector
            Origin = p1
        }

    let liesOn<'a> (num : 'a Num) (point : Point<'a>) (ray : Ray<'a>) : bool =
        match point, ray.Origin, ray.Vector with
        | Point x, Point y, Vector ray ->
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
