namespace RayTracing

/// An n-dimensional point.
type Point<'a> = | Point of 'a array

[<RequireQualifiedAccess>]
module Point =
    let difference<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : Point<'a> =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.map (fun (a, b) -> num.Subtract a b)
            |> Point

    let normSquared<'a> (num : Num<'a>) (p : Point<'a>) : 'a =
        match p with
        | Point p ->
            p
            |> Array.fold (fun s p -> num.Add (num.Times p p) s) num.Zero