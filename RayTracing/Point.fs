namespace RayTracing

/// An n-dimensional point.
[<NoEquality ; NoComparison>]
type Point<'a> = Point of 'a array

[<NoEquality ; NoComparison>]
type Vector<'a> = Vector of 'a array

[<RequireQualifiedAccess>]
module Point =
    let difference<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : Vector<'a> =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.map (fun (a, b) -> num.Subtract a b)
            |> Vector

    let sum<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : Point<'a> =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.map (fun (a, b) -> num.Add a b)
            |> Point

    let normSquared<'a> (num : Num<'a>) (p : Point<'a>) : 'a =
        match p with
        | Point p ->
            p
            |> Array.fold (fun s p -> num.Add (num.Times p p) s) num.Zero

    let equal<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : bool =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.forall (fun (a, b) -> num.Equal a b)

    let add<'a> (num : Num<'a>) (v1 : Point<'a>) (v2 : Point<'a>) : Point<'a> =
        match v1, v2 with
        | Point v1, Point v2 ->
            Array.zip v1 v2
            |> Array.map (fun (a, b) -> num.Add a b)
            |> Point

