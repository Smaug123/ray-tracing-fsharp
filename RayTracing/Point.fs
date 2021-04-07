namespace RayTracing

open System

/// An n-dimensional point.
/// We don't let you compare these for equality, because floats are hard.
[<NoEquality ; NoComparison>]
type Point = Point of float array

[<NoEquality ; NoComparison>]
type Vector = Vector of float array

type UnitVector = UnitVector of Vector

[<RequireQualifiedAccess>]
module Vector =
    let dot (p1 : Vector) (p2 : Vector) : float =
        match p1, p2 with
        | Vector p1, Vector p2 ->
            let mutable answer = 0.0
            for i in 0..p1.Length - 1 do
                answer <- answer + (p1.[i] * p2.[i])
            answer

    let scale (scale : float) (vec : Vector) : Vector =
        match vec with
        | Vector vec ->
            vec
            |> Array.map (fun i -> scale * i)
            |> Vector

    let difference (v1 : Vector) (v2 : Vector) : Vector =
        match v1, v2 with
        | Vector v1, Vector v2 ->
            let answer = Array.zeroCreate v1.Length
            for i in 0..answer.Length - 1 do
                answer.[i] <- v1.[i] - v2.[i]
            answer
            |> Vector

    let unitise (vec : Vector) : UnitVector option =
        let dot = dot vec vec
        if Float.equal dot 0.0 then None else
        let factor = 1.0 / sqrt dot
        scale factor vec
        |> UnitVector
        |> Some

    let normSquared (vec : Vector) : float =
        dot vec vec

    let equal (v1 : Vector) (v2 : Vector) : bool =
        match v1, v2 with
        | Vector p1, Vector p2 ->
            let rec go (i : int) =
                if i >= p1.Length then true else
                if Float.equal p1.[i] p2.[i] then go (i + 1) else false
            go 0

[<RequireQualifiedAccess>]
module UnitVector =
    let rec random (rand : Random) (dimension : int) : UnitVector =
        let vector =
            Array.init dimension (fun _ -> (2.0 * Float.random rand) - 1.0)
            |> Vector
            |> Vector.unitise
        match vector with
        | None -> random rand dimension
        | Some result -> result

    let dot (UnitVector a) (UnitVector b) = Vector.dot a b
    let dot' (UnitVector a) (b : Vector) = Vector.dot a b
    let difference (UnitVector v1) (UnitVector v2) = Vector.difference v1 v2
    let difference' (UnitVector v1) (v2 : Vector) = Vector.difference v1 v2
    let scale (scale : float) (UnitVector vec) = Vector.scale scale vec

    let basis (dimension : int) : UnitVector [] =
        Array.init dimension (fun i ->
            Array.init dimension (fun j ->
                if i = j then 1.0 else 0.0
            )
            |> Vector
            |> UnitVector
        )

[<RequireQualifiedAccess>]
module Point =
    let difference (p1 : Point) (p2 : Point) : Vector =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.map (fun (a, b) -> a - b)
            |> Vector

    let sum (p1 : Point) (p2 : Point) : Point =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.map (fun (a, b) -> a + b)
            |> Point

    let normSquared (p : Point) : float =
        match p with
        | Point p ->
            p
            |> Array.fold (fun s p -> (p * p) + s) 0.0

    let equal (p1 : Point) (p2 : Point) : bool =
        match p1, p2 with
        | Point p1, Point p2 ->
            Array.zip p1 p2
            |> Array.forall (fun (a, b) -> Float.equal a b)

    let add (v1 : Point) (v2 : Point) : Point =
        match v1, v2 with
        | Point v1, Point v2 ->
            Array.zip v1 v2
            |> Array.map (fun (a, b) -> a + b)
            |> Point
