namespace RayTracing

open System

/// An n-dimensional point.
/// We don't let you compare these for equality, because floats are hard.
[<NoEquality ; NoComparison>]
[<Struct>]
type Point =
    private
    | Point of struct(float * float * float)

[<NoEquality ; NoComparison>]
type Vector =
    private
    | Vector of struct(float * float * float)

type UnitVector = UnitVector of Vector

[<RequireQualifiedAccess>]
module Vector =
    let dot (p1 : Vector) (p2 : Vector) : float =
        match p1, p2 with
        | Vector (x, y, z), Vector (a, b, c) ->
            x * a + y * b + z * c

    let scale (scale : float) (vec : Vector) : Vector =
        match vec with
        | Vector (a, b, c) ->
            Vector (scale * a, scale * b, scale * c)

    let difference (v1 : Vector) (v2 : Vector) : Vector =
        match v1, v2 with
        | Vector (a, b, c), Vector (x, y, z) ->
            Vector (a - x, b - y, c - z)

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
        | Vector (a, b, c), Vector (x, y, z) ->
            Float.equal a x && Float.equal b y && Float.equal c z

    let make (x : float) (y : float) (z : float) =
        Vector (x, y, z)

[<RequireQualifiedAccess>]
module UnitVector =
    let rec random (rand : Random) (dimension : int) : UnitVector =
        let x = (2.0 * Float.random rand) - 1.0
        let y = (2.0 * Float.random rand) - 1.0
        let z = (2.0 * Float.random rand) - 1.0
        Vector.make x y z
        |> Vector.unitise
        |> function
            | None -> random rand dimension
            | Some result -> result

    let inline dot (UnitVector a) (UnitVector b) = Vector.dot a b
    let inline dot' (UnitVector a) (b : Vector) = Vector.dot a b
    let inline difference (UnitVector v1) (UnitVector v2) = Vector.difference v1 v2
    let inline difference' (UnitVector v1) (v2 : Vector) = Vector.difference v1 v2
    let inline scale (scale : float) (UnitVector vec) = Vector.scale scale vec
    let inline flip (UnitVector vec) = UnitVector (Vector.scale -1.0 vec)

    let basis (_ : int) : UnitVector [] =
        [|
            Vector (1.0, 0.0, 0.0) |> UnitVector
            Vector (0.0, 1.0, 0.0) |> UnitVector
            Vector (0.0, 0.0, 1.0) |> UnitVector
        |]

[<Struct>]
type Difference =
    {
        EndUpAt : Point
        ComeFrom : Point
    }

[<RequireQualifiedAccess>]
module Point =

    let xCoordinate (Point (x, _, _)) = x

    let sum (p1 : Point) (p2 : Point) : Point =
        match p1, p2 with
        | Point (a, b, c), Point (x, y, z) ->
            Point (a + x, b + y, c + z)

    let difference { EndUpAt = p1 ; ComeFrom = p2 } : Vector =
        match p1, p2 with
        | Point (a, b, c), Point (x, y, z) ->
            Vector (a - x, b - y, c - z)

    let equal (p1 : Point) (p2 : Point) : bool =
        match p1, p2 with
        | Point (a, b, c), Point (x, y, z) ->
            Float.equal a x && Float.equal b y && Float.equal c z

    let make (x : float) (y : float) (z : float) = Point (x, y, z)
    let inline dimension _ = 3

