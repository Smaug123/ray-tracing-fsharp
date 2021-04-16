namespace RayTracing

open System.Runtime.CompilerServices

/// An n-dimensional point.
/// We don't let you compare these for equality, because floats are hard.
[<NoEquality ; NoComparison ; Struct ; IsReadOnly>]
type Point =
    private
    | Point of struct(float * float * float)

[<NoEquality ; NoComparison ; Struct ; IsReadOnly>]
type Vector =
    private
    | Vector of struct(float * float * float)

[<Struct ; IsReadOnly ; NoEquality ; NoComparison>]
type UnitVector = | UnitVector of Vector

[<RequireQualifiedAccess>]
module Vector =
    let dot (Vector (x, y, z)) (Vector (a, b, c)) : float =
        x * a + y * b + z * c

    let sum (Vector (a, b, c)) (Vector (d, e, f)) =
        Vector (a + d, b + e, c + f)

    let scale (scale : float) (vec : Vector) : Vector =
        match vec with
        | Vector (a, b, c) ->
            Vector (scale * a, scale * b, scale * c)

    let difference (Vector (a, b, c)) (Vector (x, y, z)) : Vector =
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

    let equal (Vector (a, b, c)) (Vector (x, y, z)) : bool =
        Float.equal a x && Float.equal b y && Float.equal c z

    let make (x : float) (y : float) (z : float) =
        Vector (x, y, z)

    let cross (Vector (x, y, z)) (Vector (a, b, c)) : Vector =
        make (y * c - z * b) (z * a - x * c) (x * b - a * y)

[<RequireQualifiedAccess>]
module UnitVector =
    let rec random (floatProducer : FloatProducer) (dimension : int) : UnitVector =
        let struct(rand1, rand2, rand3) = floatProducer.GetThree ()
        let x = (2.0 * rand1) - 1.0
        let y = (2.0 * rand2) - 1.0
        let z = (2.0 * rand3) - 1.0
        Vector.make x y z
        |> Vector.unitise
        |> function
            | None -> random floatProducer dimension
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

[<RequireQualifiedAccess>]
module Point =

    let xCoordinate (Point (x, _, _)) = x

    let sum (Point (a, b, c)) (Point (x, y, z)) : Point =
        Point (a + x, b + y, c + z)

    let differenceToThenFrom (Point (a, b, c)) (Point (x, y, z)) : Vector =
        Vector (a - x, b - y, c - z)

    let equal (Point (a, b, c)) (Point (x, y, z)) : bool =
        Float.equal a x && Float.equal b y && Float.equal c z

    let make (x : float) (y : float) (z : float) = Point (x, y, z)
    let inline dimension _ = 3
