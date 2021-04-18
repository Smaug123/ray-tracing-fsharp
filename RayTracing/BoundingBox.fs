namespace RayTracing

[<Struct ; NoComparison ; NoEquality>]
type BoundingBox =
    {
        Min : Point
        Max : Point
    }

[<RequireQualifiedAccess>]
module BoundingBox =

    let make (min : Point) (max : Point) =
        {
            Min = min
            Max = max
        }

    let hits (r : Ray) (box : BoundingBox) : bool =
        let mutable answer = true
        let mutable dimension = 0
        while answer && dimension < 3 do
            let inverseDirection = 1.0 / (Ray.vector r |> UnitVector.coordinate dimension)
            let coord = Ray.origin r |> Point.coordinate dimension
            let mutable t0 = (Point.coordinate dimension box.Min - coord) * inverseDirection
            let mutable t1 = (Point.coordinate dimension box.Max - coord) * inverseDirection

            if Float.compare inverseDirection 0.0 = Less then
                let tmp = t0
                t0 <- t1
                t1 <- tmp

            if t1 < t0 then answer <- false else dimension <- dimension + 1
        answer

    let mergeTwo (i : BoundingBox) (j : BoundingBox) : BoundingBox =
        {
            Min =
                Point.make
                    (min (Point.coordinate 0 i.Min) (Point.coordinate 0 j.Min))
                    (min (Point.coordinate 1 i.Min) (Point.coordinate 1 j.Min))
                    (min (Point.coordinate 2 i.Min) (Point.coordinate 2 j.Min))
            Max =
                Point.make
                    (max (Point.coordinate 0 i.Max) (Point.coordinate 0 j.Max))
                    (max (Point.coordinate 1 i.Max) (Point.coordinate 1 j.Max))
                    (max (Point.coordinate 2 i.Max) (Point.coordinate 2 j.Max))
        }

    let merge (boxes : BoundingBox []) : BoundingBox option =
        if boxes.Length = 0 then None else
        boxes
        |> Array.reduce mergeTwo
        |> Some

type BoundingBoxTree<'a when 'a : not struct> =
    | Leaf of hittable : 'a * BoundingBox
    | Branch of axis : int * left : BoundingBoxTree<'a> * right : BoundingBoxTree<'a> * all : BoundingBox

[<RequireQualifiedAccess>]
module BoundingBoxTree =
    let make<'a when 'a : not struct> (rand : System.Random) (boxes : ('a * BoundingBox) array) : BoundingBoxTree<'a> option =
        if boxes.Length = 0 then None else

        let rec go (boxes : ('a * BoundingBox) array) =
            let boundAll =
                BoundingBox.merge (boxes |> Array.map snd) |> Option.get

            if boxes.Length = 1 then Leaf boxes.[0] else
            if boxes.Length = 2 then Branch (0, Leaf boxes.[0], Leaf boxes.[1], boundAll) else

            let axis = abs (rand.Next ()) % 3
            let comparer (box : BoundingBox) =
                Float.compare (Point.coordinate axis box.Min) (Point.coordinate axis box.Max) = Less
            let boxes =
                boxes
                |> Array.sortBy (snd >> comparer)
            let leftHalf = boxes.[0..boxes.Length / 2]
            let rightHalf = boxes.[(boxes.Length / 2) + 1..]
            Branch (axis, go leftHalf, go rightHalf, boundAll)

        go boxes
        |> Some