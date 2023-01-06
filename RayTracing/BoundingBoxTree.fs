namespace RayTracing

type BoundingBoxTree =
    | Leaf of hittable : Hittable * BoundingBox
    | Branch of left : BoundingBoxTree * right : BoundingBoxTree * all : BoundingBox

[<RequireQualifiedAccess>]
module BoundingBoxTree =
    let make (boxes : (Hittable * BoundingBox) array) : BoundingBoxTree ValueOption =
        if boxes.Length = 0 then
            ValueNone
        else

            let rec go (boxes : (Hittable * BoundingBox) array) =
                let boundAll = BoundingBox.merge (boxes |> Array.map snd) |> ValueOption.get

                if boxes.Length = 1 then
                    Leaf boxes.[0]
                else if boxes.Length = 2 then
                    Branch (Leaf boxes.[0], Leaf boxes.[1], boundAll)
                else

                    let choices =
                        Array.init
                            3
                            (fun axis ->
                                let boxes = boxes |> Array.sortBy (fun (_, b) -> Point.coordinate axis b.Min)
                                let leftHalf = boxes.[0 .. boxes.Length / 2]
                                let rightHalf = boxes.[(boxes.Length / 2) + 1 ..]
                                let leftBound = leftHalf |> Array.map snd |> BoundingBox.merge |> ValueOption.get
                                let rightBound = rightHalf |> Array.map snd |> BoundingBox.merge |> ValueOption.get
                                (leftHalf, leftBound), (rightHalf, rightBound)
                            )

                    let (leftHalf, _), (rightHalf, _) =
                        choices
                        |> Array.minBy (fun ((_, leftBound), (_, rightBound)) ->
                            (BoundingBox.volume leftBound) + (BoundingBox.volume rightBound)
                        )

                    Branch (go leftHalf, go rightHalf, boundAll)

            go boxes |> ValueSome
