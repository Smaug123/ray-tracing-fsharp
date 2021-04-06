namespace RayTracing

open System
open RayTracing

[<Measure>]
type progress

type Image = Image of Pixel [] []

[<RequireQualifiedAccess>]
module Image =
    let rowCount (Image i) : int = i.Length

    let colCount (Image i) : int = i.[0].Length

[<RequireQualifiedAccess>]
module Vector =
    let dot<'a> (num : Num<'a>) (p1 : Vector<'a>) (p2 : Vector<'a>) : 'a =
        match p1, p2 with
        | Vector p1, Vector p2 ->
            let mutable answer = num.Zero
            for i in 0..p1.Length - 1 do
                answer <- num.Add answer (num.Times p1.[i] p2.[i])
            answer

    let scale<'a> (num : Num<'a>) (scale : 'a) (vec : Vector<'a>) : Vector<'a> =
        match vec with
        | Vector vec ->
            vec
            |> Array.map (fun i -> num.Times scale i)
            |> Vector

    let difference<'a> (num : Num<'a>) (v1 : Vector<'a>) (v2 : Vector<'a>) : Vector<'a> =
        match v1, v2 with
        | Vector v1, Vector v2 ->
            Array.zip v1 v2
            |> Array.map (fun (a, b) -> num.Subtract a b)
            |> Vector

    let unitise<'a> (num : Num<'a>) (vec : Vector<'a>) : Vector<'a> option =
        let dot = dot num vec vec
        match num.Compare dot num.Zero with
        | Equal -> None
        | _ ->
            let factor = dot |> num.Reciprocal |> num.Sqrt
            scale num factor vec
            |> Some

    let normSquared<'a> (num : Num<'a>) (vec : Vector<'a>) : 'a =
        dot num vec vec

    let equal<'a> (num : Num<'a>) (v1 : Vector<'a>) (v2 : Vector<'a>) : bool =
        match v1, v2 with
        | Vector p1, Vector p2 ->
            Array.zip p1 p2
            |> Array.forall (fun (a, b) -> num.Equal a b)

    let rec randomUnit<'a> (num : Num<'a>) (rand : Random) (dimension : int) : Vector<'a> =
        let vector =
            Array.init dimension (fun _ -> num.Subtract (num.TimesInteger 2 (num.RandomBetween01 rand)) num.One)
            |> Vector
            |> unitise num
        match vector with
        | None -> randomUnit num rand dimension
        | Some result -> result
