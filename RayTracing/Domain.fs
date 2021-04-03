namespace RayTracing

[<Measure>]
type progress

[<Struct>]
type Pixel =
    {
        Red : byte
        Green : byte
        Blue : byte
    }
    static member White =
        {
            Red = 0uy
            Green = 0uy
            Blue = 0uy
        }

[<RequireQualifiedAccess>]
module Pixel =
    let average (s : Pixel seq) : Pixel =
        use e = s.GetEnumerator ()
        if not (e.MoveNext ()) then failwith "Input sequence was empty when averaging pixels"
        let mutable count = 1.0
        let mutable r = e.Current.Red |> float
        let mutable g = e.Current.Green |> float
        let mutable b = e.Current.Blue |> float
        while e.MoveNext () do
            let newCount = count + 1.0
            r <- (r * count + float e.Current.Red) / newCount
            g <- (g * count + float e.Current.Green) / newCount
            b <- (b * count + float e.Current.Blue) / newCount
            count <- newCount
        {
            Red = byte r
            Green = byte g
            Blue = byte b
        }


type Image = Image of Pixel [] []

[<RequireQualifiedAccess>]
module Image =
    let rowCount (Image i) : int = i.Length

    let colCount (Image i) : int = i.[0].Length

[<RequireQualifiedAccess>]
module Vector =
    let dot<'a> (num : Num<'a>) (p1 : Point<'a>) (p2 : Point<'a>) : 'a =
        match p1, p2 with
        | Point p1, Point p2 ->
            let mutable answer = num.Zero
            for i in 0..p1.Length - 1 do
                answer <- num.Add answer (num.Times p1.[i] p2.[i])
            answer
