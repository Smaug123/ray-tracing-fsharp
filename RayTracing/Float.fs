namespace RayTracing

open System

type Comparison =
    | Greater
    | Equal
    | Less

type FloatProducer (rand : Random) =
    let locker = obj ()

    member _.Get () : float =
        lock locker (fun () ->
            rand.NextDouble ()
        )

    member _.GetTwo () : struct(float * float) =
        lock locker (fun () ->
            rand.NextDouble (), rand.NextDouble()
        )

    member _.GetThree () : struct(float * float * float) =
        lock locker (fun () ->
            rand.NextDouble (), rand.NextDouble(), rand.NextDouble()
        )


[<RequireQualifiedAccess>]
module Float =

    let Pi = acos -1.0
    let tolerance = 0.00000001

    let inline equal (a : float) (b : float) : bool =
        abs (a - b) < tolerance

    let inline compare (a : float) (b : float) : Comparison =
        if abs (a - b) < tolerance then Comparison.Equal
        elif a < b then Comparison.Less
        else Comparison.Greater

    let sortInPlaceBy<'b> (proj : 'b -> float) (a : 'b array) : 'b array =
        for i in 0..a.Length - 2 do
            for j in i+1..a.Length - 1 do
                match compare (proj a.[i]) (proj a.[j]) with
                | Greater ->
                    let tmp = a.[j]
                    a.[j] <- a.[i]
                    a.[i] <- tmp
                | _ -> ()
        a

