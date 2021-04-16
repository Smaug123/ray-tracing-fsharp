namespace RayTracing

open System
open System.Threading

[<NoComparison>]
type Comparison =
    | Greater
    | Equal
    | Less

[<RequireQualifiedAccess>]
module private FloatProducer =
    let inline generateInt32 (x : byref<int>) (y : byref<int>) (z : byref<int>) (w : byref<int>) =
        let t = x ^^^ (x <<< 11)
        x <- y
        y <- z
        z <- w
        w <- w ^^^ (w >>> 19) ^^^ (t ^^^ (t >>> 8))
        w

    let inline toInt (w : int) =
        let highest = (w &&& 0xFF)
        let secondHighest = ((w >>> 8) &&& 0xFF)
        let thirdHighest = ((w >>> 16) &&& 0xFF)
        let lowest = ((w >>> 24) &&& 0xFF)
        ((highest <<< 24) ^^^ (secondHighest <<< 16) ^^^ (thirdHighest <<< 8) ^^^ lowest)

    let inline toDouble (i : int) =
        float i / float Int32.MaxValue

type FloatProducer (rand : Random) =
    let locker = obj ()
    let mutable x = rand.Next ()
    let mutable y = rand.Next ()
    let mutable z = rand.Next ()
    let mutable w = rand.Next ()

    member __.Get () =
        Monitor.Enter locker
        let w =
            try
                FloatProducer.generateInt32 &x &y &z &w
            finally
                Monitor.Exit locker
        FloatProducer.toDouble (FloatProducer.toInt w)

    member __.GetTwo () : struct(float * float) =
        Monitor.Enter locker
        let struct(w1, w2) =
            try
                let one = FloatProducer.generateInt32 &x &y &z &w
                let two = FloatProducer.generateInt32 &x &y &z &w
                struct(one, two)
            finally
                Monitor.Exit locker

        struct(FloatProducer.toDouble (FloatProducer.toInt w1), FloatProducer.toDouble (FloatProducer.toInt w2))

    member _.GetThree () : struct(float * float * float) =
        Monitor.Enter locker
        let struct(w1, w2, w3) =
            try
                let one = FloatProducer.generateInt32 &x &y &z &w
                let two = FloatProducer.generateInt32 &x &y &z &w
                let three = FloatProducer.generateInt32 &x &y &z &w
                struct(one, two, three)
            finally
                Monitor.Exit locker

        struct(FloatProducer.toDouble (FloatProducer.toInt w1), FloatProducer.toDouble (FloatProducer.toInt w2), FloatProducer.toDouble (FloatProducer.toInt w3))


[<RequireQualifiedAccess>]
module Float =

    let tolerance = 0.00000001

    let inline equal (a : float) (b : float) : bool =
        abs (a - b) < tolerance

    // TODO: use of this method appears to slow everything down
    // by a factor of 4 - why?
    let inline positive (a : float) : bool =
        a > tolerance

    let inline compare<[<Measure>] 'a> (a : float<'a>) (b : float<'a>) : Comparison =
        if abs (a - b) < LanguagePrimitives.FloatWithMeasure tolerance then Comparison.Equal
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

