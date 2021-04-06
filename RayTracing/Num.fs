namespace RayTracing

open System

type 'a Radian =
    | Radian of 'a

type Comparison =
    | Greater
    | Equal
    | Less

type Num<'a> =
    {
        Add : 'a -> 'a -> 'a
        Times : 'a -> 'a -> 'a
        Negate : 'a -> 'a
        Reciprocal : 'a -> 'a
        Zero : 'a
        Compare : 'a -> 'a -> Comparison
        Sqrt : 'a -> 'a
        Equal : 'a -> 'a -> bool
        TimesInteger : int -> 'a -> 'a
        DivideInteger : 'a -> int -> 'a
        One : 'a
        RandomBetween01 : Random -> 'a
        ArcCos : 'a -> 'a Radian
        // arctan(second / first)
        ArcTan2 : 'a -> 'a -> 'a Radian
        Cos : 'a Radian -> 'a
        Sin : 'a Radian -> 'a
        Round : 'a -> int
    }

    member this.Double (x : 'a) : 'a = this.Add x x
    member this.Subtract (x : 'a) (y : 'a) : 'a = this.Add x (this.Negate y)
    member this.Divide (x : 'a) (y : 'a) : 'a = this.Times x (this.Reciprocal y)

    member this.Pi =
        let (Radian t) = this.ArcCos (this.Negate this.One)
        t

[<RequireQualifiedAccess>]
module Num =
    let float : Num<float> =
        let tolerance = 0.00000001
        {
            Add = (+)
            Times = (*)
            Negate = fun x -> -x
            Zero = 0.0
            Reciprocal = fun i -> 1.0 / i
            Compare =
                fun a b ->
                    if abs (a - b) < tolerance then Comparison.Equal
                    elif a < b then Comparison.Less
                    else Comparison.Greater
            Sqrt = sqrt
            Equal = fun a b -> abs (a - b) < tolerance
            TimesInteger = fun a b -> float a * b
            DivideInteger = fun a b -> a / float b
            One = 1.0
            RandomBetween01 = fun rand -> float (abs (rand.Next ())) / float Int32.MaxValue
            ArcCos = acos >> Radian
            ArcTan2 = fun x -> atan2 x >> Radian
            Sin = fun (Radian r) -> sin r
            Cos = fun (Radian r) -> cos r
            Round = fun i -> Math.Round i |> int
        }

    let algebraic : Num<Algebraic> =
        {
            Add = Algebraic.add
            Times = Algebraic.times
            Negate = Algebraic.negate
            Zero = Algebraic.ofInt 0
            Reciprocal = Algebraic.reciprocal
            Compare =
                fun a b ->
                    if a < b then Comparison.Less
                    elif a = b then Comparison.Equal
                    else Comparison.Greater
            Sqrt = Algebraic.sqrt
            Equal = Algebraic.equal
            TimesInteger = fun _ _ -> failwith ""
            DivideInteger = fun _ _ -> failwith ""
            One = Algebraic.ofInt 1
            RandomBetween01 = fun _ -> failwith ""
            ArcCos = fun _ -> failwith ""
            ArcTan2 = fun _ -> failwith ""
            Cos = fun _ -> failwith ""
            Sin = fun _ -> failwith ""
            Round = fun _ -> failwith ""
        }

    let sortInPlaceBy<'a, 'b> (num : 'a Num) (proj : 'b -> 'a) (a : 'b array) : 'b array =
        for i in 0..a.Length - 2 do
            for j in i+1..a.Length - 1 do
                match num.Compare (proj a.[i]) (proj a.[j]) with
                | Greater ->
                    let tmp = a.[j]
                    a.[j] <- a.[i]
                    a.[i] <- tmp
                | _ -> ()
        a

[<RequireQualifiedAccess>]
module Radian =
    let add<'a> (n : Num<'a>) (Radian r1) (Radian r2) = n.Add r1 r2 |> Radian