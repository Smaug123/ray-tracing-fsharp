namespace RayTracing

open System

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
    }

    member this.Double (x : 'a) : 'a = this.Add x x
    member this.Subtract (x : 'a) (y : 'a) : 'a = this.Add x (this.Negate y)
    member this.Divide (x : 'a) (y : 'a) : 'a = this.Times x (this.Reciprocal y)

[<RequireQualifiedAccess>]
module Num =
    let float : Num<float> =
        {
            Add = (+)
            Times = (*)
            Negate = fun x -> -x
            Zero = 0.0
            Reciprocal = fun i -> 1.0 / i
            Compare =
                fun a b ->
                    if a < b then Comparison.Less
                    elif a = b then Comparison.Equal
                    else Comparison.Greater
            Sqrt = sqrt
            Equal = fun a b -> abs (a - b) < 0.00000001
            TimesInteger = fun a b -> float a * b
            DivideInteger = fun a b -> a / float b
            One = 1.0
            RandomBetween01 = fun rand -> float (abs (rand.Next ())) / float Int32.MaxValue
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
        }
