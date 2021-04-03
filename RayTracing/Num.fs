namespace RayTracing

type Comparison =
    | Greater
    | Equal
    | Less

type Num<'a> =
    {
        Add : 'a -> 'a -> 'a
        Times : 'a -> 'a -> 'a
        Subtract : 'a -> 'a -> 'a
        Zero : 'a
        Compare : 'a -> 'a -> Comparison
    }
    member this.Double (x : 'a) : 'a =
        this.Add x x

[<RequireQualifiedAccess>]
module Num =
    let int : Num<int> =
        {
            Add = (+)
            Times = (*)
            Subtract = (-)
            Zero = 0
            Compare = fun a b -> if a < b then Comparison.Less elif a = b then Comparison.Equal else Comparison.Greater
        }

    let float : Num<float> =
        {
            Add = (+)
            Times = (*)
            Subtract = (-)
            Zero = 0.0
            Compare = fun a b -> if a < b then Comparison.Less elif a = b then Comparison.Equal else Comparison.Greater
        }

    let rational : Num<Rational> =
        {
            Add = Rational.add
            Times = Rational.times
            Subtract = Rational.subtract
            Zero = Rational.ofInt 0
            Compare = fun a b -> if a < b then Comparison.Less elif a = b then Comparison.Equal else Comparison.Greater
        }
