namespace RayTracing

type Algebraic =
    | Sqrt of Algebraic
    | Rational of Rational
    | Sum of Algebraic * Algebraic
    | Times of Algebraic * Algebraic
    | Subtract of Algebraic * Algebraic
    | Negate of Algebraic
    | Reciprocal of Algebraic

module Algebraic =
    let ofInt (i : int) = Rational (Rational.ofInt i)
    let ofRational (r : Rational) = Rational r

    let add (a1 : Algebraic) (a2 : Algebraic) : Algebraic =
        Sum (a1, a2)

    let times (a1 : Algebraic) (a2 : Algebraic) : Algebraic =
        Times (a1, a2)

    let sqrt (a1 : Algebraic) : Algebraic =
        Sqrt a1

    let negate (a1 : Algebraic) : Algebraic =
        Negate a1

    let reciprocal (a1 : Algebraic) : Algebraic =
        Reciprocal a1

    let equal (a1 : Algebraic) (a2 : Algebraic) : bool =
        failwith "TODO"
