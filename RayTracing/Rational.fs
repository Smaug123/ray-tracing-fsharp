namespace RayTracing

[<Struct>]
[<CustomEquality; CustomComparison>]
type Rational =
    private
        {
            Num : int
            Denom : int
            IsNormal : bool
        }

    static member Numerator (r : Rational) : int = r.Num
    static member Denominator (r : Rational) : int = r.Denom + 1

    static member Normalise (r : Rational) : Rational =
        if r.IsNormal then
            r
        else
            let rec gcd (a : int) (b : int) : int =
                if a < 0 then -gcd (-a) b
                elif b < 0 then -gcd a (-b)
                elif a = 0 then b
                elif b = 0 then a
                else if a > b then gcd b (a % b)
                elif a = b then a
                else gcd b a

            let gcd =
                gcd (Rational.Numerator r) (Rational.Denominator r)

            { Rational.Make (Rational.Numerator r / gcd) (Rational.Denominator r / gcd) with
                IsNormal = true
            }


    member this.Normalise () = Rational.Normalise this

    static member Make (num : int) (denom : int) : Rational =
        if denom = 0 then
            failwith "Invalid zero denominator"
        elif denom < 0 then
            {
                Num = -num
                Denom = (-denom) - 1
                IsNormal = false
            }
        else
            {
                Num = num
                Denom = denom - 1
                IsNormal = false
            }

    override this.Equals (other : obj) : bool =
        match other with
        | :? Rational as other ->
            match this.Normalise () with
            | { Num = num; Denom = denom } ->
                match other.Normalise () with
                | { Num = numOther; Denom = denomOther } -> numOther = num && denom = denomOther
        | _ -> failwith "how did you do this"

    override this.GetHashCode () : int =
        let n = this.Normalise ()
        hash (n.Num, n.Denom)

    interface System.IComparable<Rational> with
        member this.CompareTo (other : Rational) =
            let this = this.Normalise ()
            let other = other.Normalise ()

            let cmp =
                Rational.Numerator this
                * Rational.Denominator other
                - Rational.Numerator other
                  * Rational.Denominator this

            if cmp < 0 then -1
            elif cmp = 0 then 0
            else 1

    interface System.IComparable with
        member this.CompareTo (other : obj) =
            match other with
            | :? Rational as other ->
                (this :> System.IComparable<Rational>)
                    .CompareTo other
            | _ -> failwith "how?!"

[<RequireQualifiedAccess>]
module Rational =
    let inline make a b = Rational.Make a b

    let add (r1 : Rational) (r2 : Rational) =
        Rational.Make
            (Rational.Numerator r1 * Rational.Denominator r2
             + Rational.Numerator r2 * Rational.Denominator r1)
            (Rational.Denominator r1 * Rational.Denominator r2)
        |> Rational.Normalise

    let ofInt (i : int) : Rational = { Num = i; Denom = 0; IsNormal = true }

    let times (r1 : Rational) (r2 : Rational) =
        Rational.Make
            (Rational.Numerator r1 * Rational.Numerator r2)
            (Rational.Denominator r1 * Rational.Denominator r2)
        |> Rational.Normalise

    let subtract (r1 : Rational) (r2 : Rational) : Rational =
        Rational.Make
            (Rational.Numerator r1 * Rational.Denominator r2
             - Rational.Numerator r2 * Rational.Denominator r1)
            (Rational.Denominator r1 * Rational.Denominator r2)
        |> Rational.Normalise
