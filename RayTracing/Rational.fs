namespace RayTracing

open System.Numerics

[<Struct>]
[<CustomEquality; CustomComparison>]
type Rational =
    private
        {
            Num : BigInteger
            Denom : BigInteger
            IsNormal : bool
        }

    static member Numerator (r : Rational) : BigInteger = r.Num
    static member Denominator (r : Rational) : BigInteger = r.Denom + BigInteger.One

    static member Normalise (r : Rational) : Rational =
        if r.IsNormal then
            r
        else
            let rec gcd (a : BigInteger) (b : BigInteger) : BigInteger =
                if a.Sign = -1 then -gcd (-a) b
                elif b.Sign = -1 then -gcd a (-b)
                elif a.IsZero then b
                elif b.IsZero then a
                else if a > b then gcd b (a % b)
                elif a = b then a
                else gcd b a

            let gcd =
                gcd (Rational.Numerator r) (Rational.Denominator r)

            { Rational.Make (Rational.Numerator r / gcd) (Rational.Denominator r / gcd) with
                IsNormal = true
            }


    member this.Normalise () = Rational.Normalise this

    static member Make (num : BigInteger) (denom : BigInteger) : Rational =
        if denom.IsZero then
            failwith "Invalid zero denominator"
        elif denom.Sign = -1 then
            {
                Num = -num
                Denom = (-denom) - BigInteger.One
                IsNormal = false
            }
        else
            {
                Num = num
                Denom = denom - BigInteger.One
                IsNormal = false
            }

    override this.Equals (other : obj) : bool =
        match other with
        | :? Rational as other ->
            printfn "%+A" other
            match this.Normalise () with
            | { Num = num; Denom = denom } ->
                match other.Normalise () with
                | { Num = numOther; Denom = denomOther } ->
                    printfn "%+A %+A %+A %+A" numOther num denom denomOther
                    numOther = num && denom = denomOther
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

            cmp.Sign

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

    let ofInt (i : int) : Rational = { Num = BigInteger i; Denom = BigInteger.Zero; IsNormal = true }

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

    let reciprocal (r : Rational) : Rational =
        Rational.Make (Rational.Denominator r) (Rational.Numerator r)

    let divide (r1 : Rational) (r2 : Rational) : Rational =
        times r1 (reciprocal r2)