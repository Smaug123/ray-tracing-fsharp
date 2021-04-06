namespace RayTracing

/// A plane spanned by two rays from a common origin.
type 'a Plane =
    private
        {
            V1 : 'a Vector
            V2 : 'a Vector
            Point : 'a Point
        }

type 'a OrthonormalPlane =
    {
        V1 : 'a Vector
        V2 : 'a Vector
        Point : 'a Point
    }


[<RequireQualifiedAccess>]
module Plane =

    let orthonormalise<'a> (num : 'a Num) (plane : 'a Plane) : 'a OrthonormalPlane option =
        let v1 = Vector.unitise num plane.V1 |> Option.get
        let coeff = Vector.dot num v1 plane.V2
        let vec2 =
            Vector.difference num plane.V2 (Vector.scale num coeff v1)
            |> Vector.unitise num
        match vec2 with
        | None -> None
        | Some v2 ->
            {
                V1 = v1
                V2 = v2
                Point = plane.Point
            }
            |> Some

    let makeSpannedBy<'a> (r1 : 'a Ray) (r2 : 'a Ray) : 'a Plane =
        {
            V1 = r1.Vector
            V2 = r2.Vector
            Point = r1.Origin
        }

    let basis<'a> (plane : 'a OrthonormalPlane) : 'a Ray * 'a Ray =
        { Origin = plane.Point ; Vector = plane.V1 }, { Origin = plane.Point ; Vector = plane.V2 }
