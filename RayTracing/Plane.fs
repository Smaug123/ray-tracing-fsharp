namespace RayTracing

/// A plane spanned by two rays from a common origin.
type 'a Plane =
    private
        {
            V1 : 'a Ray
            V2 : 'a Ray
        }

type 'a OrthonormalPlane =
    {
        V1 : 'a Ray
        V2 : 'a Ray
    }

[<RequireQualifiedAccess>]
module Plane =

    let orthonormalise<'a> (num : 'a Num) (plane : 'a Plane) : 'a OrthonormalPlane option =
        let coeff = num.Divide (Vector.dot num plane.V1.Vector plane.V2.Vector) (Vector.dot num plane.V1.Vector plane.V1.Vector)
        let v1 = { Origin = plane.V1.Origin ; Vector = Vector.unitise num plane.V1.Vector |> Option.get }
        let vec2 =
            Point.difference num plane.V2.Vector (Vector.scale num coeff plane.V1.Vector)
            |> Vector.unitise num
        match vec2 with
        | None -> None
        | Some v2 ->
            let v2 =
                {
                    Origin = plane.V1.Origin
                    Vector = v2
                }
            {
                V1 = v1
                V2 = v2
            }
            |> Some

    let makeSpannedBy<'a> (r1 : 'a Ray) (r2 : 'a Ray) : 'a Plane =
        {
            V1 = r1
            V2 = r2
        }

    /// Construct the ray at the given angle from the given ray in the given plane.
    let rayAtAngle<'a> (num : Num<'a>) (ray : 'a Ray) (plane : 'a OrthonormalPlane) (angle : 'a Radian) : 'a Ray =
        let r = Ray.walkAlong num ray num.One
        let d1 = Vector.dot num (Point.difference num r plane.V1.Origin) plane.V1.Vector
        let d2 = Vector.dot num (Point.difference num r plane.V1.Origin) plane.V2.Vector
        // r = d1 v1 + d2 v2
        // then the desired ray is x1 v1 + x2 v2, where:
        // x1 = cos (theta + arctan(d2/d1))
        // x2 = sin (theta + arctan(d2/d1))
        let atan = num.ArcTan2 d2 d1
        let intermediate = Radian.add num angle atan
        let onV1Axis =
            num.Cos intermediate
            |> Ray.walkAlong num plane.V1
        {
            Origin = ray.Origin
            Vector =
                num.Sin intermediate
                |> Ray.walkAlong num { Origin = onV1Axis ; Vector = plane.V2.Vector }
        }

    let basis<'a> (plane : 'a OrthonormalPlane) : 'a Ray * 'a Ray =
        plane.V1, plane.V2
