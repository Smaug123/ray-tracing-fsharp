namespace RayTracing

/// A plane spanned by two rays from a common origin.
type Plane =
    private
        {
            V1 : Vector
            V2 : Vector
            Point : Point
        }

type OrthonormalPlane =
    {
        V1 : Vector
        V2 : Vector
        Point : Point
    }

[<RequireQualifiedAccess>]
module Plane =

    let orthonormalise (plane : Plane) : OrthonormalPlane option =
        let v1 = Vector.unitise plane.V1 |> Option.get
        let coeff = Vector.dot v1 plane.V2
        let vec2 =
            Vector.difference plane.V2 (Vector.scale coeff v1)
            |> Vector.unitise
        match vec2 with
        | None -> None
        | Some v2 ->
            {
                V1 = v1
                V2 = v2
                Point = plane.Point
            }
            |> Some

    let makeSpannedBy (r1 : Ray) (r2 : Ray) : Plane =
        {
            V1 = r1.Vector
            V2 = r2.Vector
            Point = r1.Origin
        }

    let basis (plane : OrthonormalPlane) : Ray * Ray =
        { Origin = plane.Point ; Vector = plane.V1 }, { Origin = plane.Point ; Vector = plane.V2 }
