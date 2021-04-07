namespace RayTracing

/// A plane spanned by two rays from a common origin.
type Plane =
    private
        {
            V1 : UnitVector
            V2 : UnitVector
            Point : Point
        }

type OrthonormalPlane =
    {
        V1 : UnitVector
        V2 : UnitVector
        Point : Point
    }

[<RequireQualifiedAccess>]
module Plane =

    let orthonormalise (plane : Plane) : OrthonormalPlane option =
        let coeff = UnitVector.dot plane.V1 plane.V2
        let vec2 =
            UnitVector.difference' plane.V2 (UnitVector.scale coeff plane.V1)
            |> Vector.unitise
        match vec2 with
        | None -> None
        | Some v2 ->
            {
                V1 = plane.V1
                V2 = v2
                Point = plane.Point
            }
            |> Some

    let makeSpannedBy (r1 : Ray) (r2 : Ray) : Plane =
        {
            V1 = Ray.vector r1
            V2 = Ray.vector r2
            Point = Ray.origin r1
        }

    let basis (plane : OrthonormalPlane) : Ray * Ray =
        Ray.make plane.Point plane.V1,
        Ray.make plane.Point plane.V2
