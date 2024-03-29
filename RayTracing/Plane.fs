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

    let makeNormalTo (point : Point) (Vector (x, y, z) as v) : OrthonormalPlane =
        let v1 =
            if Float.equal z 0.0 then
                Vector.make 0.0 0.0 1.0
            else
                Vector.make 1.0 1.0 ((-x - y) / z)

        let v2 = Vector.cross v v1 |> Vector.unitise |> ValueOption.get
        let v1 = v1 |> Vector.unitise |> ValueOption.get

        {
            Point = point
            V1 = v1
            V2 = v2
        }

    let inline makeNormalTo' (point : Point) (UnitVector v) = makeNormalTo point v

    let orthonormalise (plane : Plane) : OrthonormalPlane voption =
        let coefficient = UnitVector.dot plane.V1 plane.V2

        let vec2 =
            UnitVector.difference' plane.V2 (UnitVector.scale coefficient plane.V1)
            |> Vector.unitise

        match vec2 with
        | ValueNone -> ValueNone
        | ValueSome v2 ->
            {
                V1 = plane.V1
                V2 = v2
                Point = plane.Point
            }
            |> ValueSome

    let makeSpannedBy (r1 : Ray) (r2 : Ray) : Plane =
        {
            V1 = Ray.vector r1
            V2 = Ray.vector r2
            Point = Ray.origin r1
        }

    let makeOrthonormalSpannedBy (r1 : Ray) (r2 : Ray) : OrthonormalPlane ValueOption =
        let coefficient = UnitVector.dot r1.Vector r2.Vector

        let vec2 =
            UnitVector.difference' r2.Vector (UnitVector.scale coefficient r1.Vector)
            |> Vector.unitise

        match vec2 with
        | ValueNone -> ValueNone
        | ValueSome v2 ->
            {
                V1 = r1.Vector
                V2 = v2
                Point = Ray.origin r1
            }
            |> ValueSome

    /// Construct a basis for this plane, whose second ("up") component is `viewUp` when projected onto the plane.
    let basis (viewUp : Vector) (plane : OrthonormalPlane) : Ray * Ray =
        let viewUp = Vector.unitise viewUp |> ValueOption.get
        let v1Component = UnitVector.dot plane.V1 viewUp
        let v2Component = UnitVector.dot plane.V2 viewUp

        let v2 =
            Vector.sum (UnitVector.scale v1Component plane.V1) (UnitVector.scale v2Component plane.V2)
            |> Vector.unitise
            |> ValueOption.get

        let v1 =
            Vector.sum (UnitVector.scale v2Component plane.V1) (UnitVector.scale (-v1Component) plane.V2)
            |> Vector.unitise
            |> ValueOption.get

        Ray.make plane.Point v1, Ray.make plane.Point v2
