namespace RayTracing

type Hittable =
    | Sphere of Sphere
    | UnboundedSphere of Sphere
    | InfinitePlane of InfinitePlane

    member this.Reflection (incoming : LightRay) (strikePoint : Point) =
        match this with
        | Sphere s
        | UnboundedSphere s -> s.Reflection incoming strikePoint
        | InfinitePlane p -> p.Reflection incoming strikePoint

    member this.BoundingBox : BoundingBox option =
        match this with
        | Sphere s -> Sphere.boundingBox s |> Some
        | UnboundedSphere _
        | InfinitePlane _ -> None

[<RequireQualifiedAccess>]
module Hittable =

    let inline boundingBox (h : Hittable) = h.BoundingBox

    /// Returns the distance we must walk along this ray before we first hit an object, the
    /// colour the resulting light ray is after the interaction, and the new ray.
    let hits (ray : Ray) (h : Hittable) : float voption =
        match h with
        | UnboundedSphere s
        | Sphere s -> Sphere.firstIntersection s ray
        | InfinitePlane plane -> InfinitePlane.intersection plane ray
