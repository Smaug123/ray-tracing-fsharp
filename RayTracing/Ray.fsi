namespace RayTracing

type Ray =
    {
        /// For performance reasons, this is public, but please don't use it
        Origin : Point
        /// For performance reasons, this is public, but please don't use it
        Vector : UnitVector
    }

[<RequireQualifiedAccess>]
module Ray =
    val make' : Point -> Vector -> Ray ValueOption
    val make : Point -> UnitVector -> Ray

    val walkAlong : Ray -> float -> Point

    val parallelTo : Point -> Ray -> Ray

    val liesOn : Point -> Ray -> bool

    val inline vector : Ray -> UnitVector
    val inline origin : Ray -> Point

    val flip : Ray -> Ray
