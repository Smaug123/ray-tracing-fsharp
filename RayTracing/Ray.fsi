namespace RayTracing

type Ray =
    {
        /// For performance reasons, this is public, but please don't use it
        mutable Origin : Point
        /// For performance reasons, this is public, but please don't use it
        mutable Vector : UnitVector
    }

[<RequireQualifiedAccess>]
module Ray =
    val make' : Point -> Vector -> Ray voption
    val make : Point -> UnitVector -> Ray

    /// If we can make a ray from Point and Vector, overwrite the input and return true.
    /// Otherwise do nothing and return false.
    val overwriteWithMake : Point -> Vector -> byref<Ray> -> bool

    val walkAlong : Ray -> float -> Point

    val walkAlongRay : Point -> UnitVector -> float -> Point

    val parallelTo : Point -> Ray -> Ray
    val translateToIntersect : Point -> Ray -> unit

    val liesOn : Point -> Ray -> bool

    val inline vector : Ray -> UnitVector
    val inline origin : Ray -> Point

    val flip : Ray -> Ray
    val flipInPlace : Ray -> unit
