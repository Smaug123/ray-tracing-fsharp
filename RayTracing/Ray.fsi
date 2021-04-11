namespace RayTracing

type Ray

[<RequireQualifiedAccess>]
module Ray =
    val make' : Point -> Vector -> Ray option
    val make : Point -> UnitVector -> Ray

    val walkAlong : Ray -> float -> Point

    val parallelTo : Point -> Ray -> Ray

    val liesOn : Point -> Ray -> bool

    val vector : Ray -> UnitVector
    val origin : Ray -> Point

    val flip : Ray -> Ray