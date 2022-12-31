namespace RayTracing

/// Index of refraction of this material.
[<Measure>]
type ior

type LightRay =
    {
        Ray : Ray
        Colour : Pixel
    // We have chosen not to include refractance here, because that would mean
    // we had to model the material at every point in space rather than just the
    // ratio of refractance at the boundaries of objects. (For example, if we
    // modelled a light ray leaving a glass sphere, we would have to know what
    // material we were leaving *into*, which we can't easily know given the
    // current structure of things.)
    }

type LightDestination =
    | Continues of LightRay
    | Absorbs of Pixel
