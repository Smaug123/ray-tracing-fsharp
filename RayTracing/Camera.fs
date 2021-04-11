namespace RayTracing

type Camera =
    {
        /// How tall is our viewport?
        ViewportHeight : float
        /// How wide is our viewport?
        ViewportWidth : float
        /// In which direction is the camera pointing?
        View : Ray
        /// What is the orientation of the imaginary plane
        /// onto which we're collecting the pixels of the result?
        /// This is normal to View and to ViewportYAxis, and its
        /// origin is at distance FocalLength from View.Origin.
        ViewportXAxis : Ray
        /// What is the orientation of the imaginary plane
        /// onto which we're collecting the pixels of the result?
        /// This is normal to View and to ViewportXAxis, and its
        /// origin is at distance FocalLength from View.Origin.
        ViewportYAxis : Ray
        /// How far away from the camera is the imaginary plane
        /// onto which we're collecting the pixels of the result?
        FocalLength : float
        /// How many samples will we take per pixel, for anti-aliasing?
        SamplesPerPixel : int
    }

[<RequireQualifiedAccess>]
module Camera =

    /// View angle is in radians (specified arbitrarily)
    let makeBasic
        (focalLength : float)
        (aspectRatio : float)
        (origin : Point)
        (viewDirection : UnitVector)
        (viewUp : Vector)
        : Camera
        =
        let height = 2.0
        let view = Ray.make origin viewDirection
        let corner = Ray.walkAlong view focalLength

        let viewPlane = Plane.makeNormalTo' corner viewDirection
        let xAxis, yAxis = Plane.basis viewUp viewPlane

        {
            FocalLength = focalLength
            ViewportHeight = height
            ViewportWidth = aspectRatio * height
            View = view
            ViewportXAxis = xAxis
            ViewportYAxis = yAxis
            SamplesPerPixel = 50
        }