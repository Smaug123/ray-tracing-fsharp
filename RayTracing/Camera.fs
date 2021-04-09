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
    let makeBasic
        (focalLength : float)
        (aspectRatio : float)
        (origin : Point)
        (viewDirection : UnitVector)
        : Camera
        =
        let height = 2.0
        let basis = UnitVector.basis 3
        let xAxis =
            basis.[0]
            |> Ray.make origin
        let yAxis =
            basis.[1]
            |> Ray.make origin

        let view = Ray.make origin viewDirection

        {
            FocalLength = focalLength
            ViewportHeight = height
            ViewportWidth = aspectRatio * height
            View = view
            ViewportXAxis =
                Ray.parallelTo (Ray.walkAlong view focalLength) xAxis
            ViewportYAxis =
                Ray.parallelTo (Ray.walkAlong view focalLength) yAxis
            SamplesPerPixel = 50
        }