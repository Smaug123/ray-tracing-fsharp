namespace RayTracing

type Camera<'a> =
    {
        Num : Num<'a>
        /// How tall is our viewport?
        ViewportHeight : 'a
        /// How wide is our viewport?
        ViewportWidth : 'a
        /// In which direction is the camera pointing?
        View : Ray<'a>
        /// What is the orientation of the imaginary plane
        /// onto which we're collecting the pixels of the result?
        /// This is normal to View and to ViewportYAxis, and its
        /// origin is at distance FocalLength from View.Origin.
        ViewportXAxis : Ray<'a>
        /// What is the orientation of the imaginary plane
        /// onto which we're collecting the pixels of the result?
        /// This is normal to View and to ViewportXAxis, and its
        /// origin is at distance FocalLength from View.Origin.
        ViewportYAxis : Ray<'a>
        /// How far away from the camera is the imaginary plane
        /// onto which we're collecting the pixels of the result?
        FocalLength : 'a
        /// How many samples will we take per pixel, for anti-aliasing?
        SamplesPerPixel : int
    }

[<RequireQualifiedAccess>]
module Camera =
    let makeBasic<'a>
        (n : Num<'a>)
        (focalLength : 'a)
        (aspectRatio : 'a)
        (origin : Point<'a>)
        : Camera<'a>
        =
        let height = n.Double n.One
        let view =
            {
                Origin = origin
                Vector = Point [| n.Zero ; n.Zero ; n.One |]
            }
        let xAxis =
            {
                Origin = origin
                Vector = Point [| n.One ; n.Zero ; n.Zero |]
            }
        let yAxis =
            {
                Origin = origin
                Vector = Point [| n.Zero ; n.One ; n.Zero |]
            }

        {
            Num = n
            FocalLength = focalLength
            ViewportHeight = height
            ViewportWidth = n.Times aspectRatio height
            View = view
            ViewportXAxis =
                Ray.parallelTo (Ray.walkAlong n view focalLength) xAxis
            ViewportYAxis =
                Ray.parallelTo (Ray.walkAlong n view focalLength) yAxis
            SamplesPerPixel = 5
        }