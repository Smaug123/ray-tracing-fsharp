namespace RayTracing

open SkiaSharp

[<RequireQualifiedAccess>]
type Texture =
    | Colour of Pixel
    | Arbitrary of (Point -> Pixel)

[<RequireQualifiedAccess>]
module Texture =
    let colourAt (point : Point) (t : Texture) : Pixel =
        match t with
        | Texture.Colour p -> p
        | Texture.Arbitrary f -> f point

/// A texture parameterised by 2d coordinates between 0 and 1.
[<RequireQualifiedAccess>]
type ParameterisedTexture =
    | Colour of Pixel
    | Checkered of even : ParameterisedTexture * odd : ParameterisedTexture * gridSize : float
    /// An image, given as an array of rows of pixels, top row first, left coordinate first.
    | Image of Pixel[][]
    | Arbitrary of (float -> float -> Texture)

/// A collection of textures, paramaterised by 2d coordinates between 0 and 1.
[<RequireQualifiedAccess>]
module ParameterisedTexture =

    let ofImage (img : SKBitmap) : ParameterisedTexture =
        Array.init
            img.Height
            (fun y ->
                let y = img.Height - y - 1

                Array.init
                    img.Width
                    (fun x ->
                        let p = img.GetPixel (x, y)

                        {
                            Red = p.Red
                            Green = p.Green
                            Blue = p.Blue
                        }
                    )
            )
        |> ParameterisedTexture.Image

    let rec colourAt (interpret : Point -> struct (float * float)) (t : ParameterisedTexture) (p : Point) : Pixel =
        match t with
        | ParameterisedTexture.Colour p -> p
        | ParameterisedTexture.Arbitrary f ->
            let struct (x, y) = interpret p
            Texture.colourAt p (f x y)
        | ParameterisedTexture.Checkered (even, odd, gridSize) ->
            let struct (x, y) = interpret p
            let sine = sin (gridSize * x) * sin (gridSize * y)

            match Float.compare sine 0.0 with
            | Less -> colourAt interpret even p
            | _ -> colourAt interpret odd p
        | ParameterisedTexture.Image img ->
            let struct (x, y) = interpret p
            let x = int ((1.0 - x) * float (img.[0].Length - 1))
            let y = int (y * float (img.Length - 1))
            img.[y].[x]

    let toTexture (interpret : Point -> struct (float * float)) (texture : ParameterisedTexture) : Texture =
        match texture with
        | ParameterisedTexture.Colour p -> Texture.Colour p
        | _ -> colourAt interpret texture |> Texture.Arbitrary
