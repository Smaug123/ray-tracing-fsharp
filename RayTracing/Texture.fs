namespace RayTracing

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
    | Arbitrary of (float -> float -> Texture)

/// A collection of textures, paramaterised by 2d coordinates between 0 and 1.
[<RequireQualifiedAccess>]
module ParameterisedTexture =

    let rec colourAt (interpret : float -> float -> Point) (t : ParameterisedTexture) (x : float) (y : float) : Pixel =
        match t with
        | ParameterisedTexture.Colour p -> p
        | ParameterisedTexture.Arbitrary f -> Texture.colourAt (interpret x y) (f x y)
        | ParameterisedTexture.Checkered (even, odd, gridSize) ->
            let sine = sin (gridSize * x) * sin (gridSize * y)
            match Float.compare sine 0.0 with
            | Less -> colourAt interpret even x y
            | _ -> colourAt interpret odd x y

    let rec colourAt' (interpret : Point -> struct(float * float)) (t : ParameterisedTexture) (p : Point) : Pixel =
        match t with
        | ParameterisedTexture.Colour p -> p
        | ParameterisedTexture.Arbitrary f ->
            let struct(x, y) = interpret p
            Texture.colourAt p (f x y)
        | ParameterisedTexture.Checkered (even, odd, gridSize) ->
            let struct(x, y) = interpret p
            let sine = sin (gridSize * x) * sin (gridSize * y)
            match Float.compare sine 0.0 with
            | Less -> colourAt' interpret even p
            | _ -> colourAt' interpret odd p

    let toTexture (interpret : Point -> struct(float * float)) (texture : ParameterisedTexture) : Texture =
        match texture with
        | ParameterisedTexture.Colour p -> Texture.Colour p
        | _ ->
            colourAt' interpret texture
            |> Texture.Arbitrary
