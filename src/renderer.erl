-module(renderer).
-export([render/3]).

%% Main rendering loop
%% Iterates over all pixels, traces rays, and builds pixel buffer

%% Render the scene to a pixel buffer
%% Width: image width in pixels
%% Height: image height in pixels
%% Scene: scene structure from scene:default_scene()
render(Width, Height, Scene) ->
    io:format("Starting render: ~px~p~n", [Width, Height]),

    %% Extract camera from scene
    {scene, _Objects, _Light, Camera} = Scene,

    %% Render all pixels
    Pixels = render_pixels(Width, Height, Camera, Scene, 0, []),

    io:format("Rendering complete!~n"),
    Pixels.

%% Recursively render all pixels row by row
render_pixels(_Width, Height, _Camera, _Scene, Y, Acc) when Y >= Height ->
    %% Finished rendering, reverse to get correct order
    lists:reverse(Acc);
render_pixels(Width, Height, Camera, Scene, Y, Acc) ->
    %% Progress indicator every 10 rows
    case Y rem 10 of
        0 -> io:format("Rendering row ~p/~p~n", [Y, Height]);
        _ -> ok
    end,

    %% Render all pixels in this row
    RowPixels = render_row(Width, Height, Camera, Scene, Y, 0, []),

    %% Continue to next row
    render_pixels(Width, Height, Camera, Scene, Y + 1, RowPixels ++ Acc).

%% Render a single row of pixels
render_row(Width, _Height, _Camera, _Scene, _Y, X, Acc) when X >= Width ->
    lists:reverse(Acc);
render_row(Width, Height, Camera, Scene, Y, X, Acc) ->
    %% Calculate UV coordinates [0, 1]
    U = X / Width,
    V = Y / Height,

    %% Generate ray through this pixel
    Ray = camera:get_ray(Camera, U, V),

    %% Trace ray through scene
    Color = scene:trace_ray(Ray, Scene),

    %% Convert from [0, 1] float to [0, 255] integer with gamma correction
    Pixel = color_to_pixel(Color),

    %% Continue to next pixel in row
    render_row(Width, Height, Camera, Scene, Y, X + 1, [Pixel | Acc]).

%% Convert HDR color to LDR pixel with gamma correction
%% Color: {R, G, B} in [0, inf] range (HDR)
%% Returns: {R, G, B, A} in [0, 255] range (LDR)
color_to_pixel({R, G, B}) ->
    %% Apply simple tone mapping (clamp)
    R_clamped = vec3:clamp(R, 0.0, 1.0),
    G_clamped = vec3:clamp(G, 0.0, 1.0),
    B_clamped = vec3:clamp(B, 0.0, 1.0),

    %% Apply gamma correction (gamma = 2.2)
    %% This makes the image look correct on standard displays
    R_gamma = math:pow(R_clamped, 1.0 / 2.2),
    G_gamma = math:pow(G_clamped, 1.0 / 2.2),
    B_gamma = math:pow(B_clamped, 1.0 / 2.2),

    %% Convert to 8-bit integer [0, 255]
    R_int = round(R_gamma * 255.0),
    G_int = round(G_gamma * 255.0),
    B_int = round(B_gamma * 255.0),

    %% Return RGBA (fully opaque)
    {R_int, G_int, B_int, 255}.
