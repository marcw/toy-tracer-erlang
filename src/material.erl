-module(material).
-export([new/3, metal/0, checkerboard/2]).

%% Material system for PBR (Physically Based Rendering)
%% Simplified Principled BSDF inspired by Blender

%% Creates a new material
%% Albedo: base color {R, G, B} in [0, 1] range
%% Roughness: surface roughness [0, 1] where 0=mirror, 1=diffuse
%% Metalness: metallic property [0, 1] where 0=dielectric, 1=metal
new(Albedo, Roughness, Metalness) ->
    {material, Albedo, Roughness, Metalness}.

%% Preset: metallic material (silver-like)
metal() ->
    new({0.8, 0.8, 0.8}, 0.2, 1.0).

%% Checkerboard pattern based on hit point
%% Returns either Color1 or Color2 based on position
checkerboard(HitPoint, Scale) ->
    {X, Y, Z} = HitPoint,
    %% Use floor to create integer grid
    Checker = (trunc(math:floor(X * Scale)) +
               trunc(math:floor(Y * Scale)) +
               trunc(math:floor(Z * Scale))) rem 2,

    case Checker of
        0 -> new({0.2, 0.2, 0.2}, 0.8, 0.0);  % Dark square
        _ -> new({0.9, 0.9, 0.9}, 0.8, 0.0)   % Light square
    end.
