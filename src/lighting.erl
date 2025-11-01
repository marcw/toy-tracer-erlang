-module(lighting).
-export([compute_color/6]).

%% Simplified PBR (Physically Based Rendering) lighting
%% Inspired by Blender's Principled BSDF but simplified for education
%% Uses Cook-Torrance microfacet model approximation

%% Compute color for a hit point with a single light
%% HitPoint: 3D position of surface hit
%% Normal: surface normal at hit point
%% ViewDir: direction from surface to camera (normalized)
%% LightPos: position of point light
%% LightIntensity: light brightness
%% Material: {material, Albedo, Roughness, Metalness}
compute_color(HitPoint, Normal, ViewDir, LightPos, LightIntensity, Material) ->
    {material, Albedo, Roughness, Metalness} = Material,

    %% Direction from surface to light
    LightDir = vec3:normalize(vec3:sub(LightPos, HitPoint)),

    %% Distance to light for attenuation
    LightDist = vec3:length(vec3:sub(LightPos, HitPoint)),
    Attenuation = 1.0 / (1.0 + 0.01 * LightDist * LightDist),

    %% Lambertian diffuse term
    NdotL = max(0.0, vec3:dot(Normal, LightDir)),

    %% Calculate halfway vector for specular
    HalfVector = vec3:normalize(vec3:add(LightDir, ViewDir)),
    NdotH = max(0.0, vec3:dot(Normal, HalfVector)),
    NdotV = max(0.0, vec3:dot(Normal, ViewDir)),

    %% Diffuse component (Lambert)
    %% Metals have no diffuse component
    DiffuseStrength = (1.0 - Metalness) * NdotL,
    Diffuse = vec3:mul(Albedo, DiffuseStrength),

    %% Specular component (simplified Cook-Torrance)
    %% D: GGX normal distribution
    D = ggx_distribution(NdotH, Roughness),

    %% F: Fresnel-Schlick approximation
    %% Base reflectivity depends on metalness
    F0 = mix_scalar(0.04, 1.0, Metalness),
    F = fresnel_schlick(NdotV, F0),

    %% G: Geometry term (simplified)
    G = geometry_smith(NdotL, NdotV, Roughness),

    %% Cook-Torrance specular BRDF
    Specular = (D * F * G) / max(0.001, 4.0 * NdotL * NdotV),

    %% Combine diffuse and specular
    %% For metals, tint the specular by albedo
    SpecularColor = case Metalness > 0.5 of
        true -> vec3:mul(Albedo, Specular);
        false -> {Specular, Specular, Specular}
    end,

    %% Final color with light intensity and attenuation
    Color = vec3:add(Diffuse, SpecularColor),
    FinalColor = vec3:mul(Color, LightIntensity * Attenuation),

    %% Add ambient term to prevent pure black
    Ambient = vec3:mul(Albedo, 0.03),
    vec3:add(FinalColor, Ambient).

%% GGX/Trowbridge-Reitz normal distribution function
ggx_distribution(NdotH, Roughness) ->
    A = Roughness * Roughness,
    A2 = A * A,
    NdotH2 = NdotH * NdotH,
    Nom = A2,
    Denom = NdotH2 * (A2 - 1.0) + 1.0,
    Nom / (math:pi() * Denom * Denom).

%% Fresnel-Schlick approximation
fresnel_schlick(Cosine, F0) ->
    F0 + (1.0 - F0) * math:pow(1.0 - Cosine, 5.0).

%% Simplified Smith's geometry term
geometry_smith(NdotL, NdotV, Roughness) ->
    R = Roughness + 1.0,
    K = (R * R) / 8.0,
    GL = NdotL / (NdotL * (1.0 - K) + K),
    GV = NdotV / (NdotV * (1.0 - K) + K),
    GL * GV.

%% Helper: linear interpolation for scalars
mix_scalar(A, B, T) ->
    A + (B - A) * T.
