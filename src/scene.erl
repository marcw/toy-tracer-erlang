-module(scene).
-export([default_scene/0, trace_ray/2]).

%% Scene management and ray tracing logic

%% Creates the default scene: matches the C reference scene
%% Plane, cube, sphere, cylinder, and torus with specific materials
default_scene() ->
    %% Ground plane with white diffuse material
    Ground = plane:new(
        {0.0, -1.0, 0.0},     % Point on plane
        {0.0, 1.0, 0.0},      % Normal (pointing up)
        material:new({0.9, 0.9, 0.9}, 1.0, 0.0)  % White diffuse
    ),

    %% Metal cube (silver-ish with roughness) - tilted for oriented box demo
    %% Rotation: mat3_from_euler(0.0f, 30.0f, 10.0f) - yaw 30°, roll 10°
    Cube = cube:new(
        {1.4, -0.3, 5.5},     % Center position
        mat3:from_euler(0.0, 30.0, 10.0),  % Rotation (pitch, yaw, roll in degrees)
        0.9,                   % Half-size (1.8 / 2)
        material:new({0.85, 0.84, 0.80}, 0.35, 1.0)  % Metallic silver
    ),

    %% Red diffuse sphere
    Sphere = sphere:new(
        {-1.6, -0.1, 4.5},    % Position
        0.9,                   % Radius
        material:new({0.75, 0.15, 0.12}, 1.0, 0.0)  % Red diffuse
    ),

    %% Blue diffuse cylinder (upright)
    Cylinder = cylinder:new(
        {-3.0, 0.0, 7.0},     % Position
        0.6,                   % Radius
        1.1,                   % Height (2.2 / 2)
        material:new({0.15, 0.35, 0.70}, 1.0, 0.0)  % Blue diffuse
    ),

    %% Gold metallic torus - rotated to highlight quartic intersection solver
    %% Rotation: mat3_from_euler(20.0f, 35.0f, 0.0f) - pitch 20°, yaw 35°
    Torus = torus:new(
        {0.0, 0.5, 8.0},      % Position
        mat3:from_euler(20.0, 35.0, 0.0),  % Rotation (pitch, yaw, roll in degrees)
        1.6,                   % Major radius
        0.4,                   % Minor radius
        material:new({0.95, 0.65, 0.25}, 0.25, 1.0)  % Gold metal
    ),

    %% Point light positioned above and to the side
    Light = {light, {6.0, 5.0, -2.0}, 55.0},  % {Position, Intensity}

    %% Camera setup
    Camera = camera:new(
        {0.0, 1.0, -5.0},     % Camera position (slightly above ground, looking forward)
        {0.0, 0.5, 5.0},      % Look at point (center of scene)
        60.0,                  % Field of view (degrees)
        16.0 / 9.0            % Aspect ratio (widescreen)
    ),

    %% Return scene structure
    {scene, [Ground, Cube, Sphere, Cylinder, Torus], Light, Camera}.

%% Trace a ray through the scene and return color
%% Finds closest intersection and computes lighting
trace_ray(Ray, Scene) ->
    {scene, Objects, Light, _Camera} = Scene,

    %% Find closest intersection
    case find_closest_hit(Ray, Objects, false, 1.0e30) of
        false ->
            %% No hit, return background color (gradient from blue to white)
            background_color(Ray);
        {HitDist, HitObject} ->
            %% Calculate hit point and normal
            HitPoint = ray:at(Ray, HitDist),
            Normal = get_normal(HitObject, HitPoint),

            %% Get material (might be from checkerboard)
            HitMaterial = get_material(HitObject, HitPoint),

            %% Check for shadows
            {light, LightPos, LightIntensity} = Light,
            InShadow = check_shadow(HitPoint, LightPos, Objects),

            %% Compute lighting
            {ray, _Origin, RayDir} = Ray,
            ViewDir = vec3:negate(RayDir),

            case InShadow of
                true ->
                    %% In shadow, only ambient lighting
                    {material, Albedo, _Roughness, _Metalness} = HitMaterial,
                    vec3:mul(Albedo, 0.03);
                false ->
                    %% Full lighting calculation
                    lighting:compute_color(HitPoint, Normal, ViewDir,
                                         LightPos, LightIntensity, HitMaterial)
            end
    end.

%% Find the closest intersection among all objects
find_closest_hit(_Ray, [], false, _ClosestDist) ->
    false;
find_closest_hit(_Ray, [], ClosestObject, _ClosestDist) ->
    ClosestObject;
find_closest_hit(Ray, [Object | Rest], ClosestObject, ClosestDist) ->
    case intersect_object(Object, Ray) of
        {true, Dist, _Material} when Dist < ClosestDist ->
            find_closest_hit(Ray, Rest, {Dist, Object}, Dist);
        _ ->
            find_closest_hit(Ray, Rest, ClosestObject, ClosestDist)
    end.

%% Intersect ray with any type of object
intersect_object({sphere, _Center, _Radius, _Mat} = Sphere, Ray) ->
    sphere:intersect(Sphere, Ray);
intersect_object({plane, _Point, _Normal, _Mat} = Plane, Ray) ->
    plane:intersect(Plane, Ray);
intersect_object({cube, _Center, _Rotation, _Size, _Mat} = Cube, Ray) ->
    cube:intersect(Cube, Ray);
intersect_object({cylinder, _Center, _Radius, _Height, _Mat} = Cylinder, Ray) ->
    cylinder:intersect(Cylinder, Ray);
intersect_object({torus, _Center, _Rotation, _MajorR, _MinorR, _Mat} = Torus, Ray) ->
    torus:intersect(Torus, Ray).

%% Get surface normal for any object type
get_normal({sphere, _C, _R, _M} = Sphere, Point) ->
    sphere:normal(Sphere, Point);
get_normal({plane, _P, _N, _M} = Plane, Point) ->
    plane:normal(Plane, Point);
get_normal({cube, _C, _Rot, _S, _M} = Cube, Point) ->
    cube:normal(Cube, Point);
get_normal({cylinder, _C, _R, _H, _M} = Cylinder, Point) ->
    cylinder:normal(Cylinder, Point);
get_normal({torus, _C, _Rot, _MR, _mR, _M} = Torus, Point) ->
    torus:normal(Torus, Point).

%% Get material for an object (handling checkerboard special case)
get_material({plane, _Point, _Normal, checkerboard}, HitPoint) ->
    material:checkerboard(HitPoint, 1.0);
get_material({plane, _Point, _Normal, Material}, _HitPoint) ->
    Material;
get_material({sphere, _Center, _Radius, Material}, _HitPoint) ->
    Material;
get_material({cube, _Center, _Rotation, _Size, Material}, _HitPoint) ->
    Material;
get_material({cylinder, _Center, _Radius, _Height, Material}, _HitPoint) ->
    Material;
get_material({torus, _Center, _Rotation, _MajorR, _MinorR, Material}, _HitPoint) ->
    Material.

%% Check if point is in shadow (between hit point and light)
check_shadow(HitPoint, LightPos, Objects) ->
    %% Offset slightly to avoid self-intersection
    LightDir = vec3:normalize(vec3:sub(LightPos, HitPoint)),
    OffsetPoint = vec3:add(HitPoint, vec3:mul(LightDir, 0.001)),

    ShadowRay = ray:new(OffsetPoint, LightDir),
    LightDist = vec3:length(vec3:sub(LightPos, HitPoint)),

    %% Check if any object blocks the light
    check_shadow_recursive(ShadowRay, Objects, LightDist).

check_shadow_recursive(_ShadowRay, [], _LightDist) ->
    false;  % No occlusion
check_shadow_recursive(ShadowRay, [Object | Rest], LightDist) ->
    case intersect_object(Object, ShadowRay) of
        {true, Dist, _Material} when Dist < LightDist ->
            true;  % Object blocks the light
        _ ->
            check_shadow_recursive(ShadowRay, Rest, LightDist)
    end.

%% Background color (solid dark blue-gray)
background_color(_Ray) ->
    %% Matches C reference: vec3(0.08f, 0.10f, 0.16f)
    {0.08, 0.10, 0.16}.
