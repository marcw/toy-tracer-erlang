-module(scene).
-export([default_scene/0, trace_ray/2]).

%% Scene management and ray tracing logic

%% Creates the default scene: cube on a checkerboard plane
%% As specified in AGENTS.md section 8
default_scene() ->
    %% Ground plane with checkerboard pattern
    Ground = plane:new(
        {0.0, -1.0, 0.0},     % Point on plane
        {0.0, 1.0, 0.0},      % Normal (pointing up)
        checkerboard           % Special material type
    ),

    %% Metal cube at the scene center
    Cube = cube:new(
        {0.0, 0.0, 5.0},      % Center position
        1.0,                   % Half-size (1.0 means 2x2x2 cube)
        material:metal()       % Metallic material
    ),

    %% Additional objects to showcase all primitives
    Sphere = sphere:new(
        {-3.0, 0.0, 5.0},     % Position
        1.0,                   % Radius
        material:new({0.8, 0.2, 0.2}, 0.3, 0.0)  % Red diffuse
    ),

    Cylinder = cylinder:new(
        {3.0, 0.0, 5.0},      % Position
        0.7,                   % Radius
        1.5,                   % Height
        material:new({0.2, 0.8, 0.2}, 0.4, 0.0)  % Green diffuse
    ),

    Torus = torus:new(
        {0.0, 0.0, 8.0},      % Position
        1.5,                   % Major radius
        0.5,                   % Minor radius
        material:new({0.2, 0.2, 0.8}, 0.2, 0.8)  % Blue metallic
    ),

    %% Point light positioned above and to the side
    Light = {light, {5.0, 5.0, 0.0}, 100.0},  % {Position, Intensity}

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
intersect_object({cube, _Center, _Size, _Mat} = Cube, Ray) ->
    cube:intersect(Cube, Ray);
intersect_object({cylinder, _Center, _Radius, _Height, _Mat} = Cylinder, Ray) ->
    cylinder:intersect(Cylinder, Ray);
intersect_object({torus, _Center, _MajorR, _MinorR, _Mat} = Torus, Ray) ->
    torus:intersect(Torus, Ray).

%% Get surface normal for any object type
get_normal({sphere, _C, _R, _M} = Sphere, Point) ->
    sphere:normal(Sphere, Point);
get_normal({plane, _P, _N, _M} = Plane, Point) ->
    plane:normal(Plane, Point);
get_normal({cube, _C, _S, _M} = Cube, Point) ->
    cube:normal(Cube, Point);
get_normal({cylinder, _C, _R, _H, _M} = Cylinder, Point) ->
    cylinder:normal(Cylinder, Point);
get_normal({torus, _C, _MR, _mR, _M} = Torus, Point) ->
    torus:normal(Torus, Point).

%% Get material for an object (handling checkerboard special case)
get_material({plane, _Point, _Normal, checkerboard}, HitPoint) ->
    material:checkerboard(HitPoint, 1.0);
get_material({sphere, _Center, _Radius, Material}, _HitPoint) ->
    Material;
get_material({cube, _Center, _Size, Material}, _HitPoint) ->
    Material;
get_material({cylinder, _Center, _Radius, _Height, Material}, _HitPoint) ->
    Material;
get_material({torus, _Center, _MajorR, _MinorR, Material}, _HitPoint) ->
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

%% Background gradient (sky)
background_color(Ray) ->
    {ray, _Origin, Direction} = Ray,
    {_X, Y, _Z} = vec3:normalize(Direction),

    %% Interpolate between blue (bottom) and white (top) based on Y
    T = 0.5 * (Y + 1.0),
    White = {1.0, 1.0, 1.0},
    Blue = {0.5, 0.7, 1.0},
    vec3:mix(White, Blue, T).
