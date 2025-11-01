-module(camera).
-export([new/4, get_ray/3]).

%% Simple pinhole camera for generating rays through pixels
%% Camera looks down the +Z axis by default

%% Creates a new camera
%% Position: camera position in world space
%% LookAt: point the camera looks at
%% Fov: vertical field of view in degrees
%% AspectRatio: width / height
new(Position, LookAt, Fov, AspectRatio) ->
    %% Calculate camera basis vectors
    %% Forward is from camera to look-at point
    Forward = vec3:normalize(vec3:sub(LookAt, Position)),

    %% World up vector
    WorldUp = {0.0, 1.0, 0.0},

    %% Right vector (perpendicular to forward and up)
    Right = vec3:normalize(vec3:cross(Forward, WorldUp)),

    %% Recalculate up to ensure orthogonal basis
    Up = vec3:cross(Right, Forward),

    %% Calculate viewport dimensions based on FOV
    %% Convert FOV from degrees to radians
    FovRad = Fov * math:pi() / 180.0,
    ViewportHeight = 2.0 * math:tan(FovRad / 2.0),
    ViewportWidth = ViewportHeight * AspectRatio,

    %% Viewport vectors
    ViewportU = vec3:mul(Right, ViewportWidth),
    ViewportV = vec3:mul(Up, ViewportHeight),

    {camera, Position, Forward, ViewportU, ViewportV}.

%% Generate a ray through pixel (U, V) where U and V are in [0, 1]
%% U=0, V=0 is top-left; U=1, V=1 is bottom-right
get_ray({camera, Position, Forward, ViewportU, ViewportV}, U, V) ->
    %% Convert from [0,1] to [-0.5, 0.5] centered coordinates
    UOffset = U - 0.5,
    VOffset = 0.5 - V,  % Flip V because image origin is top-left

    %% Calculate point on viewport
    %% Start at camera forward direction, then offset by U and V
    Horizontal = vec3:mul(ViewportU, UOffset),
    Vertical = vec3:mul(ViewportV, VOffset),

    %% Ray direction points from camera through the viewport point
    Direction = vec3:normalize(
        vec3:add(Forward, vec3:add(Horizontal, Vertical))
    ),

    ray:new(Position, Direction).
