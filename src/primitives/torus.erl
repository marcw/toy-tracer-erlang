-module(torus).
-export([new/4, intersect/2, normal/2]).

%% Torus primitive for raytracing
%% This is the most complex primitive, requiring solving a quartic equation
%% A torus is defined by major radius R (from center to tube center)
%% and minor radius r (tube radius)

%% Creates a new torus
%% Center: center position {X, Y, Z}
%% MajorRadius: distance from torus center to tube center
%% MinorRadius: tube radius
%% Material: material properties
new(Center, MajorRadius, MinorRadius, Material) ->
    {torus, Center, MajorRadius, MinorRadius, Material}.

%% Ray-torus intersection
%% Returns {true, Distance, Material} if hit, false otherwise
%%
%% This is complex because it requires solving a quartic (4th degree) equation
%% For educational purposes, we use an iterative numerical approach
%% rather than implementing a full quartic solver
intersect({torus, Center, MajorR, MinorR, Material}, Ray) ->
    {ray, Origin, Direction} = Ray,

    %% Transform ray to torus local space
    LocalOrigin = vec3:sub(Origin, Center),

    %% Use numerical ray marching as a simplified approach
    %% This is more stable than solving the quartic analytically
    march_torus(LocalOrigin, Direction, MajorR, MinorR, Material, 0.001, 0.0, 100).

%% Numerical ray marching to find torus intersection
%% We step along the ray and check the signed distance function
march_torus(_Origin, _Direction, _MajorR, _MinorR, _Material, _Epsilon, T, 0) ->
    %% Max iterations reached, no intersection
    false;
march_torus(Origin, Direction, MajorR, MinorR, Material, Epsilon, T, Iterations) ->
    if
        T > 1000.0 ->
            %% Too far, stop searching
            false;
        true ->
            %% Current point along ray
            Point = vec3:add(Origin, vec3:mul(Direction, T)),

            %% Signed distance to torus surface
            Dist = torus_sdf(Point, MajorR, MinorR),

            if
                Dist < Epsilon ->
                    %% Hit the surface
                    if
                        T > 0.001 ->
                            {true, T, Material};
                        true ->
                            %% Too close to origin, continue
                            march_torus(Origin, Direction, MajorR, MinorR, Material,
                                      Epsilon, T + Epsilon, Iterations - 1)
                    end;
                true ->
                    %% March forward by the distance
                    march_torus(Origin, Direction, MajorR, MinorR, Material,
                              Epsilon, T + Dist, Iterations - 1)
            end
    end.

%% Signed Distance Function for torus
%% Returns the distance from a point to the torus surface
torus_sdf({X, Y, Z}, MajorR, MinorR) ->
    %% Distance from point to Y-axis in XZ plane
    Q = math:sqrt(X * X + Z * Z) - MajorR,
    %% Distance to torus surface
    math:sqrt(Q * Q + Y * Y) - MinorR.

%% Calculate surface normal at a point on the torus
%% Use gradient of the signed distance function
normal({torus, Center, MajorR, MinorR, _Material}, Point) ->
    %% Transform to local space
    LocalPoint = vec3:sub(Point, Center),
    {X, Y, Z} = LocalPoint,

    %% Compute normal using analytical gradient
    %% This is the gradient of the torus SDF
    QX = math:sqrt(X * X + Z * Z),

    %% Handle the case where we're exactly on the Y-axis
    if
        QX < 0.0001 ->
            %% Degenerate case, use approximate normal
            {0.0, 1.0, 0.0};
        true ->
            Factor = 1.0 - MajorR / QX,
            Normal = {X * Factor, Y, Z * Factor},
            vec3:normalize(Normal)
    end.
