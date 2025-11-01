-module(plane).
-export([new/3, intersect/2, normal/2]).

%% Infinite plane primitive for raytracing
%% Defined by a point on the plane and a normal vector

%% Creates a new plane
%% Point: a point on the plane {X, Y, Z}
%% Normal: plane normal vector {X, Y, Z} (should be normalized)
%% Material: material properties (or 'checkerboard' for pattern)
new(Point, Normal, Material) ->
    {plane, Point, vec3:normalize(Normal), Material}.

%% Ray-plane intersection
%% Returns {true, Distance, Material} if hit, false otherwise
%%
%% Mathematical derivation:
%% Plane equation: (P - Point) · Normal = 0
%% Ray equation: P(t) = Origin + t * Direction
%% Substituting: (Origin + t*Direction - Point) · Normal = 0
%% Solving for t: t = (Point - Origin) · Normal / (Direction · Normal)
intersect({plane, Point, Normal, Material}, Ray) ->
    {ray, Origin, Direction} = Ray,

    %% Check if ray is parallel to plane
    Denom = vec3:dot(Direction, Normal),

    if
        abs(Denom) < 0.0001 ->
            %% Ray is parallel to plane (no intersection)
            false;
        true ->
            %% Calculate intersection distance
            T = vec3:dot(vec3:sub(Point, Origin), Normal) / Denom,

            %% Check if intersection is in front of ray origin
            if
                T > 0.001 ->
                    %% For checkerboard material, compute at hit point
                    case Material of
                        checkerboard ->
                            HitPoint = ray:at(Ray, T),
                            CheckerMat = material:checkerboard(HitPoint, 1.0),
                            {true, T, CheckerMat};
                        _ ->
                            {true, T, Material}
                    end;
                true ->
                    false
            end
    end.

%% Calculate surface normal at a point on the plane
%% For planes, the normal is constant everywhere
normal({plane, _Point, Normal, _Material}, _HitPoint) ->
    Normal.
