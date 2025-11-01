-module(sphere).
-export([new/3, intersect/2, normal/2]).

%% Sphere primitive for raytracing
%% Defined by center point and radius

%% Creates a new sphere
%% Center: position {X, Y, Z}
%% Radius: sphere radius
%% Material: material properties
new(Center, Radius, Material) ->
    {sphere, Center, Radius, Material}.

%% Ray-sphere intersection using quadratic formula
%% Returns {true, Distance, Material} if hit, false otherwise
%%
%% Mathematical derivation:
%% Ray: P(t) = Origin + t * Direction
%% Sphere: |P - Center|² = Radius²
%% Substituting: |Origin + t*Direction - Center|² = Radius²
%% This expands to: at² + bt + c = 0 (quadratic equation)
intersect({sphere, Center, Radius, Material}, Ray) ->
    {ray, Origin, Direction} = Ray,

    %% Vector from ray origin to sphere center
    OC = vec3:sub(Origin, Center),

    %% Quadratic coefficients
    %% a = Direction · Direction (should be 1.0 if Direction is normalized)
    A = vec3:dot(Direction, Direction),
    %% b = 2 * (Direction · OC)
    HalfB = vec3:dot(Direction, OC),
    %% c = |OC|² - Radius²
    C = vec3:length_squared(OC) - Radius * Radius,

    %% Discriminant determines if ray hits sphere
    %% Discriminant = b² - 4ac, but we use half_b optimization
    Discriminant = HalfB * HalfB - A * C,

    if
        Discriminant < 0.0 ->
            %% No intersection
            false;
        true ->
            %% Calculate nearest intersection point
            SqrtD = math:sqrt(Discriminant),
            Root = (-HalfB - SqrtD) / A,

            %% Check if intersection is in valid range (t > 0.001 to avoid self-intersection)
            if
                Root > 0.001 ->
                    {true, Root, Material};
                true ->
                    %% Try far intersection
                    Root2 = (-HalfB + SqrtD) / A,
                    if
                        Root2 > 0.001 ->
                            {true, Root2, Material};
                        true ->
                            false
                    end
            end
    end.

%% Calculate surface normal at a point on the sphere
%% Normal points outward from center
normal({sphere, Center, Radius, _Material}, Point) ->
    vec3:normalize(vec3:sub(Point, Center)).
