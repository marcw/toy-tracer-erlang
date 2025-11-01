-module(cylinder).
-export([new/4, intersect/2, normal/2]).

%% Infinite cylinder primitive aligned along Y-axis
%% For a finite cylinder, we would also check Y bounds

%% Creates a new cylinder
%% Center: center position {X, Y, Z}
%% Radius: cylinder radius
%% Height: cylinder height (distance from center to top/bottom)
%% Material: material properties
new(Center, Radius, Height, Material) ->
    {cylinder, Center, Radius, Height, Material}.

%% Ray-cylinder intersection
%% Returns {true, Distance, Material} if hit, false otherwise
%%
%% Cylinder equation (aligned on Y-axis): (X - CX)² + (Z - CZ)² = R²
%% Ray: P(t) = Origin + t * Direction
%% Substituting and solving quadratic equation
intersect({cylinder, Center, Radius, Height, Material}, Ray) ->
    {ray, Origin, Direction} = Ray,
    {CX, CY, CZ} = Center,
    {OX, OY, OZ} = Origin,
    {DX, DY, DZ} = Direction,

    %% Project to XZ plane (ignore Y for infinite cylinder)
    A = DX * DX + DZ * DZ,
    B = 2.0 * ((OX - CX) * DX + (OZ - CZ) * DZ),
    C = (OX - CX) * (OX - CX) + (OZ - CZ) * (OZ - CZ) - Radius * Radius,

    %% Check if ray is parallel to cylinder axis
    if
        abs(A) < 0.0001 ->
            %% Ray parallel to Y-axis, check if inside cylinder
            if
                abs(C) < 0.0001 ->
                    %% Ray is inside cylinder, no intersection
                    false;
                true ->
                    false
            end;
        true ->
            %% Solve quadratic
            Discriminant = B * B - 4.0 * A * C,

            if
                Discriminant < 0.0 ->
                    %% No intersection
                    false;
                true ->
                    SqrtD = math:sqrt(Discriminant),
                    T1 = (-B - SqrtD) / (2.0 * A),
                    T2 = (-B + SqrtD) / (2.0 * A),

                    %% Check both intersection points
                    check_cylinder_hit(T1, T2, Ray, CY, Height, Material)
            end
    end.

%% Helper function to check if intersection is within height bounds
check_cylinder_hit(T1, T2, Ray, CenterY, Height, Material) ->
    %% Check nearest intersection first
    case check_t_value(T1, Ray, CenterY, Height) of
        true ->
            {true, T1, Material};
        false ->
            %% Try far intersection
            case check_t_value(T2, Ray, CenterY, Height) of
                true ->
                    {true, T2, Material};
                false ->
                    false
            end
    end.

%% Check if t value is valid and within cylinder height
check_t_value(T, Ray, CenterY, Height) ->
    if
        T > 0.001 ->
            %% Calculate Y coordinate at intersection
            {_X, Y, _Z} = ray:at(Ray, T),
            %% Check if within height bounds
            YDist = abs(Y - CenterY),
            YDist =< Height;
        true ->
            false
    end.

%% Calculate surface normal at a point on the cylinder
%% Normal is perpendicular to Y-axis
normal({cylinder, Center, _Radius, _Height, _Material}, Point) ->
    {CX, _CY, CZ} = Center,
    {PX, _PY, PZ} = Point,

    %% Normal is radial in XZ plane
    Normal = {PX - CX, 0.0, PZ - CZ},
    vec3:normalize(Normal).
