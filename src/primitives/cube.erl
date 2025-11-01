-module(cube).
-export([new/3, intersect/2, normal/2]).

%% Axis-aligned bounding box (AABB) / Cube primitive
%% Implemented using the slab method (6 plane intersections)

%% Creates a new cube
%% Center: center position {X, Y, Z}
%% Size: half-size (distance from center to face)
%% Material: material properties
new(Center, Size, Material) ->
    {cube, Center, Size, Material}.

%% Ray-cube intersection using slab method
%% Returns {true, Distance, Material} if hit, false otherwise
%%
%% The slab method tests intersection with 6 planes (2 per axis)
%% and finds the overlapping interval
intersect({cube, Center, Size, Material}, Ray) ->
    {ray, Origin, Direction} = Ray,
    {DX, DY, DZ} = Direction,
    {CX, CY, CZ} = Center,
    {OX, OY, OZ} = Origin,

    %% Calculate cube bounds
    MinX = CX - Size,
    MaxX = CX + Size,
    MinY = CY - Size,
    MaxY = CY + Size,
    MinZ = CZ - Size,
    MaxZ = CZ + Size,

    %% Calculate t values for each slab (X, Y, Z)
    %% Handle near-zero direction components to avoid division by zero
    {TMinX, TMaxX} = if
        abs(DX) < 0.0001 ->
            %% Ray parallel to YZ plane
            if
                OX < MinX; OX > MaxX -> {1.0, -1.0};  % Miss
                true -> {-1.0e30, 1.0e30}  % Infinite range
            end;
        true ->
            T1 = (MinX - OX) / DX,
            T2 = (MaxX - OX) / DX,
            {min(T1, T2), max(T1, T2)}
    end,

    {TMinY, TMaxY} = if
        abs(DY) < 0.0001 ->
            if
                OY < MinY; OY > MaxY -> {1.0, -1.0};
                true -> {-1.0e30, 1.0e30}
            end;
        true ->
            T1Y = (MinY - OY) / DY,
            T2Y = (MaxY - OY) / DY,
            {min(T1Y, T2Y), max(T1Y, T2Y)}
    end,

    {TMinZ, TMaxZ} = if
        abs(DZ) < 0.0001 ->
            if
                OZ < MinZ; OZ > MaxZ -> {1.0, -1.0};
                true -> {-1.0e30, 1.0e30}
            end;
        true ->
            T1Z = (MinZ - OZ) / DZ,
            T2Z = (MaxZ - OZ) / DZ,
            {min(T1Z, T2Z), max(T1Z, T2Z)}
    end,

    %% Find the overlapping interval
    TMin = max(max(TMinX, TMinY), TMinZ),
    TMax = min(min(TMaxX, TMaxY), TMaxZ),

    %% Check if there's a valid intersection
    if
        TMax < 0.0 ->
            %% Cube is behind ray
            false;
        TMin > TMax ->
            %% No intersection
            false;
        TMin > 0.001 ->
            %% Hit the near face
            {true, TMin, Material};
        TMax > 0.001 ->
            %% Inside the cube, hit the far face
            {true, TMax, Material};
        true ->
            false
    end.

%% Calculate surface normal at a point on the cube
%% Determine which face was hit based on position
normal({cube, Center, Size, _Material}, Point) ->
    {PX, PY, PZ} = Point,
    {CX, CY, CZ} = Center,

    %% Find which axis is closest to a face
    DX = PX - CX,
    DY = PY - CY,
    DZ = PZ - CZ,

    %% Normalize to find which face
    AbsX = abs(DX),
    AbsY = abs(DY),
    AbsZ = abs(DZ),

    %% The largest component determines the face
    if
        AbsX > AbsY, AbsX > AbsZ ->
            %% X face (left or right)
            if DX > 0 -> {1.0, 0.0, 0.0}; true -> {-1.0, 0.0, 0.0} end;
        AbsY > AbsZ ->
            %% Y face (top or bottom)
            if DY > 0 -> {0.0, 1.0, 0.0}; true -> {0.0, -1.0, 0.0} end;
        true ->
            %% Z face (front or back)
            if DZ > 0 -> {0.0, 0.0, 1.0}; true -> {0.0, 0.0, -1.0} end
    end.
