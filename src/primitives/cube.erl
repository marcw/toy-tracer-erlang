-module(cube).
-export([new/3, new/4, intersect/2, normal/2]).

%% Oriented bounding box (OBB) / Cube primitive
%% Implemented using the slab method (6 plane intersections)
%% Supports rotation via transformation matrix

%% Creates a new axis-aligned cube
%% Center: center position {X, Y, Z}
%% Size: half-size (distance from center to face)
%% Material: material properties
new(Center, Size, Material) ->
    {cube, Center, mat3:identity(), Size, Material}.

%% Creates a new oriented (rotated) cube
%% Center: center position {X, Y, Z}
%% Rotation: 3x3 rotation matrix (from mat3 module)
%% Size: half-size (distance from center to face)
%% Material: material properties
new(Center, Rotation, Size, Material) ->
    {cube, Center, Rotation, Size, Material}.

%% Ray-cube intersection using slab method
%% Returns {true, Distance, Material} if hit, false otherwise
%%
%% The slab method tests intersection with 6 planes (2 per axis)
%% For rotated cubes, we transform the ray into local space first
intersect({cube, Center, Rotation, Size, Material}, Ray) ->
    {ray, Origin, Direction} = Ray,

    %% Transform ray to cube's local space
    %% Use transpose of rotation matrix (inverse for orthogonal matrices)
    RotationInv = mat3:transpose(Rotation),
    LocalOrigin = mat3:mul_vec3(RotationInv, vec3:sub(Origin, Center)),
    LocalDirection = mat3:mul_vec3(RotationInv, Direction),

    {OX, OY, OZ} = LocalOrigin,
    {DX, DY, DZ} = LocalDirection,

    %% In local space, cube is centered at origin
    MinX = -Size,
    MaxX = Size,
    MinY = -Size,
    MaxY = Size,
    MinZ = -Size,
    MaxZ = Size,

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
%% For rotated cubes, transform to local space, compute normal, transform back
normal({cube, Center, Rotation, Size, _Material}, Point) ->
    %% Transform point to local space
    RotationInv = mat3:transpose(Rotation),
    LocalPoint = mat3:mul_vec3(RotationInv, vec3:sub(Point, Center)),
    {DX, DY, DZ} = LocalPoint,

    %% Find which face in local space
    AbsX = abs(DX),
    AbsY = abs(DY),
    AbsZ = abs(DZ),

    %% The largest component determines the face
    LocalNormal = if
        AbsX > AbsY, AbsX > AbsZ ->
            %% X face (left or right)
            if DX > 0 -> {1.0, 0.0, 0.0}; true -> {-1.0, 0.0, 0.0} end;
        AbsY > AbsZ ->
            %% Y face (top or bottom)
            if DY > 0 -> {0.0, 1.0, 0.0}; true -> {0.0, -1.0, 0.0} end;
        true ->
            %% Z face (front or back)
            if DZ > 0 -> {0.0, 0.0, 1.0}; true -> {0.0, 0.0, -1.0} end
    end,

    %% Transform normal back to world space
    %% For normals, we use the rotation matrix directly (not its inverse)
    mat3:mul_vec3(Rotation, LocalNormal).
