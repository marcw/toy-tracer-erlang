-module(mat3).
-export([identity/0, from_euler/3, mul_vec3/2, transpose/1]).

%% 3x3 matrix operations for rotations
%% Matrix is stored as a tuple of 9 elements in row-major order:
%% {M00, M01, M02,
%%  M10, M11, M12,
%%  M20, M21, M22}

%% Identity matrix
identity() ->
    {1.0, 0.0, 0.0,
     0.0, 1.0, 0.0,
     0.0, 0.0, 1.0}.

%% Create rotation matrix from Euler angles (in degrees)
%% Rotation order: Z * Y * X (yaw * pitch * roll)
%% This matches the C implementation's mat3_from_euler
from_euler(PitchDeg, YawDeg, RollDeg) ->
    %% Convert degrees to radians
    Pitch = PitchDeg * math:pi() / 180.0,
    Yaw = YawDeg * math:pi() / 180.0,
    Roll = RollDeg * math:pi() / 180.0,

    %% Precompute sin/cos
    SP = math:sin(Pitch),
    CP = math:cos(Pitch),
    SY = math:sin(Yaw),
    CY = math:cos(Yaw),
    SR = math:sin(Roll),
    CR = math:cos(Roll),

    %% Combined rotation matrix (Z * Y * X order)
    %% Row 0
    M00 = CY * CR + SY * SP * SR,
    M01 = -CY * SR + SY * SP * CR,
    M02 = SY * CP,

    %% Row 1
    M10 = CP * SR,
    M11 = CP * CR,
    M12 = -SP,

    %% Row 2
    M20 = -SY * CR + CY * SP * SR,
    M21 = SY * SR + CY * SP * CR,
    M22 = CY * CP,

    {M00, M01, M02,
     M10, M11, M12,
     M20, M21, M22}.

%% Multiply matrix by vector: result = M * v
mul_vec3({M00, M01, M02,
          M10, M11, M12,
          M20, M21, M22}, {X, Y, Z}) ->
    {M00 * X + M01 * Y + M02 * Z,
     M10 * X + M11 * Y + M12 * Z,
     M20 * X + M21 * Y + M22 * Z}.

%% Transpose a matrix (swap rows and columns)
%% Useful for transforming normals and inverse rotations
transpose({M00, M01, M02,
           M10, M11, M12,
           M20, M21, M22}) ->
    {M00, M10, M20,
     M01, M11, M21,
     M02, M12, M22}.
