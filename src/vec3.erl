-module(vec3).
-compile({no_auto_import,[length/1]}).
-export([new/3, add/2, sub/2, mul/2, scalar_div/2, dot/2, cross/2,
         length/1, length_squared/1, normalize/1,
         clamp/3, mix/3, reflect/2, negate/1]).

%% 3D Vector operations for raytracer
%% Vectors are represented as {X, Y, Z} tuples for efficiency

%% Creates a new 3D vector
new(X, Y, Z) ->
    {X, Y, Z}.

%% Vector addition: V1 + V2
add({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 + X2, Y1 + Y2, Z1 + Z2}.

%% Vector subtraction: V1 - V2
sub({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 - X2, Y1 - Y2, Z1 - Z2}.

%% Scalar multiplication or component-wise multiplication
mul({X, Y, Z}, S) when is_number(S) ->
    {X * S, Y * S, Z * S};
mul({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 * X2, Y1 * Y2, Z1 * Z2}.

%% Scalar division
scalar_div({X, Y, Z}, S) when is_number(S), S /= 0 ->
    {X / S, Y / S, Z / S}.

%% Dot product: V1 · V2
dot({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    X1 * X2 + Y1 * Y2 + Z1 * Z2.

%% Cross product: V1 × V2
cross({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {Y1 * Z2 - Z1 * Y2,
     Z1 * X2 - X1 * Z2,
     X1 * Y2 - Y1 * X2}.

%% Squared length of vector (avoids sqrt for performance)
length_squared({X, Y, Z}) ->
    X * X + Y * Y + Z * Z.

%% Length (magnitude) of vector
length(V) ->
    math:sqrt(length_squared(V)).

%% Normalize vector to unit length
normalize(V) ->
    Len = length(V),
    case Len > 0.0000001 of
        true -> vec3:scalar_div(V, Len);
        false -> {0.0, 0.0, 0.0}
    end.

%% Clamp value between min and max
clamp(Value, Min, Max) when Value < Min ->
    Min;
clamp(Value, _Min, Max) when Value > Max ->
    Max;
clamp(Value, _Min, _Max) ->
    Value.

%% Linear interpolation: mix A and B by factor T (0.0 to 1.0)
mix({X1, Y1, Z1}, {X2, Y2, Z2}, T) ->
    {X1 + (X2 - X1) * T,
     Y1 + (Y2 - Y1) * T,
     Z1 + (Z2 - Z1) * T}.

%% Reflect vector V around normal N
%% Formula: V - 2 * dot(V, N) * N
reflect(V, N) ->
    Factor = 2.0 * dot(V, N),
    sub(V, mul(N, Factor)).

%% Negate vector
negate({X, Y, Z}) ->
    {-X, -Y, -Z}.
