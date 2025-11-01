-module(ray).
-export([new/2, at/2]).

%% Ray structure and operations
%% A ray is defined by an origin point and a direction vector
%% Parametric form: P(t) = Origin + t * Direction

%% Creates a new ray
%% Origin: starting point {X, Y, Z}
%% Direction: direction vector {X, Y, Z} (should be normalized)
new(Origin, Direction) ->
    {ray, Origin, Direction}.

%% Get point along ray at parameter t
%% Returns: Origin + t * Direction
at({ray, Origin, Direction}, T) ->
    vec3:add(Origin, vec3:mul(Direction, T)).
