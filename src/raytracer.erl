-module(raytracer).
-export([main/0, main/1]).

%% Main entry point for the raytracer
%% This is an educational CPU-based raytracer written entirely in Erlang
%% It renders a scene with multiple primitives and outputs a TGA image

%% Main function with default parameters
main() ->
    main([]).

%% Main function with command-line arguments
%% Usage: escript raytracer.erl [width] [height]
main(Args) ->
    io:format("~n=== Erlang Raytracer ===~n"),
    io:format("Educational CPU-based raytracer~n"),
    io:format("Written in pure Erlang with no dependencies~n~n"),

    %% Parse dimensions from arguments or use defaults
    {Width, Height} = parse_dimensions(Args),

    io:format("Image dimensions: ~px~p~n", [Width, Height]),
    io:format("Output file: output.tga~n~n"),

    %% Create the scene
    Scene = scene:default_scene(),

    %% Render the scene
    io:format("Starting rendering...~n"),
    StartTime = erlang:monotonic_time(millisecond),

    Pixels = renderer:render(Width, Height, Scene),

    EndTime = erlang:monotonic_time(millisecond),
    ElapsedMs = EndTime - StartTime,
    ElapsedSec = ElapsedMs / 1000.0,

    io:format("~nRendering took ~.2f seconds~n", [ElapsedSec]),

    %% Write TGA file
    io:format("Writing output.tga...~n"),
    case tga:write("output.tga", Width, Height, Pixels) of
        ok ->
            io:format("~nSuccess! Image saved to output.tga~n");
        {error, Reason} ->
            io:format("~nError writing file: ~p~n", [Reason])
    end,

    io:format("~nDone!~n~n").

%% Parse image dimensions from command-line arguments
parse_dimensions([]) ->
    %% Default: 800x450 (16:9 aspect ratio)
    {800, 450};
parse_dimensions([WidthStr]) ->
    Width = list_to_integer(WidthStr),
    Height = round(Width * 9 / 16),  % Maintain 16:9 aspect
    {Width, Height};
parse_dimensions([WidthStr, HeightStr]) ->
    Width = list_to_integer(WidthStr),
    Height = list_to_integer(HeightStr),
    {Width, Height};
parse_dimensions(_) ->
    io:format("Usage: escript raytracer.erl [width] [height]~n"),
    {800, 450}.
