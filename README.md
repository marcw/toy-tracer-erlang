# Erlang Raytracer

A CPU-based raytracer written entirely in **pure Erlang** with zero external dependencies, by Claude Sonnet 4.5.

## But... why?

I... am sorry. This should not exist.

I just wanted to know whether it was possible for Claude Sonnet 4.5 to one-shot the implementation of a Raytracer in a language that should not be used for that.

The project is based on [astrofra/toy-tracer-c](https://github.com/astrofra/toy-tracer-c) which is an implementation by an Agent, in C.

The outputs are dramatically different. The erlang implementation doesn't seem to handle lightning and material correctly.

Reference C output:

![output-reference.tga](output-reference.tga)

Erlang output:

![output.tga](output.tga)

## Quick Start

```bash
# Compile
mkdir -p ebin
erlc -o ebin src/*.erl src/primitives/*.erl

# Run (default 800x450)
erl -pa ebin -noshell -s raytracer main -s init stop

# Quick test (200x112)
erl -pa ebin -noshell -eval 'raytracer:main(["200", "112"])' -s init stop
```

Output: `output.tga` (view with any image viewer)
