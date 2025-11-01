# Erlang Raytracer

A fully functional CPU-based raytracer written entirely in **pure Erlang** with zero external dependencies.

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
