#!/bin/bash
# Build script for Erlang Raytracer
# No external dependencies or build systems required

echo "Compiling Erlang Raytracer..."

# Create output directory for compiled beam files
mkdir -p ebin

# Compile all modules
erlc -o ebin src/*.erl src/primitives/*.erl

if [ $? -eq 0 ]; then
    echo "Compilation successful!"
    echo ""
    echo "To run the raytracer:"
    echo "  erl -pa ebin -noshell -s raytracer main -s init stop"
    echo ""
    echo "Or with custom dimensions:"
    echo "  erl -pa ebin -noshell -eval 'raytracer:main([\"800\", \"600\"])' -s init stop"
else
    echo "Compilation failed!"
    exit 1
fi
