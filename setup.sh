#!/bin/bash
# Setup script for diffract
# Installs dependencies and builds grammar shared libraries

set -e
cd "$(dirname "$0")"

echo "=== Setting up diffract ==="

# Check for required tools
command -v npm >/dev/null 2>&1 || { echo "npm is required but not installed. Aborting." >&2; exit 1; }
command -v cc >/dev/null 2>&1 || { echo "C compiler (cc) is required but not installed. Aborting." >&2; exit 1; }

# Check for tree-sitter library
if ! pkg-config --exists tree-sitter 2>/dev/null && ! [ -f /usr/include/tree_sitter/api.h ]; then
    echo "tree-sitter library not found. Please install it:"
    echo "  Arch Linux: pacman -S tree-sitter"
    echo "  Ubuntu/Debian: apt install libtree-sitter-dev"
    echo "  macOS: brew install tree-sitter"
    exit 1
fi

# Install npm dependencies for grammars
echo "Installing tree-sitter grammar packages..."
(cd grammars && npm install)

# Build grammar shared libraries
echo "Building grammar shared libraries..."
./grammars/build-grammars.sh

echo ""
echo "=== Setup complete ==="
echo "Build the project with: dune build"
echo "Run with: dune exec diffract -- <file>"
