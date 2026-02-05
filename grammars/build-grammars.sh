#!/bin/bash
# Build tree-sitter grammar shared libraries locally

set -e
cd "$(dirname "$0")"

# Ensure npm packages are installed
npm install

mkdir -p lib

# Build TypeScript grammar
echo "Building TypeScript grammar..."
cc -shared -fPIC -o lib/libtree-sitter-typescript.so \
  -I node_modules/tree-sitter-typescript/typescript/src \
  node_modules/tree-sitter-typescript/typescript/src/parser.c \
  node_modules/tree-sitter-typescript/typescript/src/scanner.c

# Build TSX grammar (TypeScript with JSX)
echo "Building TSX grammar..."
cc -shared -fPIC -o lib/libtree-sitter-tsx.so \
  -I node_modules/tree-sitter-typescript/tsx/src \
  node_modules/tree-sitter-typescript/tsx/src/parser.c \
  node_modules/tree-sitter-typescript/tsx/src/scanner.c

# Build Kotlin grammar
echo "Building Kotlin grammar..."
cc -shared -fPIC -o lib/libtree-sitter-kotlin.so \
  -I node_modules/tree-sitter-kotlin/src \
  node_modules/tree-sitter-kotlin/src/parser.c \
  node_modules/tree-sitter-kotlin/src/scanner.c

# Build PHP grammar
echo "Building PHP grammar..."
cc -shared -fPIC -o lib/libtree-sitter-php.so \
  -I node_modules/tree-sitter-php/php/src \
  node_modules/tree-sitter-php/php/src/parser.c \
  node_modules/tree-sitter-php/php/src/scanner.c

# Build Scala grammar
echo "Building Scala grammar..."
cc -shared -fPIC -o lib/libtree-sitter-scala.so \
  -I node_modules/tree-sitter-scala/src \
  node_modules/tree-sitter-scala/src/parser.c \
  node_modules/tree-sitter-scala/src/scanner.c

echo "Grammar libraries built in grammars/lib/"
ls -la lib/
