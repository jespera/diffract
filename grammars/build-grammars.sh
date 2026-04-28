#!/bin/bash
# Build tree-sitter grammar static libraries

set -e
cd "$(dirname "$0")"

# Ensure npm packages are installed
npm install

mkdir -p lib
TMPDIR_LOCAL=$(mktemp -d)
trap "rm -rf $TMPDIR_LOCAL" EXIT

# Build TypeScript grammar
echo "Building TypeScript grammar..."
cc -O2 -c -o "$TMPDIR_LOCAL/ts_parser.o" \
  -I node_modules/tree-sitter-typescript/typescript/src \
  node_modules/tree-sitter-typescript/typescript/src/parser.c
cc -O2 -c -o "$TMPDIR_LOCAL/ts_scanner.o" \
  -I node_modules/tree-sitter-typescript/typescript/src \
  node_modules/tree-sitter-typescript/typescript/src/scanner.c
ar rcs lib/libtree-sitter-typescript.a "$TMPDIR_LOCAL/ts_parser.o" "$TMPDIR_LOCAL/ts_scanner.o"

# Build TSX grammar (TypeScript with JSX)
echo "Building TSX grammar..."
cc -O2 -c -o "$TMPDIR_LOCAL/tsx_parser.o" \
  -I node_modules/tree-sitter-typescript/tsx/src \
  node_modules/tree-sitter-typescript/tsx/src/parser.c
cc -O2 -c -o "$TMPDIR_LOCAL/tsx_scanner.o" \
  -I node_modules/tree-sitter-typescript/tsx/src \
  node_modules/tree-sitter-typescript/tsx/src/scanner.c
ar rcs lib/libtree-sitter-tsx.a "$TMPDIR_LOCAL/tsx_parser.o" "$TMPDIR_LOCAL/tsx_scanner.o"

# Build Kotlin grammar
echo "Building Kotlin grammar..."
cc -O2 -c -o "$TMPDIR_LOCAL/kotlin_parser.o" \
  -I node_modules/tree-sitter-kotlin/src \
  node_modules/tree-sitter-kotlin/src/parser.c
cc -O2 -c -o "$TMPDIR_LOCAL/kotlin_scanner.o" \
  -I node_modules/tree-sitter-kotlin/src \
  node_modules/tree-sitter-kotlin/src/scanner.c
ar rcs lib/libtree-sitter-kotlin.a "$TMPDIR_LOCAL/kotlin_parser.o" "$TMPDIR_LOCAL/kotlin_scanner.o"

# Build PHP grammar
echo "Building PHP grammar..."
cc -O2 -c -o "$TMPDIR_LOCAL/php_parser.o" \
  -I node_modules/tree-sitter-php/php_only/src \
  node_modules/tree-sitter-php/php_only/src/parser.c
cc -O2 -c -o "$TMPDIR_LOCAL/php_scanner.o" \
  -I node_modules/tree-sitter-php/php_only/src \
  node_modules/tree-sitter-php/php_only/src/scanner.c
ar rcs lib/libtree-sitter-php.a "$TMPDIR_LOCAL/php_parser.o" "$TMPDIR_LOCAL/php_scanner.o"

# Build Scala grammar
echo "Building Scala grammar..."
cc -O2 -c -o "$TMPDIR_LOCAL/scala_parser.o" \
  -I node_modules/tree-sitter-scala/src \
  node_modules/tree-sitter-scala/src/parser.c
cc -O2 -c -o "$TMPDIR_LOCAL/scala_scanner.o" \
  -I node_modules/tree-sitter-scala/src \
  node_modules/tree-sitter-scala/src/scanner.c
ar rcs lib/libtree-sitter-scala.a "$TMPDIR_LOCAL/scala_parser.o" "$TMPDIR_LOCAL/scala_scanner.o"

echo "Grammar libraries built in grammars/lib/"
ls -la lib/
