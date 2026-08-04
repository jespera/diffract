#!/bin/bash
# Build tree-sitter grammar static libraries
#
# Two grammars are pinned to git rather than an npm release (see
# package.json): tree-sitter-kotlin, which has no usable release, and
# tree-sitter-scala, pinned to an exact sha for XML-literal support. No
# released tree-sitter-scala parses Scala's XML literals at all (0.24.0, the
# latest, has no xml rules), and a corpus that uses them parses badly enough
# to defeat matching entirely — on apache/daffodil, 62 of 312 files carried
# ERROR nodes before the pin and 3 after. Both pins ship a pre-generated
# src/parser.c, so no tree-sitter CLI is needed to build. Revisit when
# upstream cuts a release containing the XML work (PRs #604, #606).

set -e
cd "$(dirname "$0")"

# Ensure npm packages are installed
npm install

mkdir -p lib metadata
TMPDIR_LOCAL=$(mktemp -d)
trap "rm -rf $TMPDIR_LOCAL" EXIT

# Copy per-grammar node-types.json into metadata/, embedded into the
# library by lib/dune. Each file describes its language's node types,
# fields, supertype/subtype relations, and child constraints. NOTE: no
# code reads the embedding today — it is kept as groundwork for
# field-info / supertype use cases (see docs/grammar-metadata.md).
echo "Copying node-types.json metadata..."
cp node_modules/tree-sitter-typescript/typescript/src/node-types.json metadata/typescript.node-types.json
cp node_modules/tree-sitter-typescript/tsx/src/node-types.json        metadata/tsx.node-types.json
cp node_modules/tree-sitter-kotlin/src/node-types.json                metadata/kotlin.node-types.json
cp node_modules/tree-sitter-php/php_only/src/node-types.json          metadata/php.node-types.json
cp node_modules/tree-sitter-scala/src/node-types.json                 metadata/scala.node-types.json

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
