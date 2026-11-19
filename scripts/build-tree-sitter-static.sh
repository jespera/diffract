#!/bin/bash
# Build the tree-sitter runtime as a static library into a private prefix.
#
#   scripts/build-tree-sitter-static.sh <version> <prefix>
#
# The prefix gets only libtree-sitter.a, the header and a pkg-config file, so
# with PKG_CONFIG_PATH=<prefix>/lib/pkgconfig the `pkg-config --libs
# tree-sitter` in lib/dune can only resolve to the static archive. Release
# binaries then carry the runtime instead of needing a matching
# libtree-sitter installed. Honours $CC (musl-gcc for the static Linux build).

set -euo pipefail

VERSION=${1:?usage: $0 <version> <prefix>}
PREFIX=${2:?usage: $0 <version> <prefix>}
mkdir -p "$PREFIX"
PREFIX=$(cd "$PREFIX" && pwd)

SRC=$(mktemp -d)
trap 'rm -rf "$SRC"' EXIT

git clone --quiet --depth 1 --branch "v$VERSION" \
  https://github.com/tree-sitter/tree-sitter "$SRC"
make -C "$SRC" libtree-sitter.a CC="${CC:-cc}"

mkdir -p "$PREFIX/lib/pkgconfig" "$PREFIX/include/tree_sitter"
cp "$SRC/libtree-sitter.a" "$PREFIX/lib/"
cp "$SRC/lib/include/tree_sitter/api.h" "$PREFIX/include/tree_sitter/"
cat > "$PREFIX/lib/pkgconfig/tree-sitter.pc" <<PC
prefix=$PREFIX
libdir=\${prefix}/lib
includedir=\${prefix}/include

Name: tree-sitter
Description: tree-sitter runtime (static, for diffract release builds)
Version: $VERSION
Libs: -L\${libdir} -ltree-sitter
Cflags: -I\${includedir}
PC

echo "tree-sitter $VERSION (static) installed in $PREFIX"
