#!/bin/sh
# Drive the "approach one" static executable flow from
# README.static-executable end to end, using the tiny alien library in
# this directory instead of libgmp, so the whole static linking
# machinery can be tested with no external dependencies.
#
# The SBCL used to build the core and the runtime used for the static
# link must come from the same build (their build IDs must match), and
# that build must have been made with --with-sb-linkable-runtime and
# --with-sb-prelink-linkage-table.
#
# Environment:
#   SBCL        command to run the dynamic SBCL that saves the core
#               (default: sbcl). May be several words, e.g.
#               SBCL="./src/runtime/sbcl --core output/sbcl.core" in
#               an uninstalled build tree, or SBCL="sh run-sbcl.sh".
#   SBCL_LIBDIR directory containing the runtime of the same build:
#               sbcl.mk and the object(s) named by its LIBSBCL variable
#               (default: /usr/lib/sbcl; use src/runtime for an
#               uninstalled build tree)
#   SBCL_TOP    source tree containing tools-for-build/ and this
#               directory (default: two levels above this script)
#   SB_TINY_TEST_DIR  where to put intermediate files
#                     (default: /tmp/sb-tiny-static-test)
#
# Notes:
# * sb-tiny.lisp is loaded from the source tree (not via REQUIRE), so
#   the script works with an installed SBCL and with an uninstalled
#   build tree alike.
# * The dynamic SBCL that saves the core must be able to RESOLVE the
#   tiny_* symbols (the dumped linkage info must mark them defined;
#   the override then emits &tiny_* and the final static link resolves
#   them from libsb-tiny.a). The script achieves this by building
#   libsb-tiny.so as well and LD_PRELOADing it for that step, the way
#   the sb-gmp demo relies on libgmp being resolvable. (The final
#   static executable still links the static library, not the .so.)
#
# The exit status is 0 iff the resulting static executable ran
# (sb-tiny:tiny-test) successfully.

set -eu

SBCL=${SBCL:-sbcl}
SBCL_LIBDIR=${SBCL_LIBDIR:-/usr/lib/sbcl}
SBCL_TOP=$(cd "$(dirname "$0")/../.." && pwd)
B=${SB_TINY_TEST_DIR:-/tmp/sb-tiny-static-test}

for f in "$SBCL_LIBDIR/sbcl.mk" \
         "$SBCL_TOP/tools-for-build/dump-linkage-info.lisp" \
         "$SBCL_TOP/tools-for-build/create-linkage-table-prelink-info-override.lisp"; do
    if [ ! -f "$f" ]; then
        echo "error: required file $f not found" >&2
        exit 1
    fi
done
command -v "${SBCL%% *}" >/dev/null || { echo "error: SBCL command '$SBCL' not found" >&2; exit 1; }

rm -rf "$B"
mkdir -p "$B"

# Get all the variables SBCL used to build defined in the current
# environment.
while read l; do
    eval "${l%%=*}=\"${l#*=}\""
done < "$SBCL_LIBDIR/sbcl.mk"

echo "== building libsb-tiny.a and libsb-tiny.so"
$CC -O2 -fPIC -c "$SBCL_TOP/contrib/sb-tiny/tiny.c" -o "$B/tiny.o"
ar rcs "$B/libsb-tiny.a" "$B/tiny.o"
$CC -shared -o "$B/libsb-tiny.so" "$B/tiny.o"

echo "== steps 1-3: save a core that references the tiny library"
LD_PRELOAD="$B/libsb-tiny.so" $SBCL --non-interactive \
     --no-sysinit --no-userinit \
     --eval "(load #P\"$SBCL_TOP/contrib/sb-tiny/sb-tiny.lisp\")" \
     --load "$SBCL_TOP/tools-for-build/dump-linkage-info.lisp" \
     --eval "(sb-dump-linkage-info:dump-to-file \"$B/linkage-info.sexp\")" \
     --eval "(sb-ext:save-lisp-and-die \"$B/tiny.core\")"

echo "== step 4: generate the prelink table override"
$SBCL --no-sysinit --no-userinit \
     --script "$SBCL_TOP/tools-for-build/create-linkage-table-prelink-info-override.lisp" \
     "$B/linkage-info.sexp" "$B/linkage-table-prelink-info-override.c"

echo "== step 5: statically link the runtime against the override and libsb-tiny.a"
$CC -Wno-builtin-declaration-mismatch -o "$B/override.o" -c "$B/linkage-table-prelink-info-override.c"
$CC -no-pie -static $LINKFLAGS -o "$B/static-sbcl" \
    "$SBCL_LIBDIR/$LIBSBCL" "$B/override.o" -L"$B" -lsb-tiny $LIBS

echo "== step 6: load the core into the static runtime and dump the executable"
"$B/static-sbcl" --core "$B/tiny.core" \
                 --non-interactive \
                 --no-sysinit --no-userinit \
                 --eval "(sb-ext:save-lisp-and-die \"$B/sb-tiny-tester\" :executable t :toplevel (lambda () (if (sb-tiny:tiny-test) (exit) (exit 1))))"

echo "ldd: $(ldd "$B/sb-tiny-tester" 2>&1 | head -1)"
if "$B/sb-tiny-tester"; then
    echo "sb-tiny static test: PASS"
else
    echo "sb-tiny static test: FAIL" >&2
    exit 1
fi
