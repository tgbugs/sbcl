#!/bin/sh
set -em

# --load argument skips compilation.
#
# This is a script to be run as part of make.sh. The only time you'd
# want to run it by itself is if you're trying to cross-compile the
# system or if you're doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-target-2.sh

LANG=C
LC_ALL=C
export LANG LC_ALL

# Load our build configuration
. output/build-config

if [ -n "$SBCL_HOST_LOCATION" ]; then
    echo //copying host-2 files to target
    rsync -a "$SBCL_HOST_LOCATION/output/" output/
fi

# Build the runtime system.
#
# (This C build has to come after the first genesis in order to get
# 'sbcl.h' which the C build. It's done here, rather than in
# make-target-1.sh, because in a --with-sb-prelink-linkage-table build the
# second genesis (make-host-2) writes src/runtime/linkage-table-prelink-info.c,
# which the build compiles into the runtime, so the build must come after
# make-host-2.)
echo //building runtime system and symbol table file

$GNUMAKE -C src/runtime clean
$GNUMAKE $SBCL_MAKE_JOBS -C src/runtime all

# Do warm init stuff, e.g. building and loading CLOS, and stuff which
# can't be done until CLOS is running.
#
# Note that it's normal for the newborn system to think rather hard at
# the beginning of this process (e.g. using nearly 100Mb of virtual memory
# and >30 seconds of CPU time on a 450MHz CPU), and unless you built the
# system with the :SB-SHOW feature enabled, it does it rather silently,
# without trying to tell you about what it's doing. So unless it hangs
# for much longer than that, don't worry, it's likely to be normal.
warm_compile=yes
devel=""
if [ "$1" = --load ]; then
    warm_compile=no
elif [ "$1" = --load-with-sb-devel ]; then
    warm_compile=no
    devel="(pushnew :sb-devel *features*)"
elif [ "x$1" != x ]; then
    echo Unknown option \'"$1"\' to make-target-2
    exit 1
fi
if [ "$warm_compile" = yes ]; then
    echo //doing warm init - compilation phase
    ./src/runtime/sbcl --core output/cold-sbcl.core \
     --lose-on-corruption $SBCL_MAKE_TARGET_2_OPTIONS --no-sysinit --no-userinit \
     --eval '(sb-fasl::!warm-load "src/cold/warm.lisp")' --quit
fi
echo //doing warm init - load and dump phase
./src/runtime/sbcl --noinform --core output/cold-sbcl.core \
                   --lose-on-corruption $SBCL_MAKE_TARGET_2_OPTIONS \
                   --no-sysinit --no-userinit --noprint <<EOF
(progn ${devel})
(sb-fasl::!warm-load "make-target-2-load.lisp")
(setf (extern-alien "gc_coalesce_string_literals" char) 2)
;;; Use the historical (bad) convention for *compile-file-pathname*
(setf sb-c::*merge-pathnames* t)
;;; and for storing pathname namestrings in fasls too.
(setq sb-c::*name-context-file-path-selector* 'truename)
; Turn off IR consistency checking in release mode.
(setq sb-c::*check-consistency* nil)
;; In a --with-sb-prelink-linkage-table build the prelink table was written
;; during the second genesis, i.e. before this warm init, so it only covers the
;; cold core's foreign symbols. The warm core has more (those loaded during
;; warm init), and the runtime links the warm core against the prelink table by
;; linkage index, so it needs an entry for every one of them. Dump the full
;; linkage table here, just before the core is saved, so it matches the core.
;;
;; A --with-sb-prelink-linkage-table build is recognized by the presence of
;; src/runtime/linkage-table-prelink-info.c, which the second genesis wrote
;; just before this warm init. (:sb-prelink-linkage-table itself is a
;; cross-compile-only feature and is not present in the loaded core's
;; *FEATURES*, so it cannot be tested here.)
;; Load the dump helper unconditionally (it is present in every build that
;; carries this series and has no side effects) so the SB-DUMP-LINKAGE-INFO
;; package exists before the reader reaches the qualified symbol below; the
;; reader resolves that symbol when it reads the next form, which happens only
;; after this load has been evaluated. The dump itself runs only in a
;; --with-sb-prelink-linkage-table build, recognized by the prelink file the
;; second genesis wrote just before this warm init.
(format t "DEBUG2PASS: src=~S listing=~{~A~^ ~}~%" (probe-file "src/runtime/linkage-table-prelink-info.c") (directory "src/runtime/linkage*"))
(load "tools-for-build/dump-linkage-info.lisp")
(when (probe-file "src/runtime/linkage-table-prelink-info.c")
  (sb-dump-linkage-info:dump-to-file "output/linkage-table-full.sexp"))
(let ((sb-ext:*invoke-debugger-hook* (prog1 sb-ext:*invoke-debugger-hook* (sb-ext:enable-debugger))))
 (sb-ext:save-lisp-and-die "output/sbcl.core"))
EOF

# Finish the prelink table (see the dump above). Regenerate it from the full
# linkage table that was just dumped, and rebuild the runtime so the final
# system prelinks the warm core's foreign symbols as well as the cold core's.
# The dump file only exists in a --with-sb-prelink-linkage-table build.
# The table is regenerated with the cold core, not the warm one, because the
# current runtime's prelink table is still the cold-sized one.
# (Alternatively this could be done manually between these two stages: dump the
# table with tools-for-build/dump-linkage-info.lisp, regenerate the prelink
# file with
# tools-for-build/create-linkage-table-prelink-info-override.lisp (passing "weak" as the
# third argument so the definitions are weak and a strong override object can
# shadow them), and rebuild with "make -C src/runtime all".)
#
# The "weak" argument below is essential: the generated file becomes part of
# sbcl.o, and in the README approach-one static link the override object must
# win over it for both alien_linkage_values and alien_linkage_table_n_warm.
if [ -f output/linkage-table-full.sexp ]; then
    echo //regenerating prelink linkage table for the warm core
    ./src/runtime/sbcl --core output/cold-sbcl.core \
                       --no-sysinit --no-userinit \
                       --script tools-for-build/create-linkage-table-prelink-info-override.lisp \
                       output/linkage-table-full.sexp src/runtime/linkage-table-prelink-info.c weak
    $GNUMAKE $SBCL_MAKE_JOBS -C src/runtime all
fi

./src/runtime/sbcl --noinform --core output/sbcl.core \
                   --no-sysinit --no-userinit --noprint <<EOF
  (load "validate-float.lisp")
  (check-float-file "output/xfloat-math.lisp-expr")
  (ignore-errors (delete-file "output/reorg.core"))
  ;; * Lisp won't read compressed cores, and crashes on arm64
  #+(and mark-region-gc x86-64 (not sb-core-compression))
  (progn
   (load "tools-for-build/editcore")
   (funcall (intern "REORGANIZE-CORE" "SB-EDITCORE") "output/sbcl.core" "output/reorg.core"))
EOF
if [ -r output/reorg.core ]
then
    mv output/reorg.core output/sbcl.core
fi
