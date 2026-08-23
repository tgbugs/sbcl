(in-package :cl-user)

(defun foreign-symbols-to-c (output-pathname sorted-symbols weak-p)
  (with-open-file (output output-pathname
                          :direction :output
                          :if-exists :supersede)
    ;; Needed for uintptr_t. We use the raw uintptr_t as we don't want to have
    ;; to include any SBCL headers just to get at lispobj.
    (format output "#include <stdint.h>~%~%")

    ;; Write out the extern definitions. Everything is a void function (even
    ;; variables) because compilers don't like void variables. Remove
    ;; alien_linkage_values as we need to write to it, so we should use the
    ;; actual type.
    (format output "extern void ~{~A()~^, ~};~%~%"
            (remove "alien_linkage_values"
                    (mapcar #'first
                            (remove t sorted-symbols :key #'third))
                    :test #'equal))

    ;; Record the number of entries so os_link_runtime knows how many of the
    ;; loaded core's linkage-info entries are backed by this table.  A saved
    ;; core may have more (runtime-loaded symbols appended at the end); the
    ;; runtime resolves those by name instead of reading past the array.
    ;;
    ;; The strength of the two definitions depends on the consumer.  The
    ;; override object linked into a static executable (README, approach one,
    ;; step 5) must be STRONG: it shadows the build-time weak pair in the
    ;; installed runtime (sbcl.o), and the loaded core's linkage table is
    ;; exactly this table's length.  (A weak table would lose to sbcl.o's
    ;; first-defined weak one, and a strong build-time count would be a
    ;; multiple-definition error.)  The build-time file that make-target-2.sh
    ;; regenerates with this same script passes WEAK so that the override
    ;; pair can shadow it.
    (let ((weak (and weak-p #+win32 nil t)))
      (if weak
          (progn
            (format output "__attribute__((weak)) unsigned alien_linkage_table_n_warm = ~D;~%" (length sorted-symbols))
            (format output "uintptr_t __attribute__((weak)) alien_linkage_values[] = {~%"))
          (progn
            (format output "unsigned alien_linkage_table_n_warm = ~D;~%" (length sorted-symbols))
            (format output "uintptr_t alien_linkage_values[] = {~%"))))

    ;; One address per linkage-table entry, in linkage-index order, matching
    ;; the format that os_link_runtime consumes (and that the genesis
    ;; generator writes for the cold core): no leading count and no data/-1
    ;; markers -- the count and the data-vs-code distinction come from the
    ;; core's SB-SYS:*LINKAGE-INFO* table. Undefined entries (e.g. dlopen in
    ;; a static build) get 0 so the runtime resolves them at startup.
    (dolist (symbol sorted-symbols)
      (if (third symbol)
          (format output "  (uintptr_t)0,~%")
          (format output "  (uintptr_t)&~A,~%" (first symbol))))
    (format output "};~%")))

(defun main (&optional (args (cdr sb-ext:*posix-argv*)))
  ;; Usage: <input.sexp> <output.c> [weak]
  ;; With the "weak" argument the alien_linkage_values and
  ;; alien_linkage_table_n_warm definitions are emitted weak (that is what
  ;; make-target-2.sh wants for the build-time file, so that a strong
  ;; override object can shadow it later); without it they are strong (the
  ;; README override object).
  (foreign-symbols-to-c (second args)
                        (with-open-file (s (first args))
                          (read s))
                        (string= (or (third args) "") "weak")))

(eval-when (:execute)
  (let ((args (cdr sb-ext:*posix-argv*)))
    (when args
      (let ((*print-pretty* nil))
        (main args)))))
