(in-package :cl-user)

(defun foreign-symbols-to-c (output-pathname sorted-symbols)
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

    #-win32
    (format output "uintptr_t __attribute__((weak)) alien_linkage_values[] = {~%")
    #+win32
    (format output "uintptr_t alien_linkage_values[] = {~%")

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
  (foreign-symbols-to-c (second args)
                        (with-open-file (s (first args))
                          (read s))))

(eval-when (:execute)
  (let ((args (cdr sb-ext:*posix-argv*)))
    (when args
      (let ((*print-pretty* nil))
        (main args)))))
