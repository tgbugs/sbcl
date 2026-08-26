;;; -*- Lisp -*-

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage :sb-tiny
  (:documentation "A tiny alien library for testing the static executable flow")
  (:use :common-lisp :sb-alien)
  (:export "TINY-ADD" "TINY-MUL" "TINY-ANSWER" "TINY-TEST"))

(in-package :sb-tiny)

;;;; The foreign functions and variable below are implemented in tiny.c
;;;; in this directory, which is meant to be built as a plain static
;;;; library (libsb-tiny.a) and is *not* built by the SBCL build
;;;; system.  The symbols only resolve in a runtime that has them
;;;; linked in, e.g. the static executable described in
;;;; README.static-executable, where the system linker resolves them
;;;; from libsb-tiny.a.  See test-static.sh in this directory for a
;;;; self-contained driver for that flow.
;;;;
;;;; TINY-ADD and TINY-MUL are the functions defined by the
;;;; DEFINE-ALIEN-ROUTINE forms below (the Lisp name is derived from
;;;; the foreign name), and TINY-ANSWER is the foreign variable
;;;; defined by the DEFINE-ALIEN-VARIABLE form.

(define-alien-routine "tiny_add" int
  "Add A and B using the foreign tiny_add function (tiny.c in this
directory)."
  (a int) (b int))

(define-alien-routine "tiny_mul" int
  "Multiply A and B using the foreign tiny_mul function."
  (a int) (b int))

(define-alien-variable "tiny_answer" int)

(defun tiny-test ()
  "Call every foreign symbol in this module; return T iff all the
results are as expected."
  (and (eq (tiny-add 2 3) 5)
       (eq (tiny-mul 6 7) 42)
       (eq tiny-answer 42)))
