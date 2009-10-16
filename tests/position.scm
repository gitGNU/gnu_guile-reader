#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
LTDL_LIBRARY_PATH="`pwd`/.libs"
GUILE_AUTO_COMPILE=0
export LTDL_LIBRARY_PATH GUILE_AUTO_COMPILE
main='(module-ref (resolve-module '\''(test-position)) '\'main')'
exec ${GUILE-"${top_builddir-..}/pre-inst-guile"} -l $0  \
        -c "(apply $main (cdr (command-line)))" "$@"
!#
;;;
;;; Copyright 2006, 2009  Ludovic Courtès <ludo@gnu.org>
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

(define-module (test-position)
  :use-module (system reader)
  :use-module (system reader library))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Test whether position recording works well.
;;;
;;; Code:


(define (string->port str)
  (let ((port (open-input-string str)))
    (set-port-filename! port "the-file")
    (set-port-line! port 0)
    (set-port-column! port 0)
    port))

(define %test-cases
  ;; Input strings that read as pairs (source properties are only attached to
  ;; pairs).
  `("(+ 2 2)\n"
    "\n  (* 4 5)"
    "\n (\n (x) (y) 3)"
    "\t(\n\t  (x) 3\n)"
    "  \n\t(\n\t(  \t( (\n+ 2)\n))\t)"
    "(alarm \"\a\a\a\" (+ p q))"
    ,(string-append "   (backspace \"" (string #\bs) "\" (+ 6 7))")
    ,(string-append "(lots-of-backspaces \"" (make-string 40 #\bs)
		    "\" (+ a b))")
    "  \t  \r(reset)"))

(define (expected:line e) (vector-ref e 0))
(define (expected:column e) (vector-ref e 1))
(define (expected:subs e) (vector-ref e 2))

(define (expected-position str)
  ;; Return the expected position information (a vector) for STR.  This
  ;; assumes that `read' provides source position.
  (let ((sexp (read (string->port str))))
    (let loop ((sexp sexp))
      (cond ((and (pair? sexp) (not (null? sexp)))
	     (vector (source-property sexp 'line)
		     (source-property sexp 'column)
		     (map loop sexp)))
	    (else '#(#f #f ()))))))


(define (run-test read str expected)
  (let* ((port (string->port str)))
    (let loop ((sexp (read port))
	       (expected expected))
      (cond ((and (pair? sexp) (not (null? sexp)))
	     (format #t "sexp `~S'...~%" sexp)
	     (let ((line (source-property sexp 'line))
		   (column (source-property sexp 'column)))
	       (if (= line (expected:line expected))
		   (if (= column (expected:column expected))
		       (map loop sexp (expected:subs expected))
		       (error (format #f "~S: wrong column (expected ~a)"
				      sexp (expected:column expected))
			      column))
		   (error (format #f "~S: wrong line (expected ~a)"
				  sexp (expected:line expected))
			  line))))
	    (else #t)))))

(define-public (test-position . args)
  ;; This is required in order for `expected-position' to work.
  (read-enable 'positions)

  (let ((read (make-guile-reader #f 'reader/record-positions))
	(total 0)
	(failed 0))
    (for-each (lambda (str)
		(set! total (+ 1 total))
; 		(catch #t
; 		  (lambda ()
		    (run-test read str (expected-position str))
; 		    )
; 		  (lambda (key . args)
; 		    (set! failed (+ failed 1)))
;		  )
	      )
	      %test-cases)
    (format #t "failed: ~a~%total: ~a~%" failed total)
    (exit failed)))

(define main test-position)

;;; arch-tag: 8b38482c-488a-4473-8f5d-a8ca1a28585e

;;; test-position.scm ends here
