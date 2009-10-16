#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
LTDL_LIBRARY_PATH="`pwd`/.libs"
GUILE_AUTO_COMPILE=0
export LTDL_LIBRARY_PATH GUILE_AUTO_COMPILE
main='(module-ref (resolve-module '\''(test-library)) '\'main')'
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

(define-module (test-library)
  :use-module (system reader library)
  :use-module (srfi srfi-1))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Tests `make-alternate-guile-reader' with various options.
;;;
;;; Code:

(define %test-cases
  `(#(()               "#:kw"        ,keyword?)
    #(()               ":kw"         ,symbol?)
    #(()               "#! SCSH comment \n!#\n hello" ,symbol?)
    #((colon-keywords) "(:kw #:kw)"  ,(lambda (x) (every keyword? x)))
    #((dsssl-keywords) "(#!kw #:kw)" ,(lambda (x) (every keyword? x)))
    #((colon-keywords dsssl-keywords) "(#!kw :kw #:kw)"
      ,(lambda (x) (every keyword? x)))
    #((no-sharp-keywords) "#:kw"     (unhandled . #\:))
    #((case-insensitive)  "HelloWorld"
      ,(lambda (x) (eq? x 'helloworld)))
    #((colon-keywords case-insensitive) ":Kw" ,(lambda (x) (eq? x #:kw)))
    #((no-scsh-block-comments) "\n#! SCSH comment\n!#\n something"
      (unhandled . #\!))
    #((srfi30-block-comments) "(0 1 #| a comment |# 2)"
      ,(lambda (x) (equal? x '(0 1 2))))
    #((srfi62-sexp-comments)  "(0 1 #; 2 3)"
      ,(lambda (x) (equal? x '(0 1 3))))))

(define (test:opts t) (vector-ref t 0))
(define (test:string t) (vector-ref t 1))
(define (test:predicate t) (vector-ref t 2))


(define (test-with-options opts str pred)
  (let* ((handled? #t)
	 (fault-handler (lambda (chr port reader)
			  (set! handled? #f) chr))
	 (reader (make-alternate-guile-reader opts fault-handler)))
    (catch #t
      (lambda ()
	(let ((result (with-input-from-string str reader)))
	  (if (procedure? pred)
	      (pred result)
	      (case (car pred)
		;; Test was expected to yield an unhandled-character
		;; exception.
		((unhandled)
		 (and (not handled?) (eq? (cdr pred) result)))

		(else result)))))
      (lambda (key . args)
	(eq? pred 'exception-raised)))))

(define-public (test-library . args)
  (let ((summary (fold (lambda (test summary)
			 (let ((total (car summary))
			       (failed (cdr summary)))
			   (cons (+ 1 total)
				 (if (test-with-options (test:opts test)
							(test:string test)
							(test:predicate test))
				     failed
				     (begin
				       (format #t
					       "`~a' with string ~S FAILED~%"
					       (test:opts test)
					       (test:string test))
				       (+ 1 failed))))))
		       '(0 . 0)
		       %test-cases)))
    (format #t "total: ~a~%" (car summary))
    (format #t "failed: ~a~%" (cdr summary))
    (exit (cdr summary))))

(define main test-library)

;;; arch-tag: 77fd870b-b867-4e57-bf94-f52fd9a803bb

;;; library.scm ends here
