#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(extract-doc)) '\'main')'
exec ${GUILE-../guile} -L ../module -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#
;;;
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
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


(define-module (extract-doc)
  #:use-module (system reader)
  #:use-module (system reader library)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 documentation))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Extract documentation of the standard token readers and print them in a
;;; Texinfo-friendly form so that they can be included in the manual.
;;;
;;; Code:

(define (extract-token-reader-doc)
  (define (show-char-escaped chr)
    (string-append "#\\"
		   (case chr
		     ((#\{ #\} #\\ #\@) "@")
		     (else ""))
		   (string-drop (with-output-to-string
				  (lambda ()
				    (write chr)))
				2)))

  (define (char-spec-doc spec)
    (cond ((char? spec)
	   (format #f "@code{~a}" (show-char-escaped spec)))
	  ((list? spec)
	   (let ((len (length spec)))
	     (format #f "~a characters, @code{~a}... @code{~a}"
		     len (show-char-escaped (car spec))
		     (show-char-escaped (car (drop spec (- len 1)))))))
	  ((pair? spec)
	   (format #f "from @code{~a} to @code{~a}"
		   (show-char-escaped (car spec))
		   (show-char-escaped (cdr spec))))
	  (else
	   (error "invalid character specification" spec))))

  (format #t "~%@multitable @c @columnfractions .25 .25 .50~%")
  (format #t "@headitem Token Reader~%@tab Character Spec.~%@tab Description~%~%")
  (for-each (lambda (tr-name)
	      (let* ((tr (standard-token-reader tr-name))
		     (spec-doc (char-spec-doc (token-reader-specification tr)))
		     (doc (token-reader-documentation tr)))
		(format #t "@item @code{~a}~%@tab ~a~%@tab ~a~%"
			tr-name spec-doc doc)))
	    (sort (standard-token-reader-names)
		  (lambda (s1 s2)
		    (string<=? (symbol->string s1)
			       (symbol->string s2)))))
  (format #t "~%@end multitable~%"))

(define (output-args args)
  (let loop ((args args))
    (cond ((null? args) (format #t "~%"))
	  ((list? args)
	   (format #t "~a " (car args))
	   (loop (cdr args)))
	  ((pair? args)
	   (format #t "~a ~a@dots{}~%" (car args) (cdr args))))))

(define (extract-module-doc module)
  (module-for-each (lambda (binding var)
		     (let* ((obj (module-ref module binding))
			    (doc (object-documentation obj)))
		       (if (procedure? obj)
			   (let* ((src (procedure-source obj))
				  (args (if src (cadr src) '())))
			     (format #t "~%@deffn {Scheme Procedure} ~a "
				     binding)
			     (output-args args)
			     (format #t "~a~%"
				     (if doc doc
					 "(documentation unavailable)"))
			     (format #t "@end deffn~%")))))
		   (module-public-interface module)))

(define (extract-reader-lib-doc)
  (extract-module-doc (resolve-module '(system reader library))))


(define-public (extract-doc . args)
  (with-output-to-file "token-reader-doc.texi"
    extract-token-reader-doc)

  (with-output-to-file "reader-lib-doc.texi"
    extract-reader-lib-doc))

(define main extract-doc)

;;; arch-tag: e6ff91db-72f3-4b1c-8cec-ccc6305b2fd1

;;; extract-doc.scm ends here
