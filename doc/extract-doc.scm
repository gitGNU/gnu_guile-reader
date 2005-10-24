#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(extract-doc)) '\'main')'
exec ${GUILE-../guile} -L module -l $0 -c "(apply $main (cdr (command-line)))" "$@"
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
  #:use-module (system reader))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Extract documentation of the standard token readers and print them in a
;;; Texinfo-friendly form so that they can be included in the manual.
;;;
;;; Code:

(define (extract-doc . args)
  (for-each (lambda (tr-name)
	      (let* ((tr (standard-token-reader tr-name))
		     (doc (token-reader-documentation tr)))
		(format #t "@item ~a~%~a~%"
			tr-name doc)))
	    (sort (standard-token-reader-names)
		  (lambda (s1 s2)
		    (string<=? (symbol->string s1)
			       (symbol->string s2))))))

(define main extract-doc)

;;; arch-tag: e6ff91db-72f3-4b1c-8cec-ccc6305b2fd1

;;; extract-doc.scm ends here
