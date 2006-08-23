#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
LTDL_LIBRARY_PATH="`pwd`/.libs"
export LTDL_LIBRARY_PATH
main='(module-ref (resolve-module '\''(test-confinement)) '\'main')'
exec ${GUILE-"${top_builddir-..}/pre-inst-guile"} -l $0  \
        -c "(apply $main (cdr (command-line)))" "$@"
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

(define-module (test-confinement)
  #:use-module (system reader confinement))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Test whether `(system reader confinement)' provides actual confinement of
;;; the reader settings and correctly implements `read-options-interface'.
;;;
;;; Code:

(define %test-file ",,confined-module.scm")


(define-public (test-confinement . args)
  (if (file-exists? %test-file)
      (delete-file %test-file))

  (with-output-to-file %test-file
    (lambda ()
      (display "(define-module (confined-module))
                (read-set! keywords 'prefix)
                (read-enable 'case-insensitive)
                (define lower-case-keyword :Colon-KeyWord)")))

  (primitive-load %test-file)

  (let ((the-keyword (module-ref (resolve-module '(confined-module))
				 'lower-case-keyword)))
    (if (not (and (keyword? the-keyword)
		  (eq? the-keyword #:colon-keyword)))
	(begin
	  (format #t "unexpected keyword: ~a~%" the-keyword)
	  (exit 1)))

    (if (or (not (symbol? ':symbol))
	    (eq? 'HELLO 'hello))
	(begin
	  (format #t "confinement broken~%")
	  (exit 2)))

    (exit 0)))

(define main test-confinement)

;;; arch-tag: b0101c62-2f43-4649-97f3-79f2691229b2

;;; test-confinement.scm ends here
