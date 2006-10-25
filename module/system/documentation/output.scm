;;; output.scm  --  Output documentation "snarffed" from C files in Texi/GDF.
;;;
;;; Copyright 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (system documentation output)
  :use-module (srfi srfi-13)
  :autoload   (system documentation c-snarf) (run-cpp-and-extract-snarfing)

  :export (schemify-name
           procedure-gdf-string procedure-texi-documentation
           output-procedure-texi-documentation-from-c-file))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides support function to issue Texinfo or GDF (Guile
;;; Documentation Format) documentation from "snarffed" C files.
;;;
;;; Code:


;;;
;;; Utility.
;;;

(define (schemify-name str)
  "Turn @var{str}, a C variable or function name, into a more ``Schemey''
form, e.g., one with dashed instead of underscores, etc."
  (string-map (lambda (chr)
                (if (eq? chr #\_)
                    #\-
                    chr))
              (if (string-suffix? "_p" str)
                  (string-append (substring str 0
                                            (- (string-length str) 2))
                                 "?")
                  str)))


;;;
;;; Issuing Texinfo and GDF-formatted doc (i.e., `guile-procedures.texi').
;;; GDF = Guile Documentation Format
;;;

(define (procedure-gdf-string proc-doc)
  "Issue a Texinfo/GDF docstring corresponding to @var{proc-doc}, a
documentation alist as returned by @code{parse-snarfed-line}.  To produce
actual GDF-formatted doc, the resulting string must be processed by
@code{makeinfo}."
  (let* ((proc-name (assq-ref proc-doc 'scheme-name))
         (args (assq-ref proc-doc 'arguments))
         (location (assq-ref proc-doc 'location))
         (file-name (car location))
         (line (cadr location))
         (documentation (assq-ref proc-doc 'documentation)))
    (string-append "" ;; form feed
                   proc-name (string #\newline)
                   (format #f "@c snarfed from ~a:~a~%"
                           file-name line)
                   "@deffn {Scheme Procedure} " proc-name " "
                   (string-join (map schemify-name args) " ")
                   (string #\newline)
                   documentation (string #\newline)
                   "@end deffn" (string #\newline))))

(define (procedure-texi-documentation proc-doc)
  "Issue a Texinfo docstring corresponding to @var{proc-doc}, a documentation
alist as returned by @var{parse-snarfed-line}.  The resulting Texinfo string
is meant for use in a manual since it also documents the corresponding C
function."
  (let* ((proc-name (assq-ref proc-doc 'scheme-name))
         (c-name (assq-ref proc-doc 'c-name))
         (args (assq-ref proc-doc 'arguments))
         (location (assq-ref proc-doc 'location))
         (file-name (car location))
         (line (cadr location))
         (documentation (assq-ref proc-doc 'documentation)))
  (string-append (string #\newline)
		 (format #f "@c snarfed from ~a:~a~%"
			 file-name line)
		 "@deffn {Scheme Procedure} " proc-name " "
		 (string-join (map schemify-name args) " ")
                 (string #\newline)
                 "@deffnx {C Function} " c-name " ("
                 (if (null? args)
                     "void"
                     (string-join (map (lambda (arg)
                                         (string-append "SCM " arg))
                                       args)
                                  ", "))
                 ")" (string #\newline)
		 documentation (string #\newline)
                 "@end deffn" (string #\newline))))


;;;
;;; Very high-level interface.
;;;

(define (output-procedure-texi-documentation-from-c-file c-file cpp cflags
                                                         port)
  (for-each (lambda (texi-string)
              (display texi-string port))
            (map procedure-texi-documentation
                 (run-cpp-and-extract-snarfing cpp c-file cflags))))


;;; output.scm ends here

;;; arch-tag: 20ca493a-6f1a-4d7f-9d24-ccce0d32df49
