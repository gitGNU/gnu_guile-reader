#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
LTDL_LIBRARY_PATH="`pwd`/.libs"
export LTDL_LIBRARY_PATH
main='(module-ref (resolve-module '\''(scheme-reader)) '\'main')'
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


(define-module (scheme-reader)
  #:use-module (system reader)
  #:use-module (system reader library)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 format))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Runs a test-suite to verify the behaviour of the various token readers
;;; provided and use by the default Scheme reader returned by
;;; `default-reader'.
;;;
;;; Code:


(define-public (read-this str reader)
  (with-input-from-string str
    (lambda ()
      (reader))))

(define-public (read-well? str reader)
  (equal? (read-this str read)
          (read-this str reader)))

(define-public (load-file-with-reader file-name reader)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((sexp (reader))
                 (result '()))
        (if (eof-object? sexp)
            result
	    (loop (reader) (cons sexp result)))))))

(define-public (correctly-loads-file? reader file)
  (format #t "reading `~a'... " file)
  (let* ((file (%search-load-path file))
         (correct-result (load-file-with-reader file read))
         (result (load-file-with-reader file reader)))
    (if (sexp-equal? correct-result result)
	(begin
	  (format #t "ok~%")
	  #t)
	(begin
	  (format #t "FAILED~%")
	  #f))))

(define-public (correctly-loads-standard-files? reader)
  (let ((files (list "ice-9/boot-9.scm"  "ice-9/common-list.scm"
		     "ice-9/format.scm"  "ice-9/optargs.scm"
		     "ice-9/session.scm" "ice-9/getopt-long.scm")))
    (values (length files)
	    (length (filter not
			    (map (lambda (file)
				   (correctly-loads-file? reader file))
				 files))))))

(define-public (type-name obj)
  (cond ((symbol? obj)  "symbol")
        ((string? obj)  "string")
        ((number? obj)  "number")
        ((keyword? obj) "keyword")
        ((char? obj)    "character")
        (else           "expression")))

(define-public (sexp-equal? ref sexp)
  "Compare reference S-exp @var{ref} against @var{sexp} and return @code{#t}
is those are @code{equal?}.  Additionally, display the differences
encountered."
  (define (true? x) (if x #t #f))

  (let loop ((ref ref) (sexp sexp))
    (cond ((list? ref)
           (every true? (map loop ref
                             (if (list? sexp) sexp
                                 (make-list (length ref) *unspecified*)))))
          ((pair? ref)
           (and (loop (car ref) (car sexp))
                (loop (cdr ref) (cdr sexp))))
          (else
           (if (equal? ref sexp) #t
               (begin
                 (format #t "  ~as `~a' and `~a' differ~%"
                         (type-name ref)
                         ref sexp)
                 #f))))))


(define-public test-cases
  ;; A series of simple test cases and corner cases for the standard Scheme
  ;; reader.
  '((string . "\"hello\"")
    (string-with-backslash . "\"hello \\\"world\\\"\"")
    (number . "777")
    (floating-number . "3.14")
    (positive-number . "+12")
    (negative-number . "-12")
    (number-exponential . "1e9")
    (number-exponential-negative . "1e-09")
    (negative-number-exponential-negative . "-77e-77")
    (complex-number . "1+2i")
    (complex-number-floating . "1.0-3.14I")
    (complex-pure-imaginary . "+i")
    (complex-like-symbol . "1.0-2.0")
    (complex-at . "2@2")
    (complex-+i . "+i")
    (complex--i . "-i")
    (symbol-lower-case . "symbol")
    (symbol-upper-case . "SYMBOL")
    (symbol-ampersand . "&symbol")
    (symbol-percent . "%symbol")
    (symbol-star . "*symbol*")
    (symbol-at . "@symbol@")
    (symbol-brace . "{symbol}")
    (symbol-square-brackets . "[symbol]")
    (symbol-dot . ".")
    (symbol-lower-than . "<")
    (symbol-greater-than-or-equal . ">=")
    (symbol-dollar . "$home")
    (symbol-with-numbers . "123.123.123")
    (symbol-one-minus . "1-")
    (symbol-one-plus . "1+")
    (symbol-plus-plus-i . "1++i")
    (symbol-plus-i-plus-i . "1+i+i")
    (symbol-like-complex . "1+e10000i")
    (symbol-like-exponential . "1e")
    (quoted-symbol . "'quoted-symbol")
    (quoted-number . "'7")
    (sexp-numbers . "(1 2 3)")
    (sexp-symbols . "( a b c )")
    (pair . "(a . b)")
    (pair-long . "(a b . c)")
    (pair-again . "(a .b)")
    (fake-pair . "(. a)")
    (quasiquote . "`a")
    (quasiquote-unquote . "`(a ,b)")
    (quasiquote-unquote-splicing . "`(a ,@b)")
    (line-comment . ";;; line comment\n777")

    ;; the sharp reader
    (false . "#f")
    (true . "#t")
    (false-uppercase . "#F")
    (true-uppercase . "#T")
    (character . "#\\x")
    (character-newline . "#\\newline")
    (character-dot . "#\\.")
    (character-comma . "#\\,")
    (character-quote . "#\\'")
    (character-space . "#\\ ")
    (character-bracket . "#\\)")
    (keyword . "#:kw")
    (guile-extended-symbol . "#{extended symbol}#")
    (vector . "#(1 symbol \"string\")")
    (srfi-4-vector . "#u8(1 2 3 4 5)")
    (srfi-4-floating-point-vector . "#f32(3.14 1.7 14.11 11.02)")
    (hex-number . "#x10")
    (bin-number . "#b10")
    (oct-number . "#o10")
    (dec-number . "#d10")
    (bit-vector . "#*010101010101")

    ;; more complex expressions
    (pair-of-chars . "(#\\a . #\\b)")
    (pair-of-complexes . "(1.2+3i .\t1.4+4.4i)")
    (nested-sexps . "(if (= a b)\nb\n\t(+\na b))")
    (line-comment-within-sexp . "(+ 2 ;; a line comment\n\t2)\n")
    (line-comment-finishing-sexp . "(f 2 ;; comment before bracket\n)\n")

    ;; comments within S-exps
    (scsh-block-comment-within-sexp . "(f #! a comment\n!#\n 2)\n\t\n")
    (scsh-block-comment-finishing-sexp . "(+ 2 #! a comment\n!#\n) ")))

(define-public (reader-conforms-with-guile? reader)
  "Tests whether @var{reader} conforms with Guile's built-in reader."
  (let* ((results (map (lambda (test-case)
			 (format #t "~a... " (car test-case))
			 (let* ((name (car test-case))
				(str (cdr test-case))
				(passed?
				 (read-well? str (default-reader))))
			   (format #t "~a~%"
				   (if passed? "ok" "FAILED"))
			   passed?))
		       test-cases))
	 (total (length results))
	 (failed (length (filter not results))))

    (values total failed)))


(define-public (scheme-reader . args)
  (read-enable 'positions)
  (let ((total 0) (failed 0))
    (catch #t
      (lambda ()
	(for-each (lambda (reader)
		    (map (lambda (test)
			   (let-values (((total* failed*)
					 (test reader)))
			     (set! total (+ total total*))
			     (set! failed (+ failed failed*))))
			 (list reader-conforms-with-guile?
			       correctly-loads-standard-files?)))

		  ;; Try out two versions of the Guile reader: one that
		  ;; doesn't record positions and one that does.
		  (list (default-reader)
			(make-guile-reader #f 'reader/record-positions)))
	(format #t "~%~%* summary~%~%total: ~a~%failed: ~a~%"
		total failed))

      (lambda (key . args)
        ;; Since Guile exists with status zero when an exception is thrown and
        ;; not caught, we have to do this by ourself.
        (format #t "~%~%exception caught:~%")
        (let ((subr (car args))
              (fmt (cadr args))
              (fmt-arg (caddr args)))
          (format #t (string-append fmt "~%") fmt-arg)
          (exit 1))))

    failed))

(define main scheme-reader)

;;; arch-tag: testsuite.scm

;;; scheme-reader.scm ends here
