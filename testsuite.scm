#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(testsuite)) '\'main')'
exec ${GUILE-guile} -l $0 -c "(apply $main (cdr (command-line)))" "$@"
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.


(define-module (testsuite)
  #:use-module (reader)
  #:use-module (srfi srfi-1))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Runs a test-suite to verify the behaviour of the various token readers
;;; provided.
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

(define-public (correctly-loads-boot-file? reader)
  (let* ((boot-file (%search-load-path "ice-9/boot-9.scm"))
         (correct-result (load-file-with-reader boot-file read))
         (result (load-file-with-reader boot-file reader)))
    (sexp-equal? correct-result result)))

(define-public (type-name obj)
  (cond ((symbol? obj)  "symbol")
        ((string? obj)  "string")
        ((number? obj)  "number")
        ((keyword? obj) "keyword")
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
  ;; A series of simple test cases for the standard Scheme reader.
  '((string . "\"hello\"")
    (string-with-backslash . "\"hello \\\"world\\\"\"")
    (number . "777")
    (floating-number . "3.14")
    (positive-number . "+12")
    (negative-number . "-12")
    (complex-number . "1+2i")
    (complex-number-floating . "1.0-3.14I")
    (complex-like-symbol . "1.0-2.0")
    (symbol-lower-case . "symbol")
    (symbol-upper-case . "SYMBOL")
    (symbol-percent . "%symbol")
    (symbol-star . "*symbol*")
    (symbol-at . "@symbol@")
    (symbol-dot . ".")
    (symbol-lower-than . "<")
    (symbol-greater-than-or-equal . ">=")
    (symbol-dollar . "$home")
    (symbol-with-numbers . "123.123.123")
    (symbol-one-minus . "1-")
    (symbol-one-plus . "1+")
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

    ;; this is not the default in Guile but here it is, hence the `read-set!'
    ;; that must be called before running the test-suite
    (colon-keyword . ":keyword")

    ;; the sharp reader
    (false . "#f")
    (true . "#t")
    (false-uppercase . "#F")
    (true-uppercase . "#T")
    (character . "#\\x")
    (character-newline . "#\\newline")
    (character-dot . "#\\.")
    (keyword . "#:kw")
    (guile-extended-symbol . "#{extended symbol}#")
    (srfi-4-vector . "#u8(1 2 3 4 5)")
    (srfi-4-floating-point-vector . "#f32(3.14 1.7 14.11 11.02)")
    (hex-number . "#x10")
    (bin-number . "#b10")
    (oct-number . "#o10")

    ;; more complex expressions
    (pair-of-chars . "(#\\a . #\\b)")
    (pair-of-complexes . "(1.2+3i .\t1.4+4.4i)")
    (nested-sexps . "(if (= a b)\nb\n\t(+\na b))")
    (line-comment-within-sexp . "(+ 2 ;; a line comment\n\t2)\n")
    (line-comment-finishing-sexp . "(f 2 ;; comment before bracket\n)\n")

    ;; FIXME:  This last one currently fails.  In order not to fail, the API
    ;; needs to be improved.
    (scsh-block-comment-within-sexp . "(f #! a comment\n!#\n 2)\n\t\n")
    (scsh-block-comment-finishing-sexp . "(+ 2 #! a comment\n!#\n) ")))



(define-public (testsuite . args)
  (read-set! keywords 'prefix)
  (let ((total 0) (failed 0))
    (catch #t
      (lambda ()
        (let* ((results (map (lambda (test-case)
                               (format #t "~a... " (car test-case))
                               (let* ((name (car test-case))
                                      (str (cdr test-case))
                                      (passed?
                                       (read-well? str (default-reader))))
                                 (format #t "~a~%"
                                         (if passed? "ok" "FAILED"))
                                 passed?))
                             test-cases)))
          (set! total (length results))
          (set! failed (length (filter not results)))
          (if (= 0 failed)
              (format #t "All ~a tests passed~%" total)
              (format #t "~a tests failed out of ~a~%" failed total))))

      (lambda (key . args)
        ;; Since Guile exists with status zero when an exception is thrown and
        ;; not caught, we have to do this by ourself.
        (format #t "~%~%exception caught:~%")
        (let ((subr (car args))
              (fmt (cadr args))
              (fmt-arg (caddr args)))
          (format #t (string-append fmt "~%") fmt-arg)
          (exit 1))))

    (format #t "reading `boot-9.scm'... ")
    (if (correctly-loads-boot-file? (default-reader))
        (format #t "ok~%")
        (begin
          (set! failed (+ 1 failed))
          (format #t "failed~%")))

    failed))

(define main testsuite)

;;; arch-tag: testsuite.scm

;;; testsuite.scm ends here
