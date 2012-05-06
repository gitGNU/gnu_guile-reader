#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
LTDL_LIBRARY_PATH="`pwd`/.libs"
GUILE_AUTO_COMPILE=0
export LTDL_LIBRARY_PATH GUILE_AUTO_COMPILE
main='(module-ref (resolve-module '\''(unicode)) '\'unicode')'
exec ${GUILE-"${top_builddir-..}/pre-inst-guile"} -l $0  \
        -c "(apply $main (cdr (command-line)))" "$@"
!#
;;;
;;; Copyright 2012  Ludovic Courtès <ludo@gnu.org>
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
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

(define-module (unicode)
  #:use-module (system reader)
  #:use-module (system reader library)
  #:use-module (srfi srfi-1)
  #:export (unicode))

(set-port-encoding! (current-output-port) "UTF-8")

(define make-colon-free-token-reader
  (@@ (system reader library) make-colon-free-token-reader))

(define %skribe-read
  ;; Skribe-compatible reader taken from (skribilo reader skribe).
  (let* ((dsssl-keyword-reader  ;; keywords à la `#!key'
          (make-token-reader #\!
                             (token-reader-procedure
                              (standard-token-reader 'keyword))))
         (sharp-reader
          (make-reader (cons dsssl-keyword-reader
                             (map standard-token-reader
                                  '(character srfi-4 vector number+radix
                                    boolean srfi30-block-comment
                                    srfi62-sexp-comment)))
                       #f ;; use default fault handler
                       'reader/record-positions))
         (symbol-misc-chars-tr
          ;; Make sure `:' is handled only by the keyword token reader.
          (make-colon-free-token-reader
           (standard-token-reader 'r6rs-symbol-misc-chars))))
    (make-reader (cons* (make-token-reader #\# sharp-reader)
                        symbol-misc-chars-tr
                        (map standard-token-reader
                             `(whitespace
                               sexp string r6rs-number
                               r6rs-symbol-lower-case
                               r6rs-symbol-upper-case
                               quote-quasiquote-unquote
                               semicolon-comment
                               skribe-exp)))
                 #f ;; use the default fault handler
                 'reader/record-positions)))


(define (unicode . _)
  (with-fluids ((%default-port-encoding "UTF-8"))
    (exit
     (and (eq? 'λ (pk 'lambda (call-with-input-string "λ" (default-reader))))
          (equal? '`("this is Χαοσ " ,(+ 1 2) " !")
                  (pk 'Χαοσ (call-with-input-string "[this is Χαοσ ,(+ 1 2) !]"
                                                    %skribe-read)))))))

;; Local Variables:
;; coding: utf-8
;; End:
