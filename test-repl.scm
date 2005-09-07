;;; test-repl.scm -- A simple REPL that uses a dynamically defined reader.
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

;;; arch-tag: test-repl.scm


(use-modules (reader))

;;(kill (getpid) SIGTSTP)
;;(do-stuff (current-input-port))

(format #t "Hello, this is your friendly REPL using a dynamically defined\n\
reader for Guile!~%~%")

(define *ws*
  ;; Consider characters 1 to 32 (included) as whitespaces.
  (map (lambda (x)
         (integer->char (+ 1 x)))
       (iota 32)))

(define whitespace-token-reader
  ;; Create a ``fake'' token reader (hence `#f' as the PROC argument to
  ;; MAKE-TOKEN-READER) for this set of characters: it will simply silently
  ;; ignore them.
  (make-token-reader *ws* #f))

(define test-token-reader
  ;; A simple token reader, implemented in Scheme, such that `#?' will return
  ;; a quoted symbol.
  (make-token-reader #\?
                     (lambda (chr port read)
                       (format #t "got chr `~a'~%"
                               chr)
                       (list quote 'magic!))))

(define sharp-reader
  ;; Create a reader for anything that will follow the `#' character.  This
  ;; reader is not be to be used alone but rather as the procedure of the
  ;; token reader for `#' (see below).
  (make-reader (append (list whitespace-token-reader
                             test-token-reader)
                       (map standard-token-reader
                            '(character srfi-4 number+radix
                              extended-symbol
                              boolean keyword
                              block-comment)))
               (lambda (chr port read)
                 (error "unexpected character after `#'" chr))))

(define the-reader
  ;; The top-level reader.  It reuses the previously defined reader for `#'
  ;; as well as a number of standard built-in token readers.
  (make-reader (append (list (make-token-reader #\# sharp-reader)
                             whitespace-token-reader)
                       (map standard-token-reader
                            `(sexp string number
                              symbol-lower-case
                              symbol-upper-case
                              symbol-misc-chars
                              colon-keyword
                              quote-quasiquote-unquote
                              semicolon-comment
                              skribe-exp)))))


;; And now the REPL itself.
(let loop ((reader the-reader))
  (display "guile-reader> ")
  (let ((sexp (reader (current-input-port))))
    (if (eof-object? sexp)
	(quit))
    (write (eval sexp (interaction-environment))))
  (display "\n")
  (loop reader))

;; test-repl.scm ends here
