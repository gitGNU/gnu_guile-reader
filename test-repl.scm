;;; test-repl.scm -- A simple REPL that uses a user-defined reader.
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

;;; arch-tag: test-repl.scm


(use-modules (system reader)
	     (srfi srfi-1))

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
                     (lambda (chr port read top-level-read)
                       (format #t "got chr `~a', read=~a, top-level-read=~a~%"
			       chr read top-level-read)
                       (list quote 'magic!))))

(define sharp-reader
  ;; Create a reader for anything that will follow the `#' character.  This
  ;; reader is not be to be used alone but rather as the procedure of the
  ;; token reader for `#' (see below).
  (make-reader (append (list whitespace-token-reader
                             test-token-reader)
                       (map standard-token-reader
                            '(character
			      srfi-4 vector guile-bit-vector
			      number+radix
                              guile-extended-symbol
                              boolean keyword
                              scsh-block-comment)))
               (lambda (chr port read)
                 (error "unexpected character after `#'" chr))
	       'reader/record-positions))

(define colon-keyword-token-reader
  ;; Reading `:kw'-style keywords.
  (make-token-reader #\:
		     (token-reader-procedure
		      (standard-token-reader 'keyword))))

(define brace-free-symbol-token-reader
  ;; Since we want to support the `curly-brace-sexp' token reader, we must
  ;; make can't use the `symbol-misc-chars' token reader because it would
  ;; interpret `}' as a symbol.  So we have to create our own brace-free
  ;; symbol token reader.
  (let ((char-set (token-reader-specification
		   (standard-token-reader 'brace-free-symbol-misc-chars))))
    (make-token-reader (filter (lambda (chr)
				 (not
				  (or (char=? chr #\})
				      (char=? chr #\{)
				      (char=? chr #\])
				      (char=? chr #\[))))
			       char-set)
		       (token-reader-procedure
			(standard-token-reader
			 'brace-free-symbol-misc-chars)))))

(define the-reader
  ;; The top-level reader.  It reuses the previously defined reader for `#'
  ;; as well as a number of standard built-in token readers.
  (make-reader (append (list (make-token-reader #\# sharp-reader)
                             whitespace-token-reader
			     colon-keyword-token-reader
			     brace-free-symbol-token-reader)
                       (map standard-token-reader
                            `(sexp string brace-free-number
                              brace-free-symbol-lower-case
                              brace-free-symbol-upper-case
                              quote-quasiquote-unquote
                              semicolon-comment
                              skribe-exp
			      curly-brace-sexp)))
	       #f ;; use the default fault handler
	       'reader/record-positions
	       ;;'reader/debug
	       ;;'reader/upper-case
	       ))


;; And now the REPL itself.
(let loop ((reader the-reader))
  (display "guile-reader> ")
  (let ((sexp (reader (current-input-port))))
    (if (eof-object? sexp)
	(quit))
    (write (eval sexp (interaction-environment))))
  (display "\n")
  (loop reader))

;;; test-repl.scm ends here
