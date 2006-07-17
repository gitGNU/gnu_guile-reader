;;; library.scm  --  A framework for building Scheme-like readers.
;;;
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (system reader library)
  :use-module (system reader)
  :use-module (system reader compat)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-11) ;; `let-values'
  :use-module (srfi srfi-17) ;; generalized `set!'
  :use-module (ice-9 optargs))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides a library of readers for various syntax flavours.
;;;
;;; Code:

(define-public make-guile-reader
  ;; This function is actually written in C and bound in `(system reader)'.
  (module-ref (resolve-module '(system reader))
	      'make-guile-reader))



;;;
;;; Turning ``extended'' reader options into a list of token readers.
;;;

(define (filter-out-symbol-token-readers top-specs)
  "Filter out symbol-related token readers from @var{top-specs}, a list of
token readers used by a top-level reader."
  (let* ((upper '(#\A . #\Z))
	 (lower '(#\a . #\z))
	 (misc (token-reader-specification
		(standard-token-reader 'guile-symbol-misc-chars)))
	 (number '(#\0 . #\9)))
    (filter (let ((symbol-specs (list upper lower misc number)))
	      (lambda (tr)
		(let ((spec (token-reader-specification tr)))
		  (not (member spec symbol-specs)))))
	    top-specs)))

(define (make-colon-free-token-reader tr)
  "If token reader @var{tr} handles the @code{:} (colon) character, remove it
from its specification and return the new token reader."
  (let* ((spec (token-reader-specification tr))
	 (proc (token-reader-procedure tr)))
    (make-token-reader (filter (lambda (chr)
				 (not (char=? chr #\:)))
			       spec)
		       proc)))

(define (ensure-colon-free-token-readers specs)
  "For token readers listed in @var{specs} that handle the @code{:} (colon)
character, remove @code{:} from their specification (using
@code{make-colon-free-token-reader})."
  (map (lambda (tr)
	 (if (token-reader-handles-char? tr #\:)
	     (make-colon-free-token-reader tr)
	     tr))
       specs))

(define-public (alternate-guile-reader-token-readers options)
  "Given @var{options}, a list of symbols describing reader options relative
to the reader returned by @code{(default-reader)}, return two lists of token
readers: one for use as a sharp reader and the other for use as a top-level
reader.  Currently, the options supported are the following:

@table @code
@item no-sharp-keywords
Remove support for @code{#:kw}-style keywords.
@item dsssl-keywords
Add support for DSSSL-style keywords, like @code{#!kw}.  This option also has
the same effect as @code{no-scsh-block-comments}.
@item colon-keywords
Add support for @code{:kw}-style keywords.  This is equivalent to
@code{(read-set! keywords 'prefix)}.
@item no-scsh-block-comments
Disable SCSH-style block comments (see @inforef{Block Comments,
SCSH block comments, guile}, for details).
@item srfi30-block-comments
Add support for SRFI-30 block comments, like:
@smallexample
(+ 2 #| This is an #| SRFI-30 |# comment |# 2)
@end smallexample
@item srfi62-sexp-comments
Add support for SRFI-62 S-expression comments, like:
@smallexample
(+ 2 #;(a comment) 2)
@end smallexample
@item case-insensitive
Read symbols in a case-insensitive way.
@item square-bracket-sexps
Allow for square brackets around S-expressions.
@end table\n"
  (let loop ((options options)
	     (processed '())
	     (sharp-specs (default-sharp-reader-token-readers))
	     (top-specs (default-reader-token-readers)))
    (if (null? options)
	(values sharp-specs top-specs)
	(loop (cdr options)
	      (cons (car options) processed)

	      (case (car options)
		;; remove support for `#:kw'-style keywords
		((no-sharp-keywords)
		 (filter (lambda (tr)
			   (not (eq? (token-reader-specification tr)
				     #\:)))
			 sharp-specs))

		;; add support for DSSSL keywords, i.e. `#!kw'
		((dsssl-keywords)
		 (let* ((kw-tr (standard-token-reader 'keyword))
			(kw-proc (token-reader-procedure kw-tr)))
		   (cons (make-token-reader #\! kw-proc)
			 (filter (lambda (tr)
				   (not (eq? (token-reader-specification tr)
					     #\!)))
				 sharp-specs))))

		;; remove support for SCSH-style block comments
		((no-scsh-block-comments)
		 (filter (lambda (tr)
			   (not (eq? (token-reader-specification tr)
				     #\!)))
			 sharp-specs))

		((srfi30-block-comments)
		 (cons (standard-token-reader 'srfi30-block-comment)
		       sharp-specs))

		((srfi62-sexp-comments)
		 (cons (standard-token-reader 'srfi62-sexp-comment)
		       sharp-specs))

		(else sharp-specs))

	      (case (car options)
		;; add support for `:kw-'-style keywords
		((colon-keywords)
		 (let* ((kw-tr (standard-token-reader 'keyword))
			(kw-proc (token-reader-procedure kw-tr)))
		   (cons (make-token-reader #\: kw-proc)
                         (ensure-colon-free-token-readers top-specs))))

		;; case-insensitive symbols
		((case-insensitive)
		 (append (map standard-token-reader
			      '(r5rs-lower-case-symbol-upper-case
				r5rs-lower-case-symbol-lower-case
				r5rs-lower-case-number))
			 (list (let ((tr (standard-token-reader
					  'r5rs-lower-case-symbol-misc-chars)))
				 (if (or (memq 'colon-keywords processed)
					 (memq 'colon-keywords options))
				     (make-colon-free-token-reader tr)
				     tr)))
			 (filter-out-symbol-token-readers top-specs)))

		;; square brackets in s-expressions
		((square-bracket-sexps)
		 (append (map standard-token-reader
			      '(r6rs-symbol-upper-case
				r6rs-symbol-lower-case
				r6rs-number
				square-bracket-sexp))
			 (list (let ((tr (standard-token-reader
					  'r6rs-symbol-misc-chars)))
				 (if (or (member 'colon-keywords processed)
					 (member 'colon-keywords options))
				     (make-colon-free-token-reader tr)
				     tr)))
			 (filter-out-symbol-token-readers top-specs)))

		(else top-specs))))))

(define*-public (make-alternate-guile-reader options
					     #:optional (fault-handler #f)
					     #:rest flags)
  "Return a newly created Guile reader with options @var{options} (a list of
symbols, as for @code{alternate-guile-reader-token-readers}), with fault
handler @var{fault-handler} and flags @var{flags}.  The @var{fault-handler}
and @var{flags} arguments are the same as those passed to
@code{make-reader}."
  (let-values (((sharp-specs top-specs)
		(alternate-guile-reader-token-readers options)))
    (let* ((sharp (apply make-reader `(,sharp-specs ,fault-handler ,@flags)))
	   (sharp-tr (make-token-reader #\# sharp)))
      (apply make-reader
	     `(,(cons sharp-tr
		      (filter (lambda (tr)
				(not (eq? (token-reader-specification tr)
					  #\#)))
			      top-specs))
	       ,fault-handler
	       ,@flags)))))



;;;
;;; Turning Guile's read options into ``extended'' reader options
;;; understandable by `alternate-guile-reader-token-readers'.
;;;

(define-macro (g-w/-s var)
  `(getter-with-setter (lambda () ,var)
		       (lambda (val) (set! ,var val))))

(define-public (read-options->extended-reader-options read-opts)
  "Read @var{read-opts}, a list representing read options following Guile's
built-in representation (see @inforef{Scheme Read, , guile}, for details),
and return a list of symbols represented ``extended reader options''
understood by @code{make-alternate-guile-reader} et al."
  (let loop ((read-opts read-opts)
	     (extended-opts '())
	     (make-reader-opts '()))
    (if (null? read-opts)
	(values extended-opts make-reader-opts)
	(let ((opt-spec (lookup-read-option (car read-opts))))
	  (if (not opt-spec)
	      (error "unknown read option" (car read-opts))
	      (let ((takes-arg? (read-option-takes-argument? opt-spec))
		    (convert! (read-option-convert-proc opt-spec)))
		(apply convert!
		       (append (list (g-w/-s extended-opts)
				     (g-w/-s make-reader-opts))
			       (if takes-arg?
				   (list (cadr read-opts))
				   '())))
		(loop (if takes-arg? (cddr read-opts) (cdr read-opts))
		      extended-opts
		      make-reader-opts)))))))


;;; arch-tag: 8ac38d67-472d-4371-ad92-8a1306218505

;;; library.scm ends here
