;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; compat.scm  --  Compatibility with Guile's `read-options' interface.
;;;
;;; Copyright 2005, 2010  Ludovic Courtès <ludo@gnu.org>
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


(define-module (system reader compat)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-17)
  #:export (read-option-spec?
	    read-option-name read-option-takes-argument?
	    read-option-convert-proc
	    %read-options-spec lookup-read-option
	    clean-up-read-options))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides helper functions to implement an interface
;;; compatible with Guile's `read-option' interface.
;;;
;;; Code:


;;;
;;; Read options.
;;;


;; Description of the various options of `read-options', i.e. whether they
;; need an additional argument or not.

(define-record-type <read-option-spec>
  ;; NAME names a standard Guile reader extension.
  ;; TAKES-ARG? is a boolean telling whether this extension takes an arg.
  ;; CONVERT-PROC is a procedure that takes two procedure with setters, one
  ;;              to modify the reader options (as used in `(system reader
  ;;              library)') and another to modify the `make-reader' options.
  (make-read-option-spec name takes-arg? convert-proc)
  read-option-spec?
  (name          read-option-name)
  (takes-arg?    read-option-takes-argument?)
  (convert-proc  read-option-convert-proc))

(define-macro (define-read-options name lst)
  `(define ,name
     (map (lambda (stuff)
	    (apply make-read-option-spec stuff))
	  ,lst)))

(define-read-options %read-options-spec
  ;; Guile's standard reader options.
  `((keywords #t ,(lambda (reader-options make-options value)
		    (case value
		      ((prefix)
		       (set! (reader-options)
			     (cons 'colon-keywords
				   (reader-options))))
		      ((#f)
		       (set! (reader-options)
			     (delq 'colon-keywords (reader-options))))
		      (else
		       (error "unsupported value for option `keywords'"
			      value)))))

    (case-insensitive #f ,(lambda (reader-options make-options)
			    (set! (reader-options)
				  (cons 'case-insensitive
					(reader-options)))))

    (positions #f ,(lambda (read-options make-options)
		     (set! (make-options)
			   (cons 'reader/record-positions
				 (make-options)))))

    (copy #f ,(lambda (read-options make-options)
		(error "`(read-enable 'copy)' not implemented")))))


(define (lookup-read-option option-name)
  "Look up read option named @var{option-name} (a symbol) among the list of
standard Guile reader options (see @inforef{Reader options, Guile's reader
options, guile}, for a list of options) and return a
@code{<read-option-spec>} object, or @code{#f} if not found."
  (find (lambda (opt-spec)
	  (eq? (read-option-name opt-spec) option-name))
	%read-options-spec))

(define (clean-up-read-options options)
  "Process the new read options in @var{options}, a flat list, and return a
``cleaned'' flat list without duplicate options, etc."
  (let loop ((result '())
	     (options options))
    (if (null? options)
	result
	(let* ((opt (car options))
	       (opt-spec (lookup-read-option opt)))
	  (if (not opt-spec)
	      (error "unknown read option" opt)
	      (let ((takes-arg? (read-option-takes-argument? opt-spec)))
		(if takes-arg?
		    (let ((value (memq opt result))
			  (new-value (cadr options)))
		      (if value
			  (set-cdr! value
				    (cons new-value (cddr value)))
			  (set! result
				(append (list opt new-value) result))))
		    (if (not (memq opt result))
			(set! result (append! result (list opt)))))
		(loop result
		      (if takes-arg? (cddr options) (cdr options)))))))))



;;;
;;; Dynamically configurable `primitive-load'.
;;;


;; In Guile 1.7.2++, `current-reader' is a core binding bound to a fluid
;; whose value should be either `#f' or a `read'-like procedure.  The value
;; of this fluid dictates the reader that is to be used by `primitive-load'.
;;
;; See:
;; http://lists.gnu.org/archive/html/guile-devel/2005-11/msg00006.html
;; http://lists.gnu.org/archive/html/guile-devel/2005-12/msg00062.html .

(if (not (defined? 'current-reader))
    (begin ;; forward-compatible implementation

      (module-define! the-root-module 'current-reader (make-fluid))
      (fluid-set! current-reader #f)

      (set! primitive-load
	    (lambda (file)
	      (with-input-from-file file
		(lambda ()
		  (let loop ((sexp ((or (fluid-ref current-reader) read))))
		    (if (not (eof-object? sexp))
			(begin
			  (primitive-eval sexp)
			  (loop ((or (fluid-ref current-reader)
				     read))))))))))))


;;; arch-tag: c9971617-3a90-4dbb-be3f-aa4b42d4f462

;;; compat.scm ends here
