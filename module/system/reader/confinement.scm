;;; confinement.scm  --  Confinement of reader modifications.
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

(define-module (system reader confinement)
  #:use-module (system reader))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides a simple implementation of per-module reader
;;; settings (i.e. via `read-enable') and per-module readers.  In other
;;; words, it redefines `read-enable' to a side-effect-free version of it.
;;;
;;; Code:

;; Keep a copy of the original version of `read-options'.
(define %built-in-read-options-interface read-options-interface)

;; The current value of `read-options' within this module.
(define %module-read-options (make-object-property))

;; Information useful to `make-reader'.
(define %module-reader-spec (make-object-property))
(define %module-reader-make-options (make-object-property))
(define %module-reader-fault-handler (make-object-property))
(define %module-reader (make-object-property))

(define-macro (define-ensure what default-value)
  `(define (,(symbol-append 'ensure- what) module)
     (if (not (,(symbol-append '%module- what) module))
	 (set! (,(symbol-append '%module- what) module)
	       ,default-value))
     (,(symbol-append '%module- what) module)))

(define-ensure read-options (%built-in-read-options-interface))
(define-ensure reader-spec (default-reader-token-readers))
(define-ensure reader-make-options '())
(define-ensure reader-fault-handler #f)
(define-ensure reader (default-reader))


;; Description of the various options of `read-options', i.e. whether they
;; need an additional argument or not.

(define %read-options-spec
  '((keywords . #t)
    (case-insensitive . #f)
    (positions . #f)
    (copy . #f)))

(define (read-option-takes-argument? option)
  (assoc-ref %read-options-spec option))

(define (process-new-read-options options)
  "Process the new read options in @var{options}, a flat list, and return a
``cleaned'' flat list without duplicate options, etc."
  (let loop ((result '())
	     (options options))
    (if (null? options)
	result
	(let* ((opt (car options))
	       (takes-arg? (read-option-takes-argument? opt)))
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
		(if takes-arg? (cddr options) (cdr options)))))))


;; A version of `read-options-interface' (and consequently `read-options',
;; `read-set!', `read-enable' and `read-disable') that is confined to a
;; module.
(set! read-options-interface
      (lambda args
	(format #t "confined `read-options-interface': ~a~%" args)
	(let* ((module (current-module))
	       (opts (ensure-read-options module)))
	  (cond ((null? args)
		 opts)
		((list? (car args))
		 (set! (%module-read-options module)
		       (process-new-read-options (car args))))
		(else
		 ;; FIXME: This could be implemented too.
		 (apply %built-in-read-options-interface args))))))

(define (enable-reader-options option value)
  (let ((module (current-module)))
    (format #t "read-enable: module is ~a~%" module)
    (case option
      ((positions)
       (set! (%module-reader-make-options module)
	     (cons 'reader/record-positions
		   (ensure-reader-options module)))
       (set! (%module-reader module)
	     (apply make-reader
		    (append (list (ensure-reader-spec module))
			    (cons (%module-reader-fault-handler module)
				  (%module-reader-make-options module)))))
       (module-define! module 'read
		       (%module-reader module)))
      (else
       (error "unhandled reader option" option)))))


;;; arch-tag: 9eda977f-4edb-48c5-bdb7-28a6dd0850c6

;;; confinement.scm ends here
