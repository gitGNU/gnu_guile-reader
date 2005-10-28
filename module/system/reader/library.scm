;;; library.scm  --  A framework for building Scheme-like readers.
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

(define-module (system reader library)
  #:use-module (system reader))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides a library of readers for various syntax flavours.
;;;
;;; Code:

(define-public make-guile-reader
  ;; This function is actually written in C and a bound in `(system reader)'.
  (module-ref (resolve-module '(system reader))
	      'make-guile-reader))

;;; arch-tag: 8ac38d67-472d-4371-ad92-8a1306218505

;;; library.scm ends here
