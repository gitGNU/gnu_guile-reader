#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
LTDL_LIBRARY_PATH="`pwd`/.libs"
export LTDL_LIBRARY_PATH
main='(module-ref (resolve-module '\''(benchmark)) '\'main')'
exec ${GUILE-./guile-for-test} -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#
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

(define-module (benchmark)
  :use-module (system reader)
  :use-module (ice-9 format)
  :export (benchmark))

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; A tiny benchmark that compares the time taken to read (just read, not
;;; load) standard Guile Scheme files with Guile's built-in reader and with
;;; the reader from `guile-reader' that's returned by `default-reader'.
;;;
;;; Code:


;; Sample files
(define %files-to-load
  (map %search-load-path
       '("ice-9/boot-9.scm"  "ice-9/common-list.scm"
	 "ice-9/format.scm"  "ice-9/optargs.scm"
	 "ice-9/session.scm" "ice-9/getopt-long.scm")))

;; Number of iterations reading files.  Adjust this as a function of your
;; machine's CPU power.
(define %iterations 30)



(define (load-file-with-reader file-name reader)
  (with-input-from-file file-name
    (lambda ()
      (setvbuf (current-input-port) _IOFBF 4096)
      (let loop ((sexp (reader)))
        (if (eof-object? sexp)
            #t
	    (loop (reader)))))))

(define (how-long reader)
  (let loop ((start (get-internal-run-time))
	     (iterations-left %iterations))
    (if (= 0 iterations-left)
	(- (get-internal-run-time) start)
	(begin
	  (for-each (lambda (file)
		      (load-file-with-reader file reader))
		    %files-to-load)
	  (loop start (- iterations-left 1))))))


(define (benchmark . args)
  (let ((built-in (how-long read))
	(guile-reader (how-long (default-reader))))
    (format #t "Guile's built-in reader:        ~a~%" built-in)
    (format #t "Guile-Reader's default reader:  ~a~%" guile-reader)
    (format #t "improvement:                    ~2,2f times faster~%"
	    (/ built-in guile-reader 1.0))
    #t))

(define main benchmark)


;;; arch-tag: d0777df5-80c6-4ac4-b861-a2adfb87744c

;;; benchmark.scm ends here
