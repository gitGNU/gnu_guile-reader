(define-module (reader)
   #:export (make-reader default-reader
	     do-stuff))

(dynamic-call "dynr_init_bindings"
              (dynamic-link "libguile-dynamic-reader.so"))
