(define-module (reader)
   #:export (make-reader make-token-reader
	     standard-token-reader default-reader
	     do-stuff))

(dynamic-call "scm_reader_init_bindings"
              (dynamic-link "libguile-reader.so"))
