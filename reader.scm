(define-module (reader)
   #:export (make-reader make-token-reader

	     ;; helpers
	     standard-token-reader default-reader

	     ;; accessors
	     token-reader-procedure token-reader-specification))


(dynamic-call "scm_reader_init_bindings"
              (dynamic-link "libguile-reader.so"))
