(define-module (reader)
   #:export (make-reader do-stuff))

(dynamic-call "dynr_init_bindings"
              (dynamic-link "libguile-dynamic-reader"))
