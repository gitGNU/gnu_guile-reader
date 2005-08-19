(use-modules (reader))

;;(kill (getpid) SIGTSTP)
;;(do-stuff (current-input-port))

(define sharp-reader (make-reader " \n\t"
				  '(character srfi-4
				    boolean keyword block-comment)))

(format #t "sharp-reader: ~a~%" sharp-reader)

(let loop ((reader (make-reader " \n\t"
				`(sexp string number
				  symbol-lower-case symbol-upper-case
				  symbol-misc-chars
				  quote-quasiquote-unquote
				  (#\# . ,sharp-reader)
				  semicolon-comment
				  skribe-exp))))
  (display "dynamic-reader> ")
  (write (eval (reader (current-input-port)) (interaction-environment)))
  (display "\n")
  (loop reader sharp-reader))

