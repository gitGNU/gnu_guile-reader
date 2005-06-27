(use-modules (reader))

;;(kill (getpid) SIGTSTP)
;;(do-stuff (current-input-port))

(let loop ((reader (make-reader " \n\t"
				'(sexp string number
				  symbol-lower-case symbol-upper-case
				  symbol-misc-chars
				  quote-quasiquote-unquote
				  sharp
				  semicolon-comment
				  skribe-literal))))
  (display "dynamic-reader> ")
  (write (eval (reader (current-input-port)) (interaction-environment)))
  (display "\n")
  (loop reader))

