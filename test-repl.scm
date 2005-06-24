(use-modules (reader))

;;(kill (getpid) SIGTSTP)
;;(do-stuff (current-input-port))

(let loop ((reader (make-reader #f)))
  (display "dynamic-reader> ")
  (write (eval (reader (current-input-port)) (interaction-environment)))
  (display "\n")
  (loop reader))

