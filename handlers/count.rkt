#lang s-exp "../handler.rkt"

(define counter 0)

(defhandler VERB/count
  (set! counter (add1 counter))
  (tell "~a" counter))

(defhandler VERB/reset
  (set! counter 0)
  (tell "done!"))
