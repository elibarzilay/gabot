#lang s-exp "../handler.rkt"

;; Handler for CTCPs (constructed in "utterances.rkt")

(defhandler CTCP/VERSION
  (out "NOTICE ~a :\1VERSION ~a:1:Racket ~a\1" *who* *name* (version)))

(defhandler CTCP/ACTION
  (report #:sep "")
  (*re-handle* 'ACTION 'all)) ; run all 'ACTION handlers
