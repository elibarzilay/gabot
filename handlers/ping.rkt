#lang s-exp "../handler.rkt"

(define (generic-ping pong)
  (R* 20 (tell "~a~a" pong (R* 4 "" 3 "." 3 "!" 2 "..." 2 "?" 1 "!?"))
      10 (tell "~a!  (Is this what you wanted~a?)"
               (R* 3 pong
                   1 (string-upcase (format "~a" pong))
                   1 (string-titlecase (format "~a" pong)))
               (R " me to say" " to hear" ""))
      20 (me (format "~as~a" pong
                     (R* 5 ""
                         4 (format " at ~a" *who*)
                         1 " cheerfully"
                         1 " happily"
                         1 " loudly"
                         1 " quietly"
                         1 " intently"
                         1 " twice"
                         1 " three times")))
      4  (me (format " is too ~a to ~a" (R "tired" "bored" "lazy") pong))
      1  (tell "Why don't you just ~a yourself too?" pong)
      2  (tell "Leave me alone, I'm busy now")
      2  (tell "beeps")
      2  (tell "pings")))

(defhandler VERB/ping (generic-ping 'pong))
(defhandler VERB/pong (generic-ping 'ping))
(defhandler VERB/ding (generic-ping 'dong))
(defhandler VERB/dong (generic-ping 'ding))
(defhandler VERB/bing (generic-ping 'bong))
(defhandler VERB/bong (generic-ping 'bing))
(defhandler VERB/beep (generic-ping 'peep))
(defhandler VERB/peep (generic-ping 'beep))
(defhandler VERB/fing (generic-ping 'fong))
