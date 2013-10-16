#lang s-exp "../handler.rkt"

(define telling? #f)

(define (say-lines . lines)
  (when (pair? lines)
    (let loop ([lines lines])
      (say (car lines))
      (when (pair? (cdr lines))
        (rsleep 1.5)
        (loop (cdr lines))))))

(define (rsleep n) (sleep (* (random) n)))

(defhandler VERB/joke
  (if telling?
    (tell (R "Shhh!" "Don't interrupt me!" "Quiet, please!" "Do you mind??"))
    (BG (set! telling? #t)
        (R (begin (say "A rabbi and a priest are on a train,")
                  (rsleep 3)
                  (say "The priest is eating a ham sandwich,")
                  (rsleep 4)
                  (say "Umm...")
                  (rsleep 4)
                  (say "I don't remember the end, sorry."))
           (begin (me "mumbles something")
                  (rsleep 3)
                  (say (R "Mumble, mumble mumble -- mumble!"
                          (string-append
                           "Outside the window is a third sign saying, \"3rd "
                           "worst Chinese torture test: Left testicle tied "
                           "to bedpost.\"")
                          "\"Where's my Rolex!!!!!\""
                          "Look, a talking cat!"
                          "That's it! I'll do the dishes!!"))
                  (rsleep 1)
                  (me "laughs out loud"))
           (begin (me "thinks")
                  (rsleep 3)
                  (say (format "~a!" (add1 (random 99))))
                  (rsleep 1)
                  (me "laughs hysterically"))
           (begin (say-lines (R "10 PRINT \"HELLO WORLD\""
                                "10 POKE 54296,0")
                             (R "20 END" "20 GOTO 10"))
                  (rsleep 1)
                  (me "pauses")
                  (rsleep 2)
                  (me "sighs")))
        (set! telling? #f))))
