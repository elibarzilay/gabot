#lang s-exp "../handler.rkt"

(defhandler VERB/help
  (R (tell "Ra helps those who help themselves")
     (me "pongs")
     (me "ponders")))

(defhandler VERB/source
  (R (tell "Do *you* know where your sources are?")
     (tell "I will not expose my sources")
     (me "screws his forehead out and grabs a mirror to look inside")
     (tell "Yes!  Use it.")))

(defhandler VERB/version
  (R (tell "What's *your* version?")
     (tell "Feh.  You just assume that all bots have versions?  I'm offended.")
     (tell "2.0.  I'm cool, right?")))

(defhandler VERB/seen
  (match *sent*
    [(list)
     (R (tell "Seen what?")
        (tell "Seen who?")
        (me "didn't look")
        (me (format "looks ~a" (R "confused" "puzzled")))
        (me "stares blankly"))]
    [(list (list (? me?) _ _))
     (R* 1 (tell "Yes.  I am very self-aware.")
         3 (me (format "looks ~a ~a mirror" (R "for" "at") (R "his" "a"))))]
    [else (R (tell "I haven't looked")
             (tell "I'm not really keeping track")
             (tell "I'm not telling")
             (tell "They come, they go, do I care?"))]))

(defhandler VERB/slap
  (let* ([who (and (pair? *sent*) (caar *sent*))]
         [who (if (me? who) *who* who)])
    (match (map car *sent*)
      ['(all bots)
       (let* ([nicks (hash-ref *channels* *place* #f)]
              [nicks (and nicks (hash-ref nicks 'nicks #f))]
              [nicks (and nicks
                          (filter (λ(n) (and (regexp-match? #rx"bot$"
                                                            (symbol->string n))
                                             (not (eq? n *nick*))))
                                  nicks))])
         (cond
           [(not nicks)          (me "wonders why he can't tell who is here")]
           [(null? nicks)        (me "doesn't see any bots")]
           [(null? (cdr nicks))  (me "slaps ~a" (car nicks))]
           [(null? (cddr nicks)) (apply me "slaps ~a and ~a" nicks)]
           [nicks (define-values [xs ys] (split-at-right nicks 2))
                  (me "slaps ~a~a and ~a"
                      (string-append* (map (λ(x) (format "~a, " x)) xs))
                      (car ys)
                      (cadr ys))]))]
      [(list 'offby1) (me "slaps offby1 upside the haid")]
      [(list _who) (me "slaps ~a" who)]
      [(list _who how) (me "slaps ~a ~a" who how)]
      [(list _who xs ...)
       (me "~a" (apply string-append "slaps"
                       (map (λ(x) (format " ~a" x)) (cons who xs))))]
      [else (tell "Huh?")])))

(defhandler VERB/glance
  (match *sent*
    [(list (list xs _ _) ...)
     (me (apply string-append "glances" (map (λ(x) (format " ~a" x)) xs)))]))

(define (snack)
  (match *sent*
    [(list (list xs _ _) ...)
     (R (me "~a~a"
            (R "munches" "chomps" "swallows" "eats")
            (R "" " noisily" " hungrily" " sheepishly" " loudly"))
        (me "bites~a" (R "" " noisily" " hungrily" " sheepishly" " loudly")))
     (when (zero? (random 2))
       (BG (sleep 1)
           (me "~a~a" (R "burps" "hiccups") (R "" " loudly" " quietly"))))]))
(defhandler VERB/botsnack (snack))

(defhandler VERB/snack (snack))
