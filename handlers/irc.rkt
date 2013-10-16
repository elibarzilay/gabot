#lang s-exp "../handler.rkt"

;; Basic IRC handlers

(define (track-pings) (set! *last-ping* (current-seconds)))
(defhandler (PING server)      (track-pings) (out "PONG ~a" server))
(defhandler (PONG server text) (track-pings))

(defhandler (ERROR message)
  (warn 'ERROR "~a" message))

(define (track-users where verb [reason #f])
  (define ch  (string->symbol where))
  (define t   (hash-ref! *channels* ch (λ() (make-hasheq))))
  (define me? (eq? *who* *nick*))
  (unless (irc-channel? ch)
    (error 'track-users "got a weird place for ~e: ~e" verb where))
  (set! *place* ch)
  (set! *text* reason)
  (if me?
    ;; if it's `join' it was already added in `hash-ref!' above
    (when (eq? 'part verb) (hash-remove! *channels* ch))
    (let ([nicks (hash-ref t 'nicks '())])
      (hash-set! t 'nicks
        (if (eq? 'join verb) (cons *who* nicks) (remq* `(,*who*) nicks)))))
  (report #:prefix (format "(~a)" verb) #:log? (not me?)))

(defhandler (JOIN where) (track-users where 'join))
(defhandler PART
  (match *params*
    [(list where reason) (track-users where 'part reason)]
    [(list where)        (track-users where 'part #f)]))

(defhandler (QUIT reason)
  (set! *place* '())
  (set! *text* reason)
  (hash-for-each *channels*
    (λ(ch t) (define nicks (hash-ref t 'nicks '()))
             (when (memq *who* nicks)
               (set! *place* (cons ch *place*))
               (hash-set! t 'nicks (remq* `(,*who*) nicks)))))
  (report #:prefix "(quit)"))

(defhandler (TOPIC where topic)
  (define ch (string->symbol where))
  (define t  (hash-ref! *channels* ch make-hasheq))
  (unless (irc-channel? ch) (error 'topic "got a weird place: ~e" ch))
  (set! *place* ch)
  (set! *text*  topic)
  (hash-set! t 'topic topic)
  (report #:prefix "(topic)"))

(defhandler (NICK new)
  new (string->symbol new)
  (set! *place* '())
  (set! *text* (format "-> ~a" new))
  (hash-for-each *channels*
    (λ(ch t) (define nicks (hash-ref t 'nicks '()))
             (when (memq *who* nicks)
               (set! *place* (cons ch *place*))
               (hash-set! t 'nicks (cons new (remq* `(,*who*) nicks))))))
  (report #:prefix "(nick)" #:sep ""))

;; initial join messages

;; RPL_TOPIC
(defhandler (332 (? me?) where topic)
  (define ch (string->symbol where))
  (define t  (hash-ref! *channels* ch make-hasheq))
  (unless (irc-channel? ch) (error '-topic "got a weird place: ~e" ch))
  (set! *who*   #f)
  (set! *place* ch)
  (set! *text*  topic)
  (hash-set! t 'topic topic)
  (report #:prefix "(topic)"))

;; RPL_NAMREPLY
(defhandler (353 (? me?) mode where names)
  ;; channel mode: "@" = secret, "*" = private, "="  = public
  (define ch    (string->symbol where))
  (define t     (hash-ref! *channels* ch make-hasheq))
  (define nicks (hash-ref t 'nicks '()))
  (unless (irc-channel? ch) (error '-names "got a weird place: ~e" ch))
  (set! *who*   #f)
  (set! *place* ch)
  (set! *text*  names)
  (hash-set! t 'nicks
    (remove-duplicates
     (append (map (λ(s) (string->symbol (regexp-replace #rx"^@" s "")))
                  (regexp-split #rx" +" names))
             nicks)))
  (report #:prefix "(names)"))
