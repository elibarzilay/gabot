#lang racket/base

;; Deal with the log-in sequence

(require "globals.rkt" "utils.rkt" "irc-utils.rkt" racket/match)

(define ch (make-channel))

(define (wait-for . xs)
  (for ([x xs])
    (unless (eq? x (channel-get ch))
      (error 'initialization "did not get expected token: ~e" x))))

(define (init-handler)
  (match *line*
    ;; ignore AUTH lines
    [(list 'NOTICE #f (or "AUTH" (? me?)) (rx: #rx"^\\*\\*\\*"))
     #t]
    ;; ignore notification about +i mode (invisible)
    [(list 'MODE _ (? me?) "+i")
     #t]
    ;; password invitation
    [(list 'NOTICE 'NickServ (? me?) (rx: #rx"^This nickname is registered"))
     (channel-put ch 'invite)
     #t]
    ;; password accepted reactions
    [(or (list 'NOTICE 'NickServ (? me?) (rx: #rx"^You are now identified"))
         ;; (list '901 _ (? me?) _ ...) ; RPL_LOGGEDIN
         ;; (list 'MODE _ (? me?) "+e")
         )
     (channel-put ch 'accepted)
     #t]
    [else #f]))

(provide initialize)
(define (initialize)
  (set! *pre-handlers* (list init-handler))
  ;; be polite
  (sleep 0.2)
  ;; subscribe
  (printf "Subscribing...\n")
  (out "NICK ~a" *nick*)
  (out "USER ~a unknown-host ~a :~a"
       (or (getenv "USER") "unknown") *server* *name*)
  ;; wait for two password invitation lines, and do so
  (printf "Authenticating...\n")
  (wait-for 'invite)
  (say 'NickServ (string-append "IDENTIFY " *password*))
  ;; wait for confirmation
  (wait-for 'accepted #;'accepted)
  ;; successfully done
  (printf "Subscribed as `~a'\n" *nick*)
  (set! *pre-handlers* '())
  (set! *last-ping* (current-seconds))
  (for ([c *init-channels*]) (out "JOIN ~a" c)))
