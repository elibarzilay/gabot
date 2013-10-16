#lang racket/base

(require "globals.rkt" "logger.rkt" racket/string)

;; Almost raw output, at the lower level it is still subject to trimming
;; long strings, will not show newlines, and will wait after each output
;; to avoid flooding.
(provide out)
(define (out fmt . args)
  (channel-put *output-channel*
               (if (or (string? fmt) (pair? args))
                 (apply format fmt args)
                 fmt)))

;; (say [where] str) will show a message on the `where' place (a place
;; is a symbol); if there's only a string, use `*place*'.
(provide say)
(define say
  (case-lambda
    [(str) (say *place* str)]
    [(where str)
     (unless where (error 'say "don't have a target to talk to"))
     (out "PRIVMSG ~a :~a"
          (cond [(not where) (error 'say "don't have a target to talk to")]
                [(not (eq? where *nick*)) where]
                [(not (eq? *who* *nick*)) *who*]
                [else (error 'say "trying to talk to myself")])
          str)]))

;; (tell [who] fmt arg ...) uses `say' to tell someone something at
;; `*place*'; `who' should be a symbol, if it is not given, use `*who*'.
(provide tell)
(define (tell x . xs)
  (define-values [who str]
    (if (or (symbol? x) (not x))
      (values x (apply format xs))
      (values *who* (apply format x xs))))
  (say (cond [(eq? *place* who) str]
             [(not who) str]
             [else (format "~a: ~a" who str)])))

;; (me [place] fmt arg ...) similar to `say', but uses an ACTION line
;; (as in "/me").
(provide me)
(define (me x . xs)
  (define-values [where str]
    (if (or (symbol? x) (not x))
      (values x (apply format xs))
      (values *place* (apply format x xs))))
  (out "PRIVMSG ~a :\1ACTION ~a\1" where str))

;; For use in match patterns: a predicate that matches the bot's nick.
(provide me?)
(define (me? x) (eq? (if (string? x) (string->symbol x) x) *nick*))

;; Predicate for channels.
(provide irc-channel?)
(define (irc-channel? sym)
  (and (symbol? sym) (regexp-match? #rx"^#(.*)$" (symbol->string sym))))

;; Turns a string with words to a list of (list word from to) where
;; `word' is a symbol, and `from'/`to' are the string positions; useful
;; for parsing text lines (with the indexes for grab substring ranges
;; from the original text).
(provide text->sent)
(define (text->sent text)
  (define lower (string-downcase text))
  (define ms    (regexp-match-positions* #px"[[:word:]]+" text))
  (for/list ([m (in-list ms)])
    (list (string->symbol (substring text (car m) (cdr m))) (car m) (cdr m))))

;; Reports someone saying something, possibly logging it too; useful for
;; both logging and stdout reporting.
(provide report)
(define (report #:prefix [prefix #f] #:sep [sep ":"] #:log? [log? #t])
  (define place
    (cond [(irc-channel? *place*) *place*]
          [(eq? *place* *nick*) "(private)"]
          [(not *place*)        "(nowhere)"]
          [(memq *place* '(* $* $$*)) "(global)"]
          [(list? *place*) (string-join (map symbol->string *place*) ",")]
          [else (error 'report "got a weird place: ~e" *place*)]))
  (define (msg place)
    `(,@(if prefix (list prefix " ") '())
      ,(or *who* "-")
      ,(or place "")
      ,@(if *text* (list sep " " *text*) '())
      "\n"))
  (when log?
    (thread-send
     logger-thread
     (cons (msg #f) (if (list? *place*) *place* (list *place*)))))
  (for-each display (msg place)))

(define-syntax R-internal-choose
  (syntax-rules ()
    [(R-internal-choose r [weight expr])
     expr]
    [(R-internal-choose r [weight0 expr0] [weight expr] ...)
     (if (<= (+ weight ...) r)
       expr0
       (R-internal-choose r [weight expr] ...))]))
(define-syntax R-internal
  (syntax-rules ()
    [(R-internal [weight expr] ...)
     (let ([r (random (+ weight ...))])
       (R-internal-choose r [weight expr] ...))]))
(define-syntax R*-convert
  (syntax-rules ()
    [(R*-convert (acc ...) ())
     (R-internal acc ...)]
    [(R*-convert (acc ...) (n expr more ...))
     (R*-convert (acc ... [n expr]) (more ...))]))

;; (R E1 E2 ... En) chooses a random subform to evaluate,
;; (R* W1 E1 W2 E2 ... Wn En) weighted version of `R'.
(provide R R*)
(define-syntax-rule (R expr0 expr ...) (R-internal [1 expr0] [1 expr] ...))
(define-syntax-rule (R* n0 expr0 more ...) (R*-convert () (n0 expr0 more ...)))

;; Run body in a background thread.
(provide BG)
(define-syntax-rule (BG expr ...) (thread (λ() expr ...)))
