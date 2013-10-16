#lang racket/base

;; Parse IRC messages and put the results in the right places for later
;; processing

(require "utils.rkt" "globals.rkt")

(define irc-line-rx
  (rx " *"
      "(?::([^ ]*) +)?" ; optional prefix
      "([^ ]*)"         ; command
      "( +.*)?"         ; parameters, including initial space
      ))

(define irc-prefix-rx
  (rx "(.*?)"       ; server/nick
      "(?:!(.*?))?" ; optional user
      "(?:@(.*?))?" ; optional host
      ))

(define (not-empty bs)
  (and bs (< 0 (bytes-length bs)) (bytes->string/utf-8 bs #\?)))

(define (parse-prefix x)
  (cond [(regexp-match irc-prefix-rx x)
         => (λ(m) (define xs (map not-empty (cdr m)))
                  (cons (string->symbol (car xs)) (cdr xs)))]
        [else (warn 'parse-prefix "bad prefix: ~e" x) (list x #f #f)]))

(define (parse-command x)
  (define s (bytes->string/utf-8 x #\?))
  ((if (regexp-match? #rx"^[0-9]+$" s) string->number string->symbol) s))

(define (parse-params x)
  (let loop ([x x] [r '()])
    (cond [(regexp-match #rx#"^ +: *(.*?) *$" x)
           => (λ(m) (reverse (cons (bytes->string/utf-8 (cadr m) #\?) r)))]
          [(regexp-match #rx#"^ +([^ ]+)(.*)$" x)
           => (λ(m) (loop (caddr m)
                          (cons (bytes->string/utf-8 (cadr m) #\?) r)))]
          [else
           (unless (regexp-match? #rx#"^ *$" x)
             (warn 'parse-params "bad params: ~e" x))
           (reverse r)])))

(provide irc-parse)
;; sets the message globals, if it doesn't, then a warning will happen later
(define (irc-parse line)
  (cond
    [(regexp-match? #rx#"[\0\r\n]" line)
     (warn 'irc-parse "bad character in irc line")]
    [(regexp-match irc-line-rx line)
     => (λ(m)
          (define (get n parse)
            (define x (list-ref m n))
            (and x (not (equal? "" x)) (parse x)))
          (define cmd (get 2 parse-command))
          (define-values [who user host]
            (apply values (or (get 1 parse-prefix) '(#f #f #f))))
          (define params (or (get 3 parse-params) '()))
          (set! *cmd*  cmd)
          (set! *who*  who)
          (set! *user* user)
          (set! *host* host)
          (set! *params* params)
          (set! *line* (list* cmd who params)))]))
