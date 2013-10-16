#lang racket/base

;; Some utilities for the rest of the code
;; - (get-conf key [default]) get a value from the configuration file
;; - definition forms for globals that can be `set!'-ed: `defconst'
;;   (just an alias for `define', no `set!'), `defglobal' for defining a
;;   parameter, and `defshared' for a global mutable box
;; - (on-exit thunk) and (exit) to register exit handlers
;; - (rx str ...), to be used as @rx{...} -- convenient regexp building
;; - `warn' -- similar to `error' but just show the text
;; - `ls' -- convenient function for `directory-list' + filter results
;; - `rx:' -- convenient regexp matcher for `match'

(require racket/match racket/string racket/file racket/runtime-path
         (for-syntax racket/base))
(provide (all-defined-out))

(define-runtime-path sample-config-file "sample-config.rkt")

(define get-conf
  (let ()
    (define config-file
      (build-path (find-system-path 'pref-dir) "gabot-config.rkt"))
    (define (sample)
      (define lines
        `("The config file is a plain Racket module, containing definitions"
          "for various options to configure your bot.  A sample file follows,"
          "commented definitions are optional, showing the default values:"
          "    ----------------------------------------"
          ,@(for/list ([l (file->lines sample-config-file)])
              (string-append "    " l))
          "    ----------------------------------------"))
      (string-join lines "\n  " #:before-first "  "))
    (unless (file-exists? config-file)
      (raise-user-error 'get-conf "missing config file at \"~a\"\n~a"
                        config-file (sample)))
    (dynamic-require config-file #f)
    (define ns (module->namespace config-file))
    (λ(option [default ns])
      (define def (if (eq? default ns)
                    (λ() (error 'get-conf "missing option: ~s" option))
                    (λ() default)))
      (namespace-variable-value option #t def ns))))

;; a utility for implementing some of the following definers; can be
;; used like this:
;;   (defalias a x)
;;   (defalias a (+ x 1)) ; set! won't work
;;   (defalias a (+ x 1) setter-fun)
(define-syntax (defalias stx)
  (syntax-case stx ()
    [(defalias name expr)
     (identifier? #'name)
     (if (identifier? #'expr)
       #'(define-syntax name (make-rename-transformer #'expr))
       #'(define-syntax name
           (syntax-id-rules (set! name)
             [(set! name x) (set! expr x)]
             [(name . xs) (expr . xs)]
             [name expr])))]
    [(defalias name expr setter-fun)
     (identifier? #'name)
     #'(begin (define setter setter-fun)
              (define-syntax name
                (syntax-id-rules (set!)
                  [(set! name x) (setter x)]
                  [(name . xs) (expr . xs)]
                  [name expr])))]))

;; defines a global `constant' (just a plain definition)
(define-syntax defconst
  (syntax-rules ()
    [(defconst *glob* val)
     (begin (provide *glob*)
            (define *glob* val))]))

;; defines a per-thread global (a parameter)
(define-syntax (defglobal stx)
  (syntax-case stx ()
    [(defglobal *glob* val)
     (identifier? #'*glob*)
     (let* ([p (syntax-e #'*glob*)]
            [p (symbol->string p)]
            [p (regexp-replace #rx"^([*]?)(.*?)([*]?)$" p "\\1\\2-param\\3")]
            [p (string->symbol p)]
            [p (datum->syntax #'*glob* p #'*glob*)])
       (with-syntax ([*glob-param* p])
         #'(begin
             (provide *glob* *glob-param*)
             (define *glob-param* (make-parameter val))
             (defalias *glob* (*glob-param*) (λ(_) (*glob-param* _))))))]))

;; defines a true mutable global (a box)
(define-syntax defshared
  (syntax-rules ()
    [(defshared *glob* val)
     (begin (provide *glob*)
            (define b (box val))
            (defalias *glob* (unbox b) (λ(x) (set-box! b x))))]))

(define exit-thunks '())
(define (on-exit thunk) (set! exit-thunks (cons thunk exit-thunks)))
(define exit
  ;; make it possible to exit from any thread without killing itself
  (let* ([ch (make-channel)]
         [exit-thread
          (thread (λ() (define n (channel-get ch))
                       (for ([t (in-list (reverse exit-thunks))]) (t))
                       ((exit-handler) n)))])
    (define (exit [n 0]) (channel-put ch n))
    exit))

(define (rx . strs)
  (byte-regexp (apply bytes-append
                      (map string->bytes/utf-8 `("^" ,@strs "$")))))

(define (warn who fmt . args)
  (fprintf (current-output-port) "~a: ~a\n" who (apply format fmt args)))

(define (sym . xs)
  (string->symbol
   (string-append*
    (for/list ([x (in-list xs)])
      (cond [(string? x) x]
            [(symbol? x) (symbol->string x)]
            [(number? x) (number->string x)]
            [else (error 'sym "bad value: ~e" x)])))))

(define (ls pred)
  (sort (filter pred (map path->string (directory-list))) string<?))

(define-match-expander rx:
  (λ(stx)
    (syntax-case stx ()
      [(rx regexp p ...)
       #'(app (λ(x) (cond [(regexp-match regexp x) => cdr] [else #f]))
              (list p ...))]))
  (λ(stx)
    (raise-syntax-error #f "used out of match context" stx)))
