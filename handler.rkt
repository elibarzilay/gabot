#lang racket

;; A language for writing handlers.  The main thing is `defhandler',
;; with a simple use of (defhandler FOO body ...) that defines a handler
;; for and IRC command `FOO', and (defhandler (FOO P ...) body ...) does
;; the same with P being matched against `*params*'.  The idea is that
;; these handle IRC messages, but handlers can invoke other handlers via
;; `*re-handle*'.  There can be more than one handler for the same
;; command -- see "bot.rkt" for further explanation.  The language is
;; needed to collect these handlers so they can be load/unloaded when
;; handler files change.  There are also `:onload' and `:onunload' forms
;; for registering setup/teardown for a handler file.

(require "utils.rkt" "irc-utils.rkt" "globals.rkt" (for-syntax racket/base))

(provide (except-out (all-from-out racket) #%module-begin)
         (all-from-out "utils.rkt" "irc-utils.rkt" "globals.rkt")
         (for-syntax (all-from-out racket/base)))

(define-for-syntax (handler-body stx args exprs)
  (if args
    (quasisyntax/loc stx (λ() (match *params* [(list #,@args) #,@exprs])))
    (quasisyntax/loc stx (λ() #,@exprs))))

(provide defhandler)
(define-syntax (defhandler stx)
  (syntax-case stx (defhandler)
    [(defhandler key/args body0 body ...)
     (let-values ([(key args)
                   (syntax-case #'key/args ()
                     [(key arg ...) (values #'key (syntax->list #'(arg ...)))]
                     [key (not (syntax->list #'key)) (values #'key #f)])])
       (with-syntax ([handlers (datum->syntax key 'handlers key)]
                     [handler (handler-body
                               stx args (syntax->list #'(body0 body ...)))]
                     [key key])
         #'(set! handlers (cons (list 'key handler) handlers))))]))

(provide :onload :onunload)
(define-for-syntax (keyword-handler stx)
  (syntax-case stx ()
    [(:name body0 body ...)
     (with-syntax ([handlers (datum->syntax #':name 'handlers #':name)]
                   [fun (handler-body stx #f
                                      (syntax->list #'(body0 body ...)))])
       #'(set! handlers (cons (list ':name fun) handlers)))]))
(define-syntax :onload   keyword-handler)
(define-syntax :onunload keyword-handler)

(provide (rename-out [handler-module-begin #%module-begin]))
(define-syntax (handler-module-begin stx)
  (define e (or (syntax->list stx)
                (raise-syntax-error #f "bad syntax" stx)))
  (datum->syntax
   (quote-syntax here)
   `(#%module-begin
     ,(datum->syntax stx `(define handlers '()))
     ,@(cdr e)
     ,(datum->syntax stx `(set! handlers (reverse handlers)))
     (provide handlers))
   stx))
