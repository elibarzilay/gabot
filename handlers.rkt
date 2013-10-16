#lang racket/base

;; Deal with the handlers directory: load handlers, reload them on
;; changes, unload broken ones, etc.  These files usually define
;; handlers (see "handler.rkt") but can also be used for any dynamic
;; code that interacts with IRC.

(require "globals.rkt" "utils.rkt" syntax/moddep)

(define verbose-reloading? (make-parameter #t))

;; taken from collects/handin-server/private/reloadable.ss
(define (module-get modspec valname)
  (define path (resolve-module-path modspec #f))
  (define name ((current-module-name-resolver) modspec #f #f))
  (when (verbose-reloading?) (printf "(re)loading module from ~a\n" modspec))
  (with-handlers ([void (λ(e) (warn 'module-get "couldn't get ~e from ~e: ~a"
                                    valname modspec
                                    (if (exn? e) (exn-message e) e))
                              #f)])
    (parameterize ([current-module-declare-name name]
                   [compile-enforce-module-constants #f])
      (namespace-require '(only racket module #%top-interaction))
      (load/use-compiled path))
    (dynamic-require modspec valname)))

(define handlers-table (make-hasheq))

(provide get-handlers)
(define (get-handlers key)
  (hash-ref handlers-table key '()))

(define files+times #f)
(define file->keys+handlers-table (make-hash))

(define (add-file-handlers file handlers-table)
  (define keys+handlers (module-get file 'handlers))
  (when keys+handlers
    (hash-set! file->keys+handlers-table file keys+handlers)
    (for ([k+h (in-list (reverse keys+handlers))])
      (define key (car k+h))
      (define handler (cadr k+h))
      (case key
        [(:onload) (handler)]
        [(:onunload) (void)]
        [else (hash-set! handlers-table key
                         (cons handler
                               (hash-ref handlers-table key '())))]))))

(define (remove-file-handlers file handlers-table)
  (for ([k+h (in-list (hash-ref file->keys+handlers-table file '()))])
    (define key (car k+h))
    (define handler (cadr k+h))
    (case key
      [(:onload) (void)]
      [(:onunload) (handler)]
      [else (let* ([handlers (hash-ref handlers-table key #f)]
                   [handlers (and handlers (remq handler handlers))])
              (if handlers
                (hash-set! handlers-table key handlers)
                (hash-remove! handlers-table key)))])))

(define (scan-handlers [unload? #f])
  (define init? (not files+times))
  (define old (or files+times '()))
  (define new (if unload?
                '()
                (map (λ(file)
                       (cons file (file-or-directory-modify-seconds file)))
                     (ls file-exists?))))
  (unless (equal? old new) ; optimize common case
    (define new-table (hash-copy handlers-table))
    ;; run op on items in files1 that are different or missing from files2
    (define (scan files1 files2 op)
      (for ([file+time files1])
        (define file  (car file+time))
        (define time1 (cdr file+time))
        (define time2 (cond [(assoc file files2) => cdr] [else #f]))
        (unless (equal? time1 time2) (op file new-table))))
    (parameterize ([verbose-reloading? (not init?)])
      ;; remove deleted, modified
      (scan old new remove-file-handlers)
      ;; (re)load new, modified
      (scan new old add-file-handlers))
    (set! handlers-table new-table)
    (set! files+times new)))

(provide handlers-watcher shutdown-handlers)

(define current-watcher (make-parameter #f))

(define (handlers-watcher ready-sema)
  (parameterize ([current-directory *handlers-dir*])
    (current-watcher (current-thread))
    (scan-handlers)
    (semaphore-post ready-sema)
    (let loop () (sleep 15) (scan-handlers) (loop))))

(define (shutdown-handlers)
  (define w (current-watcher))
  (current-watcher #f)
  (when w (break-thread w) (thread-wait w))
  (scan-handlers #t))
