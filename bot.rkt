#lang racket/base

;; Main entry point for running the bot

(require "globals.rkt" "utils.rkt" "irc-utils.rkt"
         "parse.rkt" "handlers.rkt" "init.rkt"
         racket/tcp)

;; Utilities

(define bad-char (bytes-ref #"?" 0))

(define (safe-irc-output line)
  ;; limit output size, do not display newlines
  (let* ([line (string->bytes/utf-8 line bad-char)]
         [line (regexp-replace* #rx#"[\r\n]+" line #"<NEWLINE>")])
    (if (> (bytes-length line) *max-output-line*)
      (let ([ctcp? (regexp-match? #rx#"\1$" line)])
        (bytes-append (subbytes line 0 (- *max-output-line* (if ctcp? 4 3)))
                      (if ctcp? #"...\1" #"...")))
      line)))

;; Output

(define (make-output-handler channel server-out)
  (let loop ()
    (define x (channel-get channel))
    (unless (eq? 'QUIT x)
      (define buf (safe-irc-output x))
      (define show? (memq 'bot->server *verbose*))
      (when show? (printf "bot->server: ~a" buf) (flush-output))
      (write-bytes buf server-out)
      (write-bytes #"\r\n" server-out)
      (flush-output server-out)
      (when show? (newline))
      (sleep *output-line-delay*)
      (loop))))

;; Dispatchers

(define reset-state!
  (let ([p (current-preserved-thread-cell-values)])
    (λ() (current-preserved-thread-cell-values p))))

(define no-default (gensym))

(define (irc-dispatch line)
  (when (memq 'server->bot *verbose*) (printf "server->bot ~a\n" line))
  (reset-state!)
  (irc-parse line)
  (let loop ([cmd *cmd*] [fst? #t] [dflt no-default])
    (with-handlers ([void (λ(e) (warn 'irc-dispatch
                                      "error in ~a handler: ~a\n  Line: ~a"
                                      cmd (if (exn? e) (exn-message e) e)
                                      line))])
      ;; A handler can return `#f' to indicate that it wasn't handled,
      ;; or any true value to indicate that it was; running a handler
      ;; (mainly via `*re-handle*') accepts a default to use when no
      ;; handlers were found.  When this default is 'all then all
      ;; handlers for the command will run, their return value is
      ;; ignored, and eventually it will always be considered as handled
      ;; (useful for generic listeners that many handler files want to
      ;; hook to).
      (define (run-handlers hs)
        (cond
          [(pair? hs)
           (if (eq? dflt 'all)
             (begin ((car hs)) (run-handlers (cdr hs)))
             (or    ((car hs)) (run-handlers (cdr hs))))]
          [(eq? dflt no-default)
           (warn 'irc-dispatch "unhandled server command (~.s); ~a" cmd line)]
          [else dflt]))
      (define (handle)
        (run-handlers (append (if fst? *pre-handlers* '())
                              (get-handlers cmd))))
      ;; to allow jumps to different dispatchers (but not using *pre-handlers*)
      (set! *re-handle* (λ(cmd [dflt dflt]) (loop cmd #f dflt)))
      (if cmd
        (handle)
        (warn 'irc-dispatch "line did not parse: ~a" line)))))

(define (local-dispatch line)
  (out "~a" line))

(define (ping-monitor)
  ;; a handler in "handlers/irc.rkt" updates `*last-ping*'
  (sleep 60)
  (define secs (- (current-seconds) *last-ping*))
  (cond [(secs . > . *ping-freq*)
         (warn 'ping-monitor "no pings for ~a seconds, sending a ping" secs)
         (out "PING ~a" *nick*)]
        [(secs . > . *ping-give-up*)
         (error (format "no pings for ~a seconds, giving up" secs))])
  (ping-monitor))

;; Start it all

(define (run)
  ;; make it produce more interactive output for a log file
  (file-stream-buffer-mode (current-output-port) 'line)
  (printf "Starting up bot...\n")
  (define-values [i o] (tcp-connect *server* *port*))
  (define och (make-channel))
  ;; start everything
  (set! *output-channel* och)
  (set! *pre-handlers* (list void)) ; ignore everything until initialized
  (define cust (make-custodian))
  (define errch (make-channel))
  (define (run-thread name run [confirmation? #f])
    (define sema (and confirmation? (make-semaphore 0)))
    (define thd
      (thread (λ() (printf "Running ~a...\n" name)
                   (define (restart e)
                     (channel-put errch (format "Error in ~a: ~a"
                                                name (exn-message e))))
                   (with-handlers ([exn? restart])
                     (if sema (run sema) (run))))))
    (when sema (semaphore-wait sema))
    thd)
  (define (run-dispatcher name inp dispatch done)
    (run-thread name
      (λ() (let loop ()
             (define line (read-bytes-line inp 'any))
             (if (eof-object? line)
               (begin (printf "~a ended.\n" name) (done))
               (begin (dispatch line) (loop)))))))
  (define threads
    (parameterize ([current-custodian cust])
      (list (run-thread "handlers-watcher" handlers-watcher #t)
            (run-thread "initialization" initialize)
            ;; single-threaded output handler
            (run-thread "output handler" (λ() (make-output-handler och o)))
            (run-thread "ping monitor" ping-monitor)
            ;; irc dispatcher
            (run-dispatcher "server listener" i irc-dispatch
              (λ() (out 'QUIT) (channel-put errch "Error: server input")))
            ;; local input reader
            (and *local-controller*
                 (run-dispatcher "bot control listener"
                                 (current-input-port) local-dispatch
                                 (λ() (printf "bot controller done.\n")))))))
  (define break-or-error
    (with-handlers ([exn:break? (λ(_) 'break)])
      (channel-get errch)))
  (shutdown-handlers)
  (flush-output o)
  (close-output-port o)
  (close-input-port i)
  (custodian-shutdown-all cust)
  (if (string? break-or-error)
    (begin (warn 'run "~a" break-or-error)
           (printf "Restarting in 20 seconds...\n") (sleep 20) (run))
    (begin (printf "Bye...\n") (exit 0))))

(run)
