#lang racket/base

;; Logging facility, for by-place log messages and a different file every day.

(require "globals.rkt" "utils.rkt" racket/list racket/file)

;; Send this thread messages to produce logged output.  A message value is
;; (list message place ...), it holds some text and several places where this
;; message applies to with `*places-to-log*' mapping places to directories
;; where log files (one per day) are put.  The message can also be eof to close
;; all log files.  It also maintains in each log directory a "current" file
;; that holds the name of the current log file, and "recent.txt" which is a
;; symlink to it.
(provide logger-thread)
(define logger-thread
  (let ()
    (define ports (make-hasheq))
    (define (make-port dir YMD)
      (define file (format "~a.txt" YMD))
      (display-to-file file (build-path dir "current") #:exists 'truncate)
      (let ([recent (build-path dir "recent.txt")])
        (when (link-exists? recent) (delete-file recent))
        (make-file-or-directory-link file recent))
      (open-output-file (build-path dir file) #:exists 'append))
    (define (get-port place dir YMD)
      (define ymd+p (hash-ref ports place #f))
      (if (and ymd+p (= YMD (car ymd+p)))
        (cdr ymd+p)
        (let ([new (cons YMD (make-port dir YMD))])
          (when ymd+p (close-output-port (cdr ymd+p)))
          (hash-set! ports place new)
          (cdr new))))
    (define (pad2 n)
      (define s (number->string n))
      (if (< n 10) (string-append "0" s) s))
    (define (handle msg)
      (define line (car msg))
      (define places+dirs
        (filter-map (λ(p) (assq p *places-to-log*)) (cdr msg)))
      (when (pair? places+dirs)
        (define date (seconds->date (current-seconds)))
        (define time (+ (* (date-year  date) 10000)
                        (* (date-month date) 100)
                        (* (date-day   date) 1)))
        (define stamp
          (format "~a:~a " (pad2 (date-hour date)) (pad2 (date-minute date))))
        (for ([place+dir (in-list places+dirs)])
          (define place (car place+dir))
          (define dir   (cadr place+dir))
          (define o (get-port place dir time))
          (for ([x (in-list (cons stamp line))]) (display x o))
          (flush-output o))))
    (thread
     (λ() (let loop ()
            (define msg (thread-receive))
            (if (eq? eof msg)
              (begin (hash-for-each ports (λ(_ x) (close-output-port (cdr x))))
                     (kill-thread logger-thread))
              (begin (handle msg) (loop))))))))

(on-exit
 (λ() (printf "Closing log ports.\n")
      (thread-send logger-thread eof)
      (thread-wait logger-thread)))
