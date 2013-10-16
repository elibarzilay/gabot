#lang racket/base

;; Configuration and globals

(require "utils.rkt" racket/runtime-path)

;; IRC server & port to connect to
(defshared *server* (get-conf 'host "irc.freenode.org"))
(defshared *port*   (get-conf 'port 6667))

;; nickname to use
(defshared *nick* (sym (get-conf 'nick)))
(defshared *name*
  (get-conf 'name (format "Eli's IRC bot, running as `~a'" *nick*)))
(defshared *password* (get-conf 'password))

;; initial channels
(defconst *init-channels* (map sym (get-conf 'initial-channels '())))
;; channels that we're actually on
(defshared *channels* (make-hasheq))

(defconst *local-controller* (get-conf 'local-controller))

;; maximum line length to send out (to avoid floods)
(defconst *max-output-line* 510) ; rfc says no more than 512, including crlf

;; delay between two lines
(defconst *output-line-delay* 0.4)

;; used for single-threaded output
(defshared *output-channel* #f)

;; directories
(define-runtime-path *source-dir* ".")
(defconst *handlers-dir*
  (get-conf 'handlers-dir (build-path *source-dir* "handlers")))

;; handlers
(defshared *pre-handlers* #f) ; allows "listening" to other events (shared)
(defglobal *re-handle* void)  ; use this to jump to a new handler

;; message-related state
(defglobal *line*   #f) ; message info as a list: (list cmd who . params)
(defglobal *cmd*    #f) ; irc command (integer or symbol)
(defglobal *who*    #f) ; nick of originator
(defglobal *user*   #f) ; username of originator
(defglobal *host*   #f) ; host of originator
(defglobal *params* #f) ; command parameters
(defglobal *text*   #f) ; the text that was said/actioned/etc
(defglobal *sent*   #f) ; parsed *text*: ((token:sym from:int to:int) ...)
(defglobal *place*  #f) ; where do we say stuff by default (usually a channel)

;; how long before we ping the server (freenode pings every ~150 seconds)
(defconst  *ping-freq*    (* 60 3))
;; how long before we decide that we lost our connection
(defconst  *ping-give-up* (* 60 5))
;; last ping time
(defshared *last-ping* (current-seconds))

;; what activity to show on stdout
(defconst *verbose* (get-conf 'verbose))

;; channel(s) to log, and where to log to
(defconst *places-to-log*
  (for/list ([place+dir (get-conf 'places-to-log '())])
    (list (sym (car place+dir)) (expand-user-path (cadr place+dir)))))
