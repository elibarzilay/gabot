#lang racket/base

;; hostname and port to connect to
;; (define port 6667)
;; (define host "irc.freenode.org")

;; login information
(define nick 'myname) ; must be lower case, no regexp specials
;; (define name "Eli's IRC bot, running as `myname'")
;; this is plain code, so you can use other ways to get a password
(define password "My Secret Password")

;; channels to join after starting, a list of symbols (or strings)
;; naming the channels, starting with a "#"
;; (define initial-channels '())

;; should the bot start a local controller -- currently either `#f' or
;; `#t' for reading input from stdin and spitting it as raw IRC commands
;; (define local-controller #t)

;; verbosity, can hold 'bot->server and/or 'server->bot to report the
;; traffic in the respective direction (on stdout)
;; (define verbose '())

;; if you want to log some channels, add them to this list as items of
;; ["channel" "log-directory"] (note that directory names go through
;; `expand-user-path')
;; (define places-to-log '())

;; the directory where your handler files are found
;; (define handlers-dir "<here>/handlers")
