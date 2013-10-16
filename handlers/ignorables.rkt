#lang s-exp "../handler.rkt"

;; IRC commands that are silently ignored

(define-syntax ignore-commands
  (syntax-rules ()
    [(_ cmd ...) (begin (defhandler cmd (void)) ...)]))

(ignore-commands
 001 ; RPL_WELCOME
 002 ; RPL_YOURHOST
 003 ; RPL_CREATED
 004 ; RPL_MYINFO
 005 ; RPL_BOUNCE
 251 ; RPL_LUSERCLIENT
 252 ; RPL_LUSEROP
 253 ; RPL_LUSERUNKNOWN
 254 ; RPL_LUSERCHANNELS
 255 ; RPL_LUSERME
 250 ; RPL_STATSDLINE
 265 ; RPL_LOCALUSERS
 266 ; RPL_GLOBALUSERS
 328 ; ???
 333 ; RPL_TOPICWHOTIME
 366 ; RPL_ENDOFNAMES
 375 ; RPL_MOTDSTART
 372 ; RPL_MOTD
 376 ; RPL_ENDOFMOTD
 )
