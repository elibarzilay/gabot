#lang s-exp "../handler.rkt"

;; Track utterance, construct and re-handle `VERB/foo's when told
;; something.

(define (huh verb)
  (R (tell "Huh?")
     (tell "What?")
     (tell "I didn't get that.")
     (tell #f "Is somebody talking?")
     (tell "~a? Huh?" verb)
     (BG (R (tell #f "*** uncaught exception: ~a: ~a; aborting"
                  "reference to undefined identifier"
                  verb)
            (tell "Program terminated with signal 11, Segmentation fault."))
         (sleep 2)
         (R (tell "Just kidding.")
            (tell "I kid, I kid.")
            (tell "Not.")
            (me "wonders if that looked convincing enough.")))
     (tell "Can I get a garlic bagel with that?")
     (tell "I'll have the chicken, please.")
     (me "is confused")
     (me "is busy gossiping with rudybot")
     (me "stares blankly")
     (me "glances around nervously")
     (me "pretends to be busy with ... stuff")))

(defhandler TOLD-ME
  (match *sent*
    [(list (list verb _ _))
     (set! *sent* '())
     (or (*re-handle* (sym 'VERB/ verb) #f)
         (huh verb))]
    [(list (list verb _ _) (list fst b e) more ...)
     (set! *sent* `((,fst ,b ,e) ,@more))
     (or (*re-handle* (sym 'VERB/ verb) #f)
         (huh verb))]))

(defhandler SAY
  (report)
  (set! *sent* (text->sent *text*))
  (*re-handle* 'UTTERANCE 'all) ; run all 'UTTERANCE handlers
  (match *sent*
    [(list (list (? me?) _ _))
     (tell "what?")]
    [(or (list (list (? me?) _ _) (list fst b e) more ...)
         ;; (list (list fst b e) more ... (list (? me?) _ _))
         )
     (set! *sent* `((,fst ,b ,e) ,@more))
     (*re-handle* 'TOLD-ME)]
    [_ (void)]))

(defhandler (PRIVMSG where text)
  (set! *place* (string->symbol where))
  (set! *text* text)
  (define ctcp (regexp-match #rx"^\1([A-Za-z0-9]+)?(?: +(.*[^ ] *))?\1$" text))
  (cond [(not ctcp)  (*re-handle* 'SAY)]
        [(cadr ctcp) (set! *text* (caddr ctcp))
                     (*re-handle* (sym 'CTCP/ (cadr ctcp)))]
        [else        (error 'PRIVMSG "bad CTCP line: ~a" text)]))

(defhandler (NOTICE where text)
  (set! *place* (string->symbol where))
  (set! *text* text)
  (report #:prefix "(notice)"))
