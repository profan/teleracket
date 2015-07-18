#lang racket

(require json)
(require net/url net/uri-codec)
(require racket/sandbox racket/date)
(require (only-in "defs.rkt"
               bot-token
               bot-poll-interval
               bot-max-duration
               bot-max-memory))

; data definitions
(date-display-format 'rfc2822)
(define base-url "https://api.telegram.org")
(define bot-source "https://github.com/profan/teleracket")

; utility functions
(define (url-open url)
  (let* ([input (get-pure-port (string->url url) #:redirections 5)]
         [response (port->string input)])
    (close-input-port input)
    response))

(define (current-date->string)
  (date->string (seconds->date (current-seconds)) #t))

(define (bot-request base-url bot-token type [params ""])
  (url-open (format "~a/bot~a/~a?~a" base-url bot-token type params)))

(define (make-message recipient message)
  (list (cons 'chat_id (~a recipient)) (cons 'text (~a message))))

(define make-request (curry bot-request base-url bot-token))
(define (fetch-bot-data method [params ""])
  (string->jsexpr (make-request method params)))

; data fetching
(define bot-info (fetch-bot-data 'getMe))
(define bot-name (hash-ref (hash-ref bot-info 'result) 'username))
(define sandbox-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-exceptions #t]
                 [sandbox-eval-limits (list bot-max-duration bot-max-memory)]
                 [sandbox-path-permissions '()])
    (call-with-limits 10 #f
                      (lambda () (make-evaluator 'racket)))))
; TODO set up structure for users where each user has personal sandboxes
; ... and fix permissions

(define (handle-command sender command)
  (match command
    [(pregexp (format "/eval(?:@~a)?\\s+(.+)" bot-name) (and x (list _ ...))) ; first is string, second is regexp match
     (define result (with-handlers
                        ([exn:fail? (lambda (e) (format "evaluation error: ~a" e))])
                      (sandbox-eval (call-with-input-string (cadr x) read))))
     (define output (get-output sandbox-eval))
     (cond
       [(string? output) (string-append output (~a result))]
       [else result])]
    [(pregexp (format "/create(?:@~a)?\\s+([^\\s]+)\\s+([^\\s]+)" bot-name) (and x (list _ ...)))
     (define name (cadr x))
     (define lang (caddr x))
     (displayln (format "[~a] Set up Sandbox \"~a\" with language ~a for ~a"
                          (current-date->string) name lang sender))
     (format "Your Sandbox \"~a\" with language ~a has been set up :)" name lang)]
    [(pregexp (format "/list(?:@~a)?" bot-name))
     (displayln (format "[~a] Listing available sandboxes for: ~a" (current-date->string) sender))
     ""]
    [(pregexp (format "/source(?:@~a)?" bot-name))
     bot-source]
    [(pregexp "/.+") (format "Unknown Command: ~a" command)]
    [_ #f]))

(define (handle-updates last-offset)
  (define update
    (fetch-bot-data 'getUpdates
                    (alist->form-urlencoded
                     (list (cons 'offset (~a last-offset))))))
  (define messages (hash-ref update 'result))
  (cond
    [(eq? messages '())
     (begin (sleep bot-poll-interval) (handle-updates last-offset))]
    [else
       (for/list ([msghash (in-list messages)])
         (define msg (hash-ref msghash 'message))
         (define recipient (hash-ref (hash-ref msg 'from) 'id))
         (cond
           [(hash-has-key? msg 'text)
            (define text (hash-ref msg 'text))
            (define result (handle-command recipient text))
            (cond
              [(not (eq? result #f))
               (make-request 'sendMessage (alist->form-urlencoded (make-message recipient result)))
               (displayln (format "[~a] Served Request: ~a to ~a, sent result: ~a"
                                  (current-date->string) text recipient result))])]))
     (define offset (hash-ref (last messages) 'update_id))
     (handle-updates (add1 offset))]))

(displayln (format "~a Serving Requests..." bot-name))
(handle-updates 0)