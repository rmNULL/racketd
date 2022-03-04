#lang racket/base
(require (only-in racket/contract listof)
         (only-in racket/tcp tcp-accept tcp-listen)
         (only-in racket/function curry)
         racket/match
         json)

(define host "127.0.0.1")
(define port 65511)
(define server (tcp-listen port 4 #f host))
(define (println-if-non-void val) (unless (void? val) (println val)))
(define (eval-with-io sexp ns args input output)
  (parameterize
      ([current-namespace ns]
       [current-command-line-arguments args]
       [current-input-port input]
       [current-output-port output])
    (with-handlers
      ([exn?
        (compose displayln exn-message)])
      (if (and (list? sexp) (eq? (car sexp) 'module))
          (eval `(begin ,sexp (require ',(cadr sexp))))
          (println-if-non-void (eval sexp))))))

(define (handle input output)
  (with-handlers ([exn?
                   (compose (curry displayln output) exn-message)])
    (define data
      (let ([json (read-json input)])
        (begin
          (close-input-port input)
          json)))
    (define-values (args stdin value)
      (match data
        [(hash-table
          ('file (? string? input-file-name))
          ('stdin (? string? stdin-file-name))
          ('args (? (listof string?) args-list)))
         (begin
           (values
            (list->vector args-list)
            (open-input-file stdin-file-name)
            (read (open-input-file input-file-name #:mode 'text))))]
        [_ (error "Invalid json")]))
    (eval-with-io value (make-base-namespace) args stdin output)))

(define (main)
  (displayln (format "listening at ~a:~a" host port))
  (let-values ([(input output) (tcp-accept server)])
    (thread
     (λ ()
       (handle input output)
       (close-output-port output)
       (close-input-port input)))
    (main)))

(main)
