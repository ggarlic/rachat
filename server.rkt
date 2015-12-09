#lang racket

(provide launch-rachat-server)

(require 2htdp/image 2htdp/universe "shared.rkt")

(define u0 empty)

(define (launch-rachat-server)
  (universe u0
            (on-new connect)
            (on-msg handle-msg)))

(define (connect u client)
  (make-bundle (append u (list client))
               empty
               empty))

(define (handle-msg u client msg)
  (local [(define message (list (iworld-name client) msg))]
    (make-bundle u
                 (map (lambda (c) (make-mail c message))
                      u)
                 empty)))

(launch-rachat-server)
