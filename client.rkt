#lang racket

(provide launch-rachat-client)

(require 2htdp/image 2htdp/universe "shared.rkt")


;;InitState
(define InitClientState (list "" empty))

;; Constants
(define WIDTH 600)
(define HEIGHT 400)
(define SEND-BUTTON (overlay (text "SEND" 14 'black) 
                             (circle 20 'solid 'blue)))
(define INPUT-BOX (rectangle (- WIDTH 70) 25 'outline 'black))
(define BACKGROUND (place-image SEND-BUTTON (- WIDTH 30) (- HEIGHT 20)
                                (empty-scene WIDTH HEIGHT)))



(define (launch-rachat-client username host)
  (big-bang InitClientState
            (to-draw render)
            (on-key handle-keys)
            (on-mouse handle-click)
            (on-receive handle-msg)
            (name username)
            (register host)))

(define (render client)
    (local [(define textbox (first client))
          (define textbox-img (text textbox 13 'black))
          (define width/t (image-width textbox-img))
          (define archive (map message->text (second client)))
          (define (render/h archive offset)
            (cond
              [(empty? archive) BACKGROUND]
              [else (local [(define current (first archive))
                            (define as-img (text current 13 'black))
                            (define width (image-width as-img))]
                      (place-image as-img (+ (/ width 2) 10) offset
                                   (render/h (rest archive) (+ 15 offset))))]))]
    (place-image textbox-img (+ (/ width/t 2) 10) (- HEIGHT 15)
                 (render/h archive 20))))

(define (message->text message)
  (string-append (first message) ": " (second message)))

(define (handle-keys client key)
  (cond
    [(string=? "\b" key)
     (list (backspace (first client)) (second client))]
    [(= (string-length key) 1)
     (list (string-append (first client) key) (second client))]
    [else
      client]))

(define (backspace string)
  (cond
    [(<= (string-length string) 1) ""]
    [else
      (string-append (substring string 0 1)
                     (backspace (substring string 1)))]))

(define (handle-click client x y event)
  (if (and (> x (- WIDTH 60)) (> y (- HEIGHT 40)) (string=? event "button-down"))
    (make-package (list "" (second client)) (first client))
    client))

(define (handle-msg client msg)
  (list (first client) (append (second client) (list msg))))

(launch-rachat-client "ggarlic" "localhost")

