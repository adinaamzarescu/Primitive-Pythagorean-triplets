#lang racket

(require "ppt.rkt")

(provide (all-defined-out))


(define (a1 e f) (+ (* e e) (* 2 f e)))
(define (b1 e f) (+ (* 2 f f) (* 2 f e)))
(define (c1 e f) (+ (* 2 f f) (* e e) (* 2 f e)))

(define (a2 e f) (- (* e e) (* 2 f e)))
(define (b2 e f) (- (* 2 f f) (* 2 f e)))
(define (c2 e f) (+ (* 2 f f) (* e e) (* -2 f e)))

(define (a3 e f) (- (* f f) (* e e)))
(define (b3 e f) (* 2 f e))
(define (c3 e f) (+ (* f f) (* e e)))

(define (check e f)
  (let ((a1 (a1 e f)) (b1 (b1 e f)) (c1 (c1 e f))
                      (a2 (a2 e f)) (b2 (b2 e f)) (c2 (c2 e f))
                      (a3 (a3 e f)) (b3 (b3 e f)) (c3 (c3 e f)))
    (display (list (list a1 b1 c1) (list a2 b2 c2) (list a3 b3 c3)))
    (newline)
    (and
     (= (+ (sqr a1) (sqr b1)) (sqr c1))
     (= (+ (sqr a2) (sqr b2)) (sqr c2))
     (= (+ (sqr a3) (sqr b3)) (sqr c3)))))


;; (map check
;;      '(1 1 2 2 1 4 4 2 5 5 2 3 3)
;;      '(2 4 5 3 6 9 7 9 12 8 7 8 4))


; First of all tuple will store the n-th quadruple
; e will store the second element of tuple and f the third
; the λ function applies the functions listed and stores the result
; modulo is so the number are between 0 and 26
(define (key n)
    (let* ([tuple (get-nth-quadruple n)] [e (car(cdr tuple))] [f (caddr tuple)])
    (foldr (λ(function acc) (cons (modulo (function e f) 27) acc)) '() (list a1 b1 c1 a2 b2 c2 a3 b3 c3))))


; 32 is the ASCII code for space and from 97 the
; characters for letters begin.
; So when the number 96 is substracted from the ASCII
; code for a(97) for example, it will result 1, which
; means its position in the alphabet, the first one.
; char->integer is used to convert every char to its
; ASCII code and string->list converts the whole
; sentance into a list of characters
(define (message->codes message)
   (let ([L (map char->integer (string->list message))])
    (foldr (λ(carac acc) (cons [if (equal? carac 32)
                                   0
                                   (- carac 96)] acc)) '() L)))

; This time we receive a list so there is no need for let
; The method is similar to message->codes
(define (codes->message codes)
    (list->string (foldr (λ(carac acc) (cons (integer->char [if (zero? carac)
                                                                32
                                                                (+ carac 96)]) acc)) '() codes)))


; There are 2 cases.
; The legth of the key is greater than the size
; => the key will be shortened. The "take" function will do this
; The length of the key is less than the size
; => the key must be extended. The key will be added again
(define (extend-key key size)
    (let my-function ([key key] [size size])
    (if [>= (length key) size]
        (take key size)
        (my-function (append key key) size))))


; This helper receives the souce, the key and a function
; depending if the message is encrypted(+) or decrypted(-)
;   c = (m + k) mod 27          
;   m = (c - k) mod 27
(define (encrypt-decrypt-code source key function)
  (map (λ(a b) (modulo (function a b) 27)) source key))

; Before the  encryption/decryption the key must be extended
(define (encrypt-codes source key)
  (encrypt-decrypt-code source (extend-key key (length source)) +))
(define (decrypt-codes source key)
  (encrypt-decrypt-code source (extend-key key (length source)) -))



; For messages, they must be converted to codes before any operation
; The function in this case is encrypt-codes or decrypt-codes.
; In the end the code will be converted again to a message.
(define (encrypt-decrypt-message string key function) 
  (let ([code (message->codes string)])
    (codes->message (function code (extend-key key (length code))))))

(define (encrypt-message string key)
  (encrypt-decrypt-message string key encrypt-codes))
(define (decrypt-message string key)
  (encrypt-decrypt-message string key decrypt-codes))
