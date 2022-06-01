#lang racket

(provide (all-defined-out))

(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

(define naturals
  (let nat ([seed 0])
    (stream-cons seed (nat (add1 seed)))))

(define (dot-product X Y)
  (apply + (map * X Y)))

(define (multiply M V)
  (map (λ(X) (dot-product X V)) M))


; For the '(3 4 5) tuple each new element will be the previous ones
; multiplied. then stream-rest will take each tuple to multiply
(define ppt-stream-in-tree-order
  (let f ([queue (stream-cons '(3 4 5) empty-stream)])
    (stream-cons (stream-first queue) (f (stream-append (stream-rest queue)
                                                        (list (multiply T1 (stream-first queue)))
                                                        (list (multiply T2 (stream-first queue)))
                                                        (list (multiply T3 (stream-first queue))))))))

 ;(stream->list (stream-take ppt-stream-in-tree-order 20))

;;    g, h impare
;;    g < h
;;    g, h prime între ele
;;
;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2
;;
;;                        h      
;;         3     5     7     9     11   .  .  .
;;    1  (1,3) (1,5) (1,7) (1,9) (1,11) .  .  .
;;    3        (3,5) (3,7)   -   (3,11) .  .  .
;;    5              (5,7) (5,9) (5,11) .  .  .
;; g  7                    (7,9) (7,11) .  .  .
;;    9                          (9,11) .  .  .
;;    .                                 .  .  .
;;    .                                    .  .
;;    .                                       .
;;
;;    (3,4,5)                           
;;    (5,12,13), (15,8,17)              
;;    (7,24,25), (21,20,29), (35,12,37) 
;;
;;    * G = 1, 3, 5, 7 ...
;;    * H = 3, 5, 7, 9 ... (g < h)


(define (pairs G H)
  (let help ([g G] [h H] [n 0] [max 0])
    (stream-cons (cons (stream-first g)(stream-first h)) (if (not(equal? n max))
                                                             (help (stream-rest g) h (+ n 1) max)
                                                             (help G (stream-rest h) 0 (+ 1 max))))))

(define odd
  (let nat ([seed 1])
    (stream-cons seed (nat (+ 2 seed)))))

(define gh-pairs-stream
  (stream-filter (λ(pair) (if (= (gcd (car pair) (cdr pair)) 1) #t #f)) (pairs odd (stream-rest odd))))


;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2
(define ppt-stream-in-pair-order
  (stream-map (λ(pair) (let ([g (car pair)] [h (cdr pair)])
                         (list (* g h) (/ (- (expt h 2) (expt g 2)) 2) (/ (+ (expt h 2) (expt g 2)) 2)))) gh-pairs-stream))

