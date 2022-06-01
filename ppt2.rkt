#lang racket

(provide (all-defined-out))
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;;
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

(define (FQ1 V) (apply Q1 V))
(define (FQ2 V) (apply Q2 V))
(define (FQ3 V) (apply Q3 V))

(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

; In order to avoid recursivity, I will
; use apply as well as map
(define (dot-product X Y)
  (apply + (map * X Y)))


; Map applies the λ function for each element of M
; X will be each line of M and then dot-product will
; be applied to X and V
(define (multiply M V)
  (map (λ(X) (dot-product X V)) M))


(define (get-transformations n)
  (if (= n 1)
      '()
      (append (get-transformations
               (cond
                 [(= (modulo n 3) 1) (quotient (- n 1) 3)]
                 [(= (modulo n 3) 2) (quotient (+ n 1) 3)]
                 [else (quotient n 3)]))
              (list
               (cond
                 [(= (modulo n 3) 1) 3]
                 [(= (modulo n 3) 2) 1]
                 [else 2])))))


; First of all foldl takes every function of the Fs list
; Then the λ function applies that specific function on tuple
; and in the end the result is stored in tuple
(define (apply-functional-transformations Fs tuple)
  (foldl (λ(given-function tuple) (given-function tuple)) tuple Fs))


(define (get-nth-tuple tuple)
  (λ(f)
    (apply-functional-transformations f tuple))) 

; This function returns in acc a list of the order of T1 T2 T3 depending on
; the get-transformation list.
; Ex: transformations = '(1 3 1 2) => acc = (T1 T3 T1 T2)
 (define (get-multiply-order transformations L acc)
  (if (null? transformations)
      (reverse acc)
      (cond [(= (car transformations) 1) (get-multiply-order (cdr transformations) L (cons (car L) acc))]
            [(= (car transformations) 2) (get-multiply-order (cdr transformations) L (cons (car(cdr L)) acc))]
            [(= (car transformations) 3) (get-multiply-order (cdr transformations) L (cons (car(cdr(cdr L))) acc))])))

; get-multiply-order will receive a list of transformations (get-transformations n), the 3 matrix T1 T2 T3 and will return a list
; of the order in which they should be multiplied
; Map will apply the multiply function for each one of those matrix for the (3 5 6) tuple
(define (get-nth-ppt-from-matrix-transformations n)
  ((get-nth-tuple '(3 4 5)) (map (curry multiply) (get-multiply-order (get-transformations n) (list T1 T2 T3) '()))))


; This one is similar but in this case the list is a list of functions, FQ1 FQ2 FQ3
(define (get-nth-quadruple n)
  ((get-nth-tuple '(1 1 2 3)) (get-multiply-order (get-transformations n) (list FQ1 FQ2 FQ3) '())))


; a = gh,   b = 2ef,   c = e^2 + f^2
(define (n-thTTP g e f h)
  (list (* g h) (* 2 e f) (+ (* e e) (* f f))))
(define (get-nth-ppt-from-GH-quadruples n)
  (apply n-thTTP (get-nth-quadruple n)))
