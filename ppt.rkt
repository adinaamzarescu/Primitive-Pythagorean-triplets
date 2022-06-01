#lang racket

(provide (all-defined-out))

;;    a^2 + b^2 = c^2
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.

(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; This function will go as long as the 2 lists
; are not empty.
; Then the first 2 elements of each list will be multiplied.
; The dot-product function will be then called with the
; rest of the 2 lists, without the first 2 elements. Then
; all the products will be summed.

(define (dot-product X Y)
  (cond
    [(zero? (length X)) 0]
    [else (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))]
    )
  )


; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; For this function I used a helper function that stores
; the result and reverses the matrix after it was multiplied.
; For each line of M I used dot-product to multipy its elements
; by the elements of V.

(define (multiply-helper M V rez)
  (if (null? M)
      (reverse rez)
      (multiply-helper (cdr M) V (cons (dot-product (car M) V) rez))))

(define (multiply M V)
  (multiply-helper M V '()))



; For each number, since the tree is separated in 3
; branches (for each level), the first step is to find
; the reminder of n % 3.
; For every number that it divided by 3, the transformation
; is in the middle, so the second.
; If the reminder is 1, then it is the 3rd transformation.
; If the reminder is 2 then it is the first transformation.

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


; For every element of Ts I multiply the ppt by
; the matrix that is associated to that element.

(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (apply-matrix-transformations (cdr Ts) (multiply
                                              (cond
                                                [(= (car Ts) 1) T1]
                                                [(= (car Ts) 2) T2]
                                                [else (= (car Ts) 3) T3])
                                              ppt))))


; Firstly I apply the tranformations and then I select the n-th one
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))
