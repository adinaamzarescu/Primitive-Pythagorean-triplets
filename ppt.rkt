#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
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
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
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


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
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


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (apply-matrix-transformations (cdr Ts) (multiply
                                              (cond
                                                [(= (car Ts) 1) T1]
                                                [(= (car Ts) 2) T2]
                                                [else (= (car Ts) 3) T3])
                                              ppt))))


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) (list 3 4 5)))
