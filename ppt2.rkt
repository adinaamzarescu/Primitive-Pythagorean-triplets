#lang racket

(provide (all-defined-out))
;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

(define (FQ1 V) (apply Q1 V))
(define (FQ2 V) (apply Q2 V))
(define (FQ3 V) (apply Q3 V))

;; Vom refolosi matricile T1, T2, T3:
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


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
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
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).


; First of all foldl takes every function of the Fs list
; Then the λ function applies that specific function on tuple
; and in the end the result is stored in tuple
(define (apply-functional-transformations Fs tuple)
  (foldl (λ(given-function tuple) (given-function tuple)) tuple Fs))


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.
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
; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.

; get-multiply-order will receive a list of transformations (get-transformations n), the 3 matrix T1 T2 T3 and will return a list
; of the order in which they should be multiplied
; Map will apply the multiply function for each one of those matrix for the (3 5 6) tuple
(define (get-nth-ppt-from-matrix-transformations n)
  ((get-nth-tuple '(3 4 5)) (map (curry multiply) (get-multiply-order (get-transformations n) (list T1 T2 T3) '()))))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.

; This one is similar but in this case the list is a list of functions, FQ1 FQ2 FQ3
(define (get-nth-quadruple n)
  ((get-nth-tuple '(1 1 2 3)) (get-multiply-order (get-transformations n) (list FQ1 FQ2 FQ3) '())))


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
; a = gh,   b = 2ef,   c = e^2 + f^2
(define (n-thTTP g e f h)
  (list (* g h) (* 2 e f) (+ (* e e) (* f f))))
(define (get-nth-ppt-from-GH-quadruples n)
  (apply n-thTTP (get-nth-quadruple n)))
