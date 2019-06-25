#lang eopl

; Taller 2 Fundamentos de lenguaje de programacion
; 
; bignum16.rkt
; 
; Desarrolladores:
; 
; Jorge Eduardo Mayor Fernandez
; Codigo: 201738661
; 
; Juan Sebastian Velasquez Acevedo
; Codigo: 201744936


;-------------------------------------------------------------------------------
;GRAMATIC 
;------------------------------------------------------------------------------

;<bignum>::=empty ,  (n=0)
;        ::= (cons r [q]) , (n=qN+r  0<=r <N)

;------------------------------------------------------------------------
;Bignum implementation 16
;------------------------------------------------------------------------

;The Base
(define BASE 16)

; ;zero: ()
; ;      -> null {list}
; ;Purpose:Definition for n = 0, returning an empty list
;;according to the grammar.
(define zero (lambda () '()))

; ;is-zero?: n {lista}
; ;      -> boolean
; ;Purpose:Observer that determines if a data is zero,
;; in this case if it is an empty list.
;; It is used in predecessor and successor entities
(define is-zero? (lambda (n) (null? n)))

; ;succesor: n {lista}
; ;      -> list
; ;Purpose:Entity responsible for returning the successor of
;; a type of data represented in base 16.
;;It is based on the grammar to verify the limits of the
;;numbers depending on the base.

(define successor (lambda (n)
                    (cond [(is-zero? n) (list 1)]
                          [(and (< (car n) (- BASE 1) ) (>= (car n) 0 ))
                           (cons (+ 1 (car n)) (cdr n))]
                          [else (cons 0 (successor (cdr n)))])))

; ;predecessor: n {lista}
; ;      -> list
; ;Purpose:Entity responsible for returning the predecessor of
;; a type of data represented in base 16.
;;It is based on the grammar to verify the limits of the
;;numbers depending on the base.
(define predecessor (lambda (n)
                      (cond [(is-zero? n) '()]
                            [(and (= 0 (car n)) (is-zero? (cdr n))) '()]
                            [(and (is-zero? (cdr n))(= 1 (car n))) '()]
                            [(and (< (car n) BASE ) (> (car n) 0 ))
                             (cons (- (car n) 1) (cdr n))]
                            [else (cons (- BASE 1) (predecessor (cdr n)))])))

;pruebas
(predecessor '(0 1)) ;(15)
(predecessor '(1 1)) ;(0 1)
(predecessor '(0 15)); (15 14)
(successor '(15));(0 1)
(successor '(15 14));(0 15)
(is-zero? (zero))

;;----------------------------------------------------------------------------------
;Codigo Cliente
;;----------------------------------------------------------------------------

; ;sumaBase: x,y {list}
; ;      -> list 
; ;Purpose:Performs the sum operation on hexadecimal numbers represented
;; in lists, using the entities and the defined grammar.
(define sumaBase
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (sumaBase (predecessor x) y)))))

;pruebas
(sumaBase '(2 4)'(1)) ;(3 4)
(sumaBase '(15 1)'(3 2)); (2 4)


; ;resta: x,y {list}
; ;      -> list
; ;Purpose: Performs the operation subtracts on hexadecimal numbers represented
;; in lists, using the entities and the defined grammar.
(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

;pruebas
(resta '(2 4) '(3 2));(15 1)
(resta '(15 1) '(3 2));() returns an empty list because the result is negative
(resta '(2 4) '(15 1)); (3 2)

; ;multiplicacion: x,y {list}
; ;      -> list 
; ;Purpose:Performs the sum operation on hexadecimal numbers represented
;; in lists, using the entities and the defined grammar.
(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (sumaBase (multiplicacion (predecessor x) y) y))
    ))

(multiplicacion '(0 1) '(2)); (0 2)
(multiplicacion '(15 3) '(8));(8 15 1)

;;potencia: x,y {list}
;;      -> list 
;;Purpose:Performs the power operation on hexadecimal numbers represented
;; in lists, using the entities and the defined grammar.
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

(potencia '(15) '(2));(1 14)
(potencia '(5) '(3)); (13 7)

;;factorial: n {list}
;;      -> list 
;; Purpose:Performs the factorial operation on hexadecimal numbers represented
;; in lists, using the entities and the defined grammar.
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))

(factorial '(8));(0 8 13 9)
(factorial '(3));(6)

