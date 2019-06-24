//Encabezado

;Punto 1

;Sirve para cualquier base, solo hay que modificar la constante BASE
(define BASE 16)

(define zero (lambda () '()))

(define is-zero? (lambda (n) (null? n)))

(define successor (lambda (n)
                    (cond [(is-zero? n) (list 1)]
                          [(and (< (car n) (- BASE 1) ) (>= (car n) 0 ))
                           (cons (+ 1 (car n)) (cdr n))]
                          [else (cons 0 (successor (cdr n)))])))

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


;Codigo Cliente
(define sumaBase
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (sumaBase (predecessor x) y)))))

;pruebas
(sumaBase '(2 4)'(1)) ;(3 4)
(sumaBase '(15 1)'(3 2)); (2 4)



(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))
;pruebas
(resta '(2 4) '(3 2));(15 1)
(resta '(15 1) '(3 2));() da negativo
(resta '(2 4) '(15 1)); (3 2)

(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (sumaBase (multiplicacion (predecessor x) y) y))
    ))

(multiplicacion '(0 1) '(2)); (0 2)


(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))

