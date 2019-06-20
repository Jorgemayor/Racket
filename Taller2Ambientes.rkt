#lang eopl


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


;------------------------------------------------------------------------------------------------------------------------------------------------------

;Punto 2
(define-datatype sumaAnidada sumaAnidada?
  (valorType (x number?))
  (sumaType (x number?) (body sumaAnidada?))
 )

;Unparse y Parse

(define unparse
  (lambda (exp)
    (cases sumaAnidada exp
      (valorType (x) (list 'valor x))
      (sumaType (x body)
                (list 'sumaAnidada (list 'valor x) (unparse body))))))


(define parse
  (lambda (dato)
    (cond
      [(number? (valor->numero dato)) (valorType (valor->numero dato))]
      [(pair? dato)
       (if  (eqv? (car dato) 'sumaAnidada)
           (sumaType (cadr(valor->numero dato))
                     (parse (caddr dato)))
           ('invalido)
           )]
      (else 'invalido))))

;codigo cliente

(define suma->valDer
  (lambda (l)
    (caddr l)))

(define suma->valIzq
  (lambda (l)
    (cadr l)))

(define valor->numero
  (lambda (l)
    (cadr l)))

(define valor?
  (lambda (l)
    (eqv? (car l) 'valor)))

(define suma-anidada
  (lambda (exp)
    (if (valor? exp)
        (valor->numero exp)
        (+ (suma-anidada (suma->valIzq exp))
           (suma-anidada (suma->valDer exp))
           )
        )
    )
  )

(define a
  (sumaType 4        
        (sumaType 5 (valorType 6) )
        ))

(define listaA '(sumaAnidada (valor 4) (sumaAnidada (valor 5) (valor 6))))

(unparse a)
(parse listaA)
(suma-anidada (unparse a))

;-------------------------------------------------------------------------------
;punto 3


