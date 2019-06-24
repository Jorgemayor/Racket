;Encabezado

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
