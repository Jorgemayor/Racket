#lang eopl
; Taller 2 Fundamentos de lenguaje de programacion
; 
; sumasanidadas-datatypes.rkt
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
;-------------------------------------------------------------------------------

;<sumaAnidada> ::= (<int>)
;              ::= (<int> <sumaAnidada>)

;-------------------------------------------------------------------------------
;DATATYPE
;-------------------------------------------------------------------------------
;"sumaAnidada" is defined as an abstract syntax
;The datatype "sumaAnidada" is defined, with the constructors
;"valorType"; that defines a --- and "sumaType"; that
;defines a 
;sumaAnidada?: Validates if the given data is a sumaAnidada.
(define-datatype sumaAnidada sumaAnidada?
  (valorType (x number?))
  (sumaType (x number?)
            (body sumaAnidada?))
  )

;-------------------------------------------------------------------------------
;sumaAnidada instantiations
(define a
  (sumaType 4        
            (sumaType 5 (valorType 6))
            )
  )

(define b
  (sumaType 4        
            (valorType 3)
            )
  )

;Unparsed instantiations
(define listaA '(sumaAnidada (valor 4) (sumaAnidada (valor 5) (valor 6))))
(define listaB '(sumaAnidada (valor 4) (valor 3)))

;-------------------------------------------------------------------------------
;unparseTree: exp {sumaAnidada}
;             -> {list}
;Purpose:
;Takes a sumaAnidada and unparses it into a list, analyzing
;each case of the gramatic.
(define unparse
  (lambda (exp)
    (cases sumaAnidada exp
      (valorType (x) (list 'valor x))
      (sumaType (x body)
                (list 'sumaAnidada (list 'valor x) (unparse body))))))

;Pruebas:
(unparse a)
(unparse b)

;-------------------------------------------------------------------------------
;parseTree: dato {list}
;           -> {sumaAnidada}
;Purpose:
;Takes a list and parses it into an abstract syntax type,
;defined by sumaAnidada; analyzing if the given list fixes with
;the definition of the datatype.
(define parse
  (lambda (dato)
    (cond
      [(number? (cadr dato)) (valorType (cadr dato))]
      [(pair? dato)
       (if  (eqv? (car dato) 'sumaAnidada)
            (sumaType (cadr(cadr dato))
                      (parse (caddr dato)))
            ('invalido)
            )]
      (else 'invalido))))

;Pruebas:
(parse listaA)
(parse listaB)
