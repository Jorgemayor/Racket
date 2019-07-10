#lang eopl
; Taller 4 Fundamentos de lenguaje de programacion
; 
; 201738661-201744936-Taller4FLP
; 
; Developers:
; 
; Jorge Eduardo Mayor Fernandez
; Code: 201738661
; 
; Juan Sebastian Velasquez Acevedo
; Code: 201744936

;-------------------------------------------------------------------------------
;******************************************************************************************
;;;;; Simple Interpreter

;; Definition BNF for language expressions:

;;<programa> := (un-programa) <expresion>


;;<expresion> := (numero-lit) <numero>
;;            := (texto-lit)"<letras>"
;;            := (primitiva-exp) <primitiva> [expresion (expresion*) (;)]
;;            := (identificador-lit) <identificador>
;;            := (condicional-exp) Si <expresion> entonces <expresion> sino <expresion> fin
;;            := (variableLocal-exp) declarar (<identificador> = <expresion> (;)) haga <expresion> fin
;;            := (procedimiento-exp) procedimiento [<identificador>*';'] haga <expresion> fin
;;            ::= (letrec) letrec (<identificador> (<identificador> ,)* = <expresion>)* in <expresion> 

;;<primitiva> := (suma) +
;;            := (resta) -
;;            := (div) /
;;            := (multiplicacion) *
;;            := (concat) concat
;;            := (length) length

;******************************************************************************************

;******************************************************************************************
;Lexical Specification

(define scanner-lexical-specification
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit "." (arbno digit)) number)
    (number
     (digit "." (arbno digit)) number)
    (number
     ("-" digit "." (arbno digit)) number)
    (text
     ("\"" letter (arbno (or letter digit)) "\"") string)
    (identifier
     (letter (arbno (or letter digit "?"))) symbol))
  )

;Syntactic specification (grammar)

(define grammar-syntatic-specification
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion (text) texto-lit)
    (expresion (identifier) identificador-lit)
    (expresion (primitiva "[" expresion (arbno ";" expresion) "]") primitiva-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "fin") condicional-exp)
    (expresion ("declarar" "(" (separated-list identifier "=" expresion ";") ")" "haga" expresion "fin") variableLocal-exp)
    (expresion ("procedimiento" "[" (separated-list identifier ";") "]" "haga" expresion "fin") procedimiento-exp)
    (expresion ("evaluar" expresion "enviando" "[" (separated-list expresion ";") "]" "fin") proc-evaluacion-exp)
    (expresion ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion)  "in" expresion) letrec-exp)
    (primitiva ("+") suma)
    (primitiva ("-") resta)
    (primitiva ("*") multiplicacion)
    (primitiva ("/") div)
    (primitiva ("concat") concat)
    (primitiva ("length") length)
    )
  )

;Data types built automatically:

(sllgen:make-define-datatypes scanner-lexical-specification grammar-syntatic-specification)

;Test
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-lexical-specification grammar-syntatic-specification)))
(show-the-datatypes)
;*******************************************************************************************
;Parser, Scanner, Interface

;The FrontEnd (Lexicon Analyzer (scanner) y syntactic (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-lexical-specification grammar-syntatic-specification))

;Lexicon Analyzer (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-lexical-specification grammar-syntatic-specification))

; Tests
(scan&parse "-[55]")
(scan&parse "-[5;1]")
(scan&parse "\"ghf\"")

;The Interpreter (FrontEnd + evaluation + sign for reading )

(define interpreter
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (eval-program  pgm))
                        (sllgen:make-stream-parser 
                         scanner-lexical-specification
                         grammar-syntatic-specification)))

;*******************************************************************************************
;eval-program: <programa> -> number |string | symbol
; Purpose: function that evaluates a program 

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                   (eval-expression body (init-env))))))

; Initial Enviroment
(define init-env
  (lambda ()
    (extend-env
     '(a b c)
     '(1 2 3)
     (empty-env))))

;valor-verdad? determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;eval-expression: <expression> -> number || string
; Purpose: Evaluate the expression using cases to determine which datatype is,
; it is used in eval-program. 
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (texto-lit (datum)
                 datum)
      (numero-lit (characters)
                  characters)
      (identificador-lit (identificador)
                         (buscar-variable env identificador))
      (condicional-exp (predicado expVerdad expFalso)
                       (if (valor-verdad? (eval-expression predicado env))
                           (eval-expression expVerdad env)
                           (eval-expression expFalso env)))
      (variableLocal-exp (ids rands body)
                         (let ([args (eval-rands rands env)])
                           (eval-expression body (extendido ids args env))
                           )
                         )
      (procedimiento-exp (ids body)
                         (cerradura ids body env))
      (proc-evaluacion-exp (rator rands)
                           (let ([proc (eval-expression rator env)]
                                 [args (eval-rands rands env)])
                             (if (procedimiento? proc)
                                 (apply-procedure proc args)
                                 ("Attemp to apply non-procedure ~s" proc))
                             )
                           )
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (recursively-extended-env-record proc-names idss bodies env))
                  )
      (primitiva-exp (prim exp rands)
                     (if (null? rands)
                         (apply-primitive prim (list (eval-expression exp env)))
                         (apply-primitive prim (cons (eval-expression exp env) (map (lambda (x) (eval-expression x env)) rands)))
                         )
                     )
      )
    )
  )


; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))


;apply-primitive: <primitiva> <list-of-expression> -> number || string
;Purpose: Operates the list of expression(at least one expression acording to grammar)
; depending on what primitive is, which is identified with cases.
; This procedure is used in  eval-expression.

(define apply-primitive
  (lambda (prim args)
    (if (null? (cdr args))
        (cases primitiva prim
          (length () (string-length (car args)))
          (concat () (car args))
          (default (car args))
          )
        (cases primitiva prim
          (suma () (+ (car args) (apply-primitive prim (cdr args))))
          (resta () (- (car args) (apply-primitive prim (cdr args))))
          (multiplicacion () (* (car args) (apply-primitive prim (cdr args))))
          (div () (/ (car args) (apply-primitive prim (cdr args))))
          (concat () (string-append (car args) (apply-primitive prim (cdr args))))
          (length ()  (string-length (car args)))
          )
        )
    )
  )

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (vacio)
  (extendido (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env ambiente?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env ambiente?))
  )

(define scheme-value? (lambda (v) #t))

;empty-env:      -> ambiente
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (vacio)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> ambiente -> ambiente
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extendido syms vals env))) 

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases ambiente env
      (vacio ()
                        (eopl:error 'buscar-variable "No binding for ~s" sym))
      (extendido (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ([pos (list-find-position sym proc-names)])
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                        (list-ref bodies pos)
                                                        env)
                                             (buscar-variable old-env sym))))
      )
    )
  )


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;******************************************************************************************
;Procediminetos

(define-datatype procedimiento procedimiento?
  (cerradura
   (ids (list-of symbol?))
   (body expresion?)
   (env ambiente?))
  )

(define apply-procedure
  (lambda (proc args)
    (cases procedimiento proc
      (cerradura (ids body env)
                 (eval-expression body (extendido ids args env)))
      )
    )
  )

(interpreter)


;Pruebas:

;declarar (x=2;y=3) haga declarar (t=4;a = procedimiento[x;y;z] haga +[x;y;z] fin) haga evaluar a enviando [1;2;3] fin fin fin

;-------------------------------------------------------------------
;PUNTO 12: factorial de un numero n de forma recursiva
;-------------------------------------------------------------------
;letrec
;fact (x) = Si x entonces *[ x; evaluar fact enviando [-[x;1]] fin] sino 1 fin
;in
;evaluar fact enviando [6] fin
;-------------------------------------------------------------------