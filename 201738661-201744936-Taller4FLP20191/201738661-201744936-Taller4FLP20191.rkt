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
     (letter (arbno (or letter digit))) string))
  )

;Syntactic specification (grammar)

(define grammar-syntatic-specification
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion ("\"" text "\"") texto-lit)
    (expresion (text) id)
    (expresion
     (primitiva "[" expresion (arbno ";" expresion) "]")
     primitiva-exp)
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
                 (eval-expression body)))))


;eval-expression: <expression> -> number || string
; Purpose: Evaluate the expression using cases to determine which datatype is,
; it is used in eval-program. 
(define eval-expression
  (lambda (exp)
    (cases expresion exp
      (texto-lit (datum) datum)
      (numero-lit (characters) characters)
      (id (identificador) (string->symbol identificador))
      (primitiva-exp (prim exp rands)
                     (if (null? rands)
                         (apply-primitive prim (list (eval-expression exp)))
                         (apply-primitive prim (cons (eval-expression exp) (map (lambda (x) (eval-expression x)) rands)))
                         )
                     )
      )
    )
  )

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