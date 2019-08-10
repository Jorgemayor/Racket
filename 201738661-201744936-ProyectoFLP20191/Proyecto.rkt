#lang eopl
; Proyecto Fundamentos de lenguaje de programacion
; 
; 201738661-201744936-ProyectoFLP
; 
; Developers:
; 
; Jorge Eduardo Mayor Fernandez
; Code: 201738661
; 
; Juan Sebastian Velasquez Acevedo
; Code: 201744936

;-------------------------------------------------------------------------------
;;<program> := (a-program) <expression>


;;<expression> := (lit-number) <number>
;;             := (lit-id) <identifier>
;;             := (lit-text) "<letters>"
;;             := (primitive-exp) expression <primitive> (\n)* expression (or ; \n) (\n)*
;;             := (condicional-exp) if (\n)* <expression> (\n)* <expresion> (or ; (\n)*) else (\n)* <expresion> (or ; (\n)*) end
;;             := (variable-exp) <identifier> = (\n)* <expression> (or ; \n) (\n)*

;;<primitive> := (sum) +
;;            := (subs) -
;;            := (div) /
;;            := (mult) *

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

(define scanner-lexical-specification
  '((line-break
     ("\n") skip)
    (white-sp
     (whitespace) skip)
    (comment
     ("=begin" (arbno (or letter digit #\newline)) "=end") skip)
    (comment
     ("#" (arbno (not #\newline))) skip)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (number
     (digit "." digit (arbno digit)) number)
    (number
     ("-" digit "." digit (arbno digit)) number)
    (text
     (letter (arbno (or letter digit "?"))) string))
  )


(define grammar-syntatic-specification
  '((programa (expression) a-program)
    (expression (number) lit-number)
    (expression (text) lit-id)
    (expression ("\"" text "\"") lit-text)
    ;(expression (expression primitive (arbno "#\newline") expression (arbno "#\newline")) primitive-exp)
    ;(expression ("if " (arbno "#\newline") expression "#\newline" (arbno "#\newline") expression (arbno "#\newline") "else " (arbno "#\newline") expression (arbno "#\newline") "end" "#\newline") condicional-exp)
    ;(expression (text "=" expression ";" (arbno "#\newline")) variable-exp)
    (primitive ("+") sum)
    (primitive ("-") subd)
    (primitive ("*") mult)
    (primitive ("/") div)
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

;The Interpreter (FrontEnd + evaluation + sign for reading )

(define interpreter
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) pgm)
                        (sllgen:make-stream-parser 
                         scanner-lexical-specification
                         grammar-syntatic-specification)))

;*******************************************************************************************

(interpreter)