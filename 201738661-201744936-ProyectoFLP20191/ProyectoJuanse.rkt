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
  '( (white-sp
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
  '(
    (programa ( "/-" exp-batch  "-/") a-program)
    (exp-batch (expression (arbno expression)) a-batch)
    (expression (number) lit-number)
    (expression (text) lit-id)
    (expression ("\"" text "\"") lit-text)
    (expression ("true") true-val)
    (expression ("false") false-val)
    (expression ("nil") empty-val)
    (expression ("declare" text "=" expression ";") variable-exp)
    (expression (unary-operation expression) unary-expression)
    (expression ("if"   expression  (arbno "then") expression
                        (arbno "elsif" expression (arbno "then") expression )
                        "else" expression "end") condicional-exp)
    (expression ("puts" (separated-list expression ",") ";") print-expression)
    (expression ("(" expression primitive-bin expression (arbno primitive-bin expression) ")") primitive-exp)
    (expression ("for" text "in" number ".." number expression "end") for-exp)
    (expression ("return" expression ";") return-exp)
    (expression ("def" text "(" (separated-list text ",") ")" expression "end") proc-exp)
    
    (primitive-bin ("+") sum)
    (primitive-bin ("-") subd)
    (primitive-bin ("*") mult)
    (primitive-bin ("/") div)
    (primitive-bin ("%") mod)
    (primitive-bin ("**") pow)
    (primitive-bin (">") higher)
    (primitive-bin (">=") higher-eq)
    (primitive-bin ("<") less)
    (primitive-bin ("<=") less-eq)
    (primitive-bin ("==") equal)
    (primitive-bin ("!=") different)
    (primitive-bin ("and") and)
    (primitive-bin ("&&") and)
    (primitive-bin ("or") or)
    (primitive-bin ("||") or)
    (primitive-bin ("..") in-range)
 
    ;(assign-op ("+=") add-eq)
    ;(assign-op ("-=") diff-eq)
    ;(assign-op ("*=") mult-eq)
    ;(assign-op ("/=") div-eq)
    ;(assign-op ("**=") pow-eq)

    (unary-operation ("not") not-op)
    (unary-operation ("!") not-op)

    
    ;(expression-simple (prim-value (arbno complement) ";") primitive-bin-expression)
    ;(expression-simple (prim-value ) primitive-bin123)
    ;(expression-simple (prim-value (arbno complement) ";") primitive-bin-expression)
    
    ;(prim-value ("\"" text "\"") text-value)
    ;(prim-value (text) id-value)
    ;(prim-value (number) number-value)

    ;(complement ("=" expression) assign)
    ;(complement (primitive-bin expression) prim-op)
    

    
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

;********************************************************************************)(***********

(interpreter)
