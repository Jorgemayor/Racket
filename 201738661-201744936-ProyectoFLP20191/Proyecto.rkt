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
     ("#\newline") skip)
    (blank
     (" ") skip)
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
    (beginsWithExp ((arbno "\n") expression options) a-program)
    ;(batch (arbno expression "}") a-batch)
    (options "\n" lb)
    (options (restOfExp) roe)
    (restOfExp (binary-op expression ";") primitive-exp)
    (restOfExp ("=" expression ";") variable-exp)
    (restOfExp ("+=" expression ";") add-assign)
    (restOfExp ("-=" expression ";") diff-eq)
    (restOfExp ("*=" expression ";") mult-eq)
    (restOfExp ("/=" expression ";") div-eq)
    (restOfExp ("**=" expression ";") pow-eq)
    
    (expression (number) lit-number)
    (expression (text) lit-id)
    (expression ("true") true-val)
    (expression ("false") false-val)
    (expression ("nil") nil-val)
    (expression ("\"" text "\"") lit-text)
    (expression (unary-operation expression) unary-expression)
    (expression ("if"   expression  (arbno "then") (arbno expression)
                        (arbno "elsif" expression (arbno "then") expression)
                        "else" expression "end") condicional-exp)
    (expression ("puts" expression ";") print-expression-puts)
    (expression ("for" text "in" number ".." number expression "end") for-exp)
    (expression ("return" expression ";") return-exp)
    (expression ("def" text "(" (separated-list text ",") ")" expression "end") proc-exp)
    
    (binary-op ("+") sum)
    (binary-op ("-") subd)
    (binary-op ("*") mult)
    (binary-op ("/") div)
    (binary-op ("%") mod)
    (binary-op ("**") pow)
    (binary-op (">") higher)
    (binary-op (">=") higher-eq)
    (binary-op ("<") less)
    (binary-op ("<=") less-eq)
    (binary-op ("==") equal)
    (binary-op ("!=") different)
    (binary-op ("and") and)
    (binary-op ("&&") and)
    (binary-op ("or") or)
    (binary-op ("||") or)
    (binary-op ("..") in-range)

    (unary-operation ("not") not-op)
    (unary-operation ("!") not-op)
    
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