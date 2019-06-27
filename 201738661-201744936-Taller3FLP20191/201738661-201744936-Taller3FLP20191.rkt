#lang eopl

;******************************************************************************************
;;;;; Simple Interpreter

;; Definition BNF for language expressions:

;;<program> := (a-program) <expression>


;;<expression> := (number-lit) <number>
;;            := (text-lit)"<letters>"
;;            := (primitive-exp) <primitive> [expression (expression*) (;)]

;;<primitive> := (sum) +
;;            := (substraction) -
;;            := (div) /
;;            := (multiplication) *
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
    (identifier
     (letter (arbno (or letter digit "?"))) symbol)
    (number
     (digit (arbno digit)) number)
    (numberexpression
     ("-" digit (arbno digit)) number)
    (number
     (digit "." (arbno digit)) number)
    (number
     ("-" digit "." (arbno digit)) number)
    (text
     ((arbno letter)) string))
  )

;Syntactic specification (grammar)

(define grammar-syntatic-specification
  '((program (expression) a-program)
    (expression (number) number-lit)
    (expression ("\"" (arbno text) "\"") text-lit)
    (expression
     (primitive "[" expression (arbno ";" expression) "]")
     primitive-exp)
    (primitive ("+") sum)
    (primitive ("-") substrac)
    (primitive ("*") mult)
    (primitive ("/") div)
    (primitive ("concat") concat)
    (primitive ("lengt") lengt)
    )
  )

;Data types built automatically:

(sllgen:make-define-datatypes scanner-lexical-specification grammar-syntatic-specification)

;Test
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-lexical-specification grammar-syntatic-specification)))
(show-the-datatypes)
;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)


(define scan&parse
  (sllgen:make-string-parser scanner-lexical-specification grammar-syntatic-specification))
;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-lexical-specification grammar-syntatic-specification))

(scan&parse "-[55]")
(scan&parse "-[5;1]")
(scan&parse "\"ghf\"")

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (eval-program  pgm))
                        (sllgen:make-stream-parser 
                         scanner-lexical-specification
                         grammar-syntatic-specification)))

;*******************************************************************************************
;eval-program: <program> -> number
; function that evaluates a program taking into account a given environment (it is initialized within the program)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body)))))


;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;Nota: En este caso primitive-exp necesita 2 parametros, peros siempre al menos un operando
(define eval-expression
  (lambda (exp)
    (cases expression exp
      (number-lit (datum) datum)
      (text-lit (characters) characters)
      (primitive-exp (prim exp rands)
                     (if (null? rands)
                         (apply-primitive prim (list (eval-rand exp)))
                         (apply-primitive prim (cons (eval-rand exp) (eval-rands rands)))
                         )
                     )
      )
    )
  )

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands)
    (map (lambda (x) (eval-rand x)) rands)))

(define eval-rand
  (lambda (rand)
    (eval-expression rand)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (if (null? (cdr args))
        (cases primitive prim
          (sum () (car args))
          (substrac () (car args))
          (mult () (car args))
          (div() (car args))
          (concat() (car args))
          (lengt()  (length (string->list (caar args))))
          )
        (cases primitive prim
          (sum () (+ (car args) (apply-primitive prim (cdr args))))
          (substrac () (- (car args) (apply-primitive prim (cdr args))))
          (mult () (* (car args) (apply-primitive prim (cdr args))))
          (div() (/ (car args) (apply-primitive prim (cdr args))))
          (concat () (cons (car args) (apply-primitive prim (cdr args))))
          (lengt ()  (length (string->list (car args))))
          )))
  )

