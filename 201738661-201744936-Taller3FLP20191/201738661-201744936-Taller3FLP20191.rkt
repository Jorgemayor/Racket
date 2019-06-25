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
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit "." (arbno digit)) number)
  (number
   ("-" digit "." (arbno digit)) number)
  (text
   ("" string (arbno digit) "" ) string)))

;Syntactic specification (grammar)

(define grammar-syntatic-specification
  '((program (expression) a-program)
    (expression (number) number-lit)
    (expression (text) text-lit)
    (expression
     (primitive "[" expression (arbno expression) ";" "]")
     primitive-exp)
    (primitive ("+") sum)
    (primitive ("-") substrac)
    (primitive ("*") mult)
    (primitive ("/") div)
    (primitive ("concat") concat)
    (primitive ("length") length)
    ))

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
                 (eval-expression body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;Nota: En este caso primitive-exp necesita 2 parametros, peros siempre al menos un operando
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (number-lit (datum) datum)
      (text-lit (characters) characters)
      (primitive-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args))))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

; Llama a eval-expression con el ambiente actual para determinar los valores de las variables
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (if (null? (cdr args))
        (car args)
    (cases primitive prim
      (sum () (+ (car args) (apply-primitive prim (cdr args))))
      (substrac () (- (car args) (apply-primitive prim (cdr args))))
      (mult () (* (car args) (apply-primitive prim (cdr args))))
      (div() (/ (car args) (apply-primitive prim (cdr args))))
      (concat() (cons (car args) (apply-primitive prim (cdr args))))
      (length()  (length (string->list (car args))))
      )))
  )

(define push-final
  (lambda (element stack)
    (cond
      [(empty-stack? stack) (cons element (empty-stack))]
      [else (cons (car stack) (push-final element (cdr stack)))])))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))


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

(string->list "123 123")

;******************************************************************************************

(show-the-datatypes)
just-scan
scan&parse

