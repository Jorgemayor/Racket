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
;;<program> := (a-program) < / exp-batch />

;;<exp-batch>:= (a-batch) <expression> (expression)*


;;<expression> := (lit-number) <number>
;;             := (expHex) <expressionHex>
;;             := (expOct) <expressionOct>
;;             := (lit-id) <text>
;;             := (lit-text) "<text>"
;;             := (true-val) true
;;             := (false-val) false
;;             := (empty-val) nil
;;             := (set-dec-exp) $ <letters> <assign-op> <expression> ;
;;             := (unary-expression) <unary-operation> <expression> ;
;;             := (condicional-exp) if <expression> then* <exp-batch>
;;                                  (elsif <expression> then* <exp-batch>)*
;;                                   else <exp-batch> end
;;             := (print-expression) puts  (<expression> ,)* ;
;;             := (primitive-exp) (<expression> <binary-op> <expression> ( <binary-op> <expression>)* )
;;             := (expPrimitiveString) [ <expression> <primitiveString> <expression>* ]
;;             := (for-exp) for <text> in <number> .. <number> <exp-batch> end
;;             := (proc-exp) def <text> ( (<text> ,)* ) <exp-batch> end
;;             := (evalProc-exp) {  <text> ( (<expression> ,)* ) }
;;             := (primitive8) prim8 <expressionOct> <primitiveOct> <expressionOct> <expressionOct>* ;
;;             := (primitive16) prim16 <expressionHex> <primitiveHex> <expressionHex> <expressionHex>* ;

;; <expressionHex> := (hex-number) (hex (number ,)* )
;; <expressionOct> := (oct-number) (oct (number ,)* )

;;<binary-op> := (sum) +
;;            := (subd) -
;;            := (mult) *
;;            := (div) /
;;            := (mod) %
;;            := (pow) **
;;            := (higher) >
;;            := (higher-eq) >=
;;            := (less) <
;;            := (less-eq) <=
;;            := (equal) ==
;;            := (different) !=
;;            := (and) and
;;            := (and) &&
;;            := (or) or
;;            := (or) ||
;;            := (in-range) ..

;;<primitiveOct>  := (sum8) +8+
;;                := (rest8) -8-
;;                := (mult8) *8*
;;                := (plusOne8) ++8++
;;                := (restOne8) --8--

;;<primitiveHex>  := (sum16) +16+
;;                := (rest16) -16-
;;                := (mult16) *16*
;;                := (plusOne16) ++16++
;;                := (restOne16) --16--

;;<primitiveString>  := (sizeString) .length
;;                   := (concatString) +

;;<assign-op>  := (declarative-opp) =
;;             := (add-eq) +=
;;             := (diff-eq) -=
;;             := (mult-eq) *=
;;             := (div-eq) /=
;;             := (pow-eq) **=

;;<unary-operation>  := (not-op) not
;;                   := (not-op) !


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
    (program ( "/" exp-batch  "/") a-program)
    
    (exp-batch (expression (arbno expression)) a-batch)
    
    (expression (number) lit-number)
    (expression (expressionHex) expHex)
    (expression (expressionOct) expOct)
    ;(expression (expressionString) stringExpression) ; revision
    (expression (text) lit-id)
    (expression ("\"" text "\"") lit-text)
    (expression ("true") true-val)
    (expression ("false") false-val)
    (expression ("nil") empty-val)
    (expression ("$" text assign-op expression ";") set-dec-exp)
    (expression (unary-operation expression ";") unary-expression)
    (expression ("if"   expression  (arbno "then") exp-batch
                        (arbno "elsif" expression (arbno "then") exp-batch)
                        "else" exp-batch "end") condicional-exp)
    (expression ("puts" (separated-list expression ",") ";") print-expression)
    (expression ("(" expression binary-op expression (arbno binary-op expression) ")") primitive-exp)
    (expression ("[" expression primitiveString "]") expPrimitiveString) ; Revisión
    (expression ("for" text "in" number ".." number exp-batch "end") for-exp)
    ;(expression ("return" expression ";") return-exp) ; revision
    (expression ("def" text "(" (separated-list text ",") ")" exp-batch "end") proc-exp)
    (expression ("{" text "(" (separated-list expression ",") ")" "}") evalProc-exp) ; revision
    (expression ("prim8" expressionOct primitiveOct expressionOct (arbno primitiveOct expressionOct)";") primitive8) ; revision
    (expression ("prim16" expressionHex primitiveHex expressionHex  (arbno primitiveHex expressionHex)";") primitive16); revision
    
    (expressionHex ("(hex"  (separated-list number "," )  ")") hex-number )
    (expressionOct ( "(oct" (separated-list number ",") ")") oct-number )
    
    
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

    (primitiveOct ("+8+") sum8); revision
    (primitiveOct ("-8-") rest8); revision
    (primitiveOct ("*8*") mult8); revision
    (primitiveOct ("++8++") plusOne8); revision
    (primitiveOct ("--8--") restOne8); revision

    (primitiveHex ("+16+") sum16); revision
    (primitiveHex ("-16-") rest16); revision
    (primitiveHex ("*16*") mult16); revision
    (primitiveHex ("++16++") plusOne16); revision
    (primitiveHex ("--16--") restOne16); revision

    (primitiveString (".length") sizeString) ; revision

    (assign-op ("=") declarative-opp)
    (assign-op ("+=") add-eq)
    (assign-op ("-=") diff-eq)
    (assign-op ("*=") mult-eq)
    (assign-op ("/=") div-eq)
    (assign-op ("**=") pow-eq)

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
                        (lambda (pgm) (eval-program pgm))
                        (sllgen:make-stream-parser 
                         scanner-lexical-specification
                         grammar-syntatic-specification)))

;********************************************************************************)(***********

;*******************************************************************************************
;eval-program: <programa> -> number |string | symbol
; Purpose: function that evaluates a program 

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-batch body (init-env))))))

; Initial Enviroment
(define init-env
  (lambda ()
     (empty-env)))

;valor-verdad? determines whether a given value corresponds to a false or true Boolean value
(define true-value?
  (lambda (x)
    #t))


;eval-exp-batch:
(define (eval-batch batch env)
  (cases exp-batch batch
    (a-batch (e exps) 
      (if (null? exps) empty-val (eval-expressions e exps env))
    )
  )
)

;eval-expression: <expresion> <ambiente>-> number || string || cerradura
; Purpose: Evaluate the expression using cases to determine which datatype is,
; it is used in eval-program. 
(define eval-expressions
  (lambda (exp env)
    (cases expression exp
      (lit-number (number) number) ;revisar el batch
      (lit-id (id) (apply-env env (string->symbol id))) ;revisar el batch
      (lit-text (text) text)  ;revisar el batch
      (true-val () #t)  ;revisar el batch
      (false-val () #f)  ;revisar el batch
      (expOct (octRepresentation) (evalOct octRepresentation))
      (expHex (hexRepresentation) (evalHex hexRepresentation))
      (empty-val () (eopl:pretty-print '=>nil))
      (set-dec-exp (id assign body)
                   (applyAssigns-primitive (listOfString->listOfSymbols (list id)) assign (list (eval-expressions body empty env)) env))
      (unary-expression (unary-op body) unary-op)
      (condicional-exp (test-exp true-exp elseiftest elseIfTrue false-exp) test-exp)
      (print-expression (listExps)
                        (map 
                         (lambda (x) 
                           (eopl:pretty-print (eval-expressions x empty env))
                           ) 
                         listExps
                         )
                        ) ;Puts funciona bien con el batch
      (primitive-exp (exp1 op exp2 op2 rands)
                     (let ((value1 (eval-expressions exp1 empty env))
                           (value2 (eval-expressions exp2 empty env))
                           )
                       (if (null? rands)
                           (apply-primitive value1 value2 op rands)
                           (apply-primitive value1 value2 op (map (lambda (x) (eval-expressions x env)) rands)))
                       )
                     )
      (expPrimitiveString (exp op) (length exp))
      (for-exp (iterator numberRange1 numberRange2 body) iterator)
      (proc-exp (id args body) id)
      (evalProc-exp (id args) id)
      (primitive8 (exp1 op exp2 ops rands) rands)
      (primitive16 (exp1 op exp2 ops rands) rands)
      )
    )
  )

(define eval-exps
  (lambda (exps env)
    (cond 
      [(null? exps) "Process finished with exit code 0"]
      [else (eval-expressions (car exps) (cdr exps) env)]
    )
  )
)

(define evalHex
  (lambda (sintHex)
    (cases expressionHex sintHex
      (hex-number (list-numbers) (cons 'hex list-numbers)))))

(define evalOct
  (lambda (sintOct)
    (cases expressionOct sintOct
      (oct-number (list-numbers) (cons 'oct list-numbers)))))



; auxiliary functions to apply eval-expression to each element of a
; list of operands (expressions)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expressions rand env)))

;Auxiliary functions to convert lists of strings to lists of symbols
(define listOfString->listOfSymbols
  (lambda (ids)
    (cond [(null? ids) empty]
          [else (cons (string->symbol (car ids)) (listOfString->listOfSymbols (cdr ids)))])))

(define listOfListString->listOfListSymbols
  (lambda (ids)
    (cond [(null? ids) empty]
          [else (cons (listOfString->listOfSymbols (car ids)) (listOfListString->listOfListSymbols (cdr ids)))])))

;apply-primitive: <primitiva> <list-of-expression> -> number || string
;Purpose: Operates the list of expression(at least one expression acording to grammar)
; depending on what primitive is, which is identified with cases.
; This procedure is used in  eval-expression.

(define apply-primitive
  (lambda (exp1 exp2 prim args)
    (if (null? empty)
        (cases binary-op prim
          (sum () (+ exp1 exp2))
          (subd () (- (car args) (apply-primitive prim (cdr args))))
          (mult () (* (car args) (apply-primitive prim (cdr args))))
          (div () (/ (car args) (apply-primitive prim (cdr args))))
          (mod () (string-append (car args) (apply-primitive prim (cdr args))))
          (pow ()  (string-length (car args)))
          (higher ()  (string-length (car args)))
          (higher-eq ()  (string-length (car args)))
          (less ()  (string-length (car args)))
          (less-eq ()  (string-length (car args)))
          (equal ()  (string-length (car args)))
          (different ()  (string-length (car args)))
          (and ()  (string-length (car args)))
          (or ()  (string-length (car args)))
          (in-range ()  (string-length (car args)))
          
          )
        0
        
        )
    )
  )
;;Evaluate primitives for strings 
(define applyString-primitive
  (lambda (exp1 prim args)
    (if (null? (args))
        (cases primitiveString prim
          (sizeString () (+ (car args) (apply-primitive prim (cdr args))))
          )
        0
        )
    )
  )
;;Evaluate primitives for Octs 
(define applyOcts-primitive
  (lambda (exp1 exp2 prim args)
    (if (null? (args))
        (cases primitiveOct prim
          (sum8 () (+ (car args) (apply-primitive prim (cdr args))))
          (rest8 () (- (car args) (apply-primitive prim (cdr args))))
          (mult8 () (- (car args) (apply-primitive prim (cdr args))))
          (plusOne8 () (- (car args) (apply-primitive prim (cdr args))))
          (restOne8 () (- (car args) (apply-primitive prim (cdr args))))
          )
        0
        )
    )
  )

;;Evaluate primitives for Hex
(define applyHex-primitive
  (lambda (exp1 exp2 prim args)
    (if (null? (args))
        (cases primitiveHex prim
          (sum16 () (+ (car args) (apply-primitive prim (cdr args))))
          (rest16 () (- (car args) (apply-primitive prim (cdr args))))
          (mult16 () (- (car args) (apply-primitive prim (cdr args))))
          (plusOne16 () (- (car args) (apply-primitive prim (cdr args))))
          (restOne16 () (- (car args) (apply-primitive prim (cdr args))))
          )
        0
        )
    )
  )

;;Evaluate primitives of assigns 
(define applyAssigns-primitive
  (lambda (id assign body env)
    (cases assign-op assign
      (declarative-opp ()(extend-env id body env) )
      (add-eq ()0 )
      (diff-eq ()0 )
      (mult-eq ()0 )
      (div-eq ()0 )
      (pow-eq ()0 )
      )
    )
  )

;*******************************************************************************************
;*******************************************************************************************
;Referencias
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;*******************************************************************************************
;Datatypes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec  vector?)
   (env environment?)))

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body exp-batch?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args env)
    (cases procval proc
      (closure (ids body env)
        (eval-expressions body (extend-env ids args env))
      )
    )
  )
)

(define scheme-value? (lambda (v) #t))

;*******************************************************************************************
;Ambientes

(define empty-env  
  (lambda ()
    (empty-env-record)))       



(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;Ambiente recursivo para un solo procedimiento
(define (a-recursive-env a-proc-name ids body env)
  (let ((vec (make-vector 1)))
    (let ((env (extended-env-record (list a-proc-name) vec env)))
      (vector-set! vec 0 (closure ids body env))
      env
    )
  )
)

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'Error "undefined local variable or method ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;apply-set-ref: asigna un valor a un id
(define apply-set-ref
  (lambda (id value env)
    (setref! (apply-env-ref env id) value)))

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))


; Auxiliary functions

; auxiliary functions to find the position of a symbol
; in the list of symbols of an environment

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

;*******************************************************************************************

(interpreter)
