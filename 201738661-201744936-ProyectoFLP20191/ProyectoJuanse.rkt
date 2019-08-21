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

;;<exp-batch>:= (a-batch) (expression)*


;;<expression> := (lit-number) <number>
;;             := (lit-id) <text>
;;             := (lit-text) "<text>"
;;             := (true-val) true
;;             := (false-val) false
;;             := (empty-val) nil
;;             := (expHex) <expressionHex>
;;             := (expOct) <expressionOct>
;;             := (set-dec-exp) $ <letters> <assign-op> <expression> ;
;;             := (unary-expression) <unary-operation> <expression> ;
;;             := (condicional-exp) if <expression> then <exp-batch>
;;                                  (elsif <expression> then <exp-batch>)*
;;                                   else <exp-batch> end
;;             := (print-expression) puts  <expression> ;
;;             := (print-expression) puts  (<expression> ,)* <expresion> ;
;;             := (primitive-exp) (<expression> <binary-op> <expression>)
;;             := (expPrimitiveString) [<expression> <primitiveString>]
;;             := (for-exp) for <text> in <number> .. <number> <exp-batch> end
;;             := (proc-exp) def <text> (<text>) <exp-batch> end
;;             := (proc-exp) def <text> ( (<text> ,)* <text>) <exp-batch> end
;;             := (evalProc-exp) { <text> (<expression>) }
;;             := (evalProc-exp) { <text> ( (<expression> ,)* <expression>) }
;;             := (binary8) prim8 <expressionOct> <binaryOct> <expressionOct>;
;;             := (binary16) prim16 <expressionHex> <binaryHex> <expressionHex>;

;; <expressionHex> := (hex-number) (hex number)
;;                 := (hex-number) (hex (number ,)* number)
;; <expressionOct> := (oct-number) (oct number)
;;                 := (oct-number) (oct (number ,)* number)

;;<binary-op> := (sum) +
;;            := (subd) -
;;            := (mult) *
;;            := (div) /
;;            := (mod-op) %
;;            := (pow) **
;;            := (higher) >
;;            := (higher-eq) >=
;;            := (less) <
;;            := (less-eq) <=
;;            := (equal) ==
;;            := (different) !=
;;            := (and-op) and
;;            := (and-op) &&
;;            := (or-op) or
;;            := (or-op) ||
;;            := (in-range) ..

;;<binaryOct>  := (sum8) +8+
;;                := (rest8) -8-
;;                := (mult8) *8*
;;                := (plusOne8) ++8++
;;                := (restOne8) --8--

;;<binaryHex>  := (sum16) +16+
;;                := (rest16) -16-
;;                := (mult16) *16*
;;                := (plusOne16) ++16++
;;                := (restOne16) --16--

;;<primitiveString>  := (sizeString) .length

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
    
    (exp-batch ((arbno expression)) a-batch)
    
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
    (expression ("if" expression "then" exp-batch
                      (arbno "elsif" expression "then" exp-batch)
                      "else" exp-batch "end") condicional-exp)
    (expression ("puts" (separated-list expression ",") ";") print-expression)
    (expression ("(" expression binary-op expression ")") primitive-exp)
    (expression ("[" expression primitiveString "]") expPrimitiveString)
    (expression ("for" text "in" number ".." number exp-batch "end") for-exp)
    (expression ("def" text "(" (separated-list text ",") ")" exp-batch "end") proc-exp)
    (expression ("{" text "(" (separated-list expression ",") ")" "}") evalProc-exp) ; revision
    (expression ("bin8" expressionOct binaryOct expressionOct ";") binary8)
    (expression ("un8" expressionOct unaryOct ";") unary8) ;nuevo
    (expression ("bin16" expressionHex binaryHex expressionHex ";") binary16)
    (expression ("un16" expressionHex unaryHex ";") unary16) ;nuevo
    
    (expressionHex ("(hex"  (separated-list number "," )  ")") hex-number )
    (expressionOct ( "(oct" (separated-list number ",") ")") oct-number )
    
    (binary-op ("+") sum)
    (binary-op ("-") subd)
    (binary-op ("*") mult)
    (binary-op ("/") div)
    (binary-op ("%") mod-op)
    (binary-op ("**") pow)
    (binary-op (">") higher)
    (binary-op (">=") higher-eq)
    (binary-op ("<") less)
    (binary-op ("<=") less-eq)
    (binary-op ("==") equal)
    (binary-op ("!=") different)
    (binary-op ("and") and-op)
    (binary-op ("&&") and-op)
    (binary-op ("or") or-op)
    (binary-op ("||") or-op)
    (binary-op ("..") in-range)

    (binaryOct ("+8+") sum8); revision
    (binaryOct ("-8-") rest8); revision
    (binaryOct ("*8*") mult8); revision
    
    (unaryOct ("++8++") plusOne8); revision
    (unaryOct ("--8--") restOne8); revision

    (binaryHex ("+16+") sum16); revision
    (binaryHex ("-16-") rest16); revision
    (binaryHex ("*16*") mult16); revision
    
    (unaryHex ("++16++") plusOne16); revision
    (unaryHex ("--16--") restOne16); revision

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
    (a-batch (exps) 
      (if (null? exps) (eopl:pretty-print '=>nil) (aux-print2 exps env))
    )
  )
)

(define aux-print
 (lambda (toPrint)
   (map (lambda (x) (eopl:pretty-print x)) toPrint)
   )
  )

(define aux-print2
 (lambda (toPrint env)
   (cond [(null? toPrint) "Succesfully executed"]
         [#true (eopl:pretty-print (eval-expression (car toPrint) env))
                (aux-print2 (cdr toPrint) env)])
   )
  )

;eval-expression: <expresion> <ambiente>-> number || string || cerradura
; Purpose: Evaluate the expression using cases to determine which datatype is,
; it is used in eval-program. 
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-number (number) number)
      (lit-id (id) (apply-env env (string->symbol id)))
      (lit-text (text) text)
      (true-val () #t)
      (false-val () #f)
      (empty-val () (eopl:pretty-print '=>nil))
      (expOct (octRepresentation) (evalOct octRepresentation))
      (expHex (hexRepresentation) (evalHex hexRepresentation))
      (set-dec-exp (id assign body)
                   (applyAssigns-primitive (listOfString->listOfSymbols (list id)) assign (list (eval-expression body env)) env))
      (unary-expression (unary-op body) unary-op)
      (condicional-exp (test-exp true-exp elseiftest elseIfTrue false-exp) test-exp)
      (print-expression (listExps)
                        (map 
                         (lambda (x) 
                           (eopl:pretty-print (eval-expression x env))
                           ) 
                         listExps)
                        )
      (primitive-exp (exp1 op exp2)
                     (let ((value1 (eval-expression exp1 env))
                           (value2 (eval-expression exp2 env))
                           )
                           (apply-primitive value1 value2 op))
                     )
      (expPrimitiveString (exp op) (applyString-primitive (eval-expression exp env) op))
      (for-exp (iterator numberRange1 numberRange2 body) iterator)
      (proc-exp (id args body) id)
      (evalProc-exp (id args) id)
      (binary8 (exp1 op exp2) (cons "oct" (reverse (applyOct-binary (reverse (cdr (evalOct exp1))) (reverse (cdr (evalOct exp2))) op))))
      (binary16 (exp1 op exp2) (cons "hex" (reverse (applyHex-binary (reverse (cdr (evalHex exp1))) (reverse (cdr (evalHex exp2))) op))))
      (unary8 (exp1 op) (cons "oct" (reverse (applyOct-unary (reverse (cdr (evalOct exp1))) op))))
      (unary16 (exp1 op) (cons "hex" (reverse (applyHex-unary (reverse (cdr (evalHex exp1))) op))))
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
    (eval-expression rand env)))

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
  (lambda (exp1 exp2 prim)
        (cases binary-op prim
          (sum () 
               (if (string? exp1)
                   (string-append exp1 exp2)
                   (+ exp1 exp2 )
                   )
               )
          (subd () (- exp1 exp2))
          (mult () (* exp1 exp2))
          (div () (/ exp1 exp2))
          (mod-op () (modulo exp1 exp2))
          (pow ()  (expt exp1 exp2))
          (higher ()  (> exp1 exp2))
          (higher-eq () (>= exp1 exp2))
          (less ()  (< exp1 exp2))
          (less-eq () (<= exp1 exp2))
          (equal ()  (eq? exp1 exp2))
          (different () (not (eq? exp1 exp2)))
          (and-op ()  (and exp1 exp2))
          (or-op ()  (or exp1 exp2))
          (in-range () (if (< exp1 exp2) (getInterval exp1 exp2) (reverse (getInterval exp2 exp1))))
          )
    )
  )

;Constructs an interval due to its limits
(define getInterval
  (lambda (value1 value2)
    (cond [(= value1 value2) (list value2)]
          [else (cons value1 (getInterval (+ value1 1) value2))])
    )
  )

;;Evaluate primitives for strings 
(define applyString-primitive
  (lambda (exp1 prim)
    (cases primitiveString prim
      (sizeString () (string-length exp1))
      )
    )
  )

;;Evaluate unary primitives for Hex
(define applyHex-unary
  (lambda (exp1 prim)
    (cases unaryHex prim
      (plusOne16 () (plusOne exp1 16))
      (restOne16 () (restOne exp1 16))
      )
    )
  )

;;Evaluate binary primitives for Hex
(define applyHex-binary
  (lambda (exp1 exp2 prim)
    (cases binaryHex prim
      (sum16 () (sumBase exp1 exp2 16))
      (rest16 () (restBase exp1 exp2 16))
      (mult16 () (multBase exp1 exp2 16))
      )
    )
  )

;;Evaluate unary primitives for Hex
(define applyOct-unary
  (lambda (exp1 prim)
    (cases unaryOct prim
      (plusOne8 () (plusOne exp1 8))
      (restOne8 () (restOne exp1 8))
      )
    )
  )

;;Evaluate binary primitives for Octs 
(define applyOct-binary
  (lambda (exp1 exp2 prim)
    (cases binaryOct prim
      (sum8 () (sumBase exp1 exp2 8))
      (rest8 () (restBase exp1 exp2 8))
      (mult8 () (multBase exp1 exp2 8))
      )
    )
  )

(define restOne (lambda (n base)
                  (cond [(null? n) '()]
                        [(and (= 0 (car n)) (null? (cdr n))) '()]
                        [(and (null? (cdr n))(= 1 (car n))) '()]
                        [(and (< (car n) base) (> (car n) 0 ))
                         (cons (- (car n) 1) (cdr n))]
                        [else (cons (- base 1) (restOne (cdr n) base))])
                  )
  )

(define plusOne (lambda (n base)
                  (cond [(null? n) (list 1)]
                        [(and (< (car n) (- base 1) ) (>= (car n) 0 ))
                         (cons (+ 1 (car n)) (cdr n))]
                        [else (cons 0 (plusOne (cdr n) base))])
                  )
  )

(define sumBase
  (lambda (x y base)
    (if (null? x)
        y
        (plusOne (sumBase (restOne x base) y base) base))
    )
  )

(define restBase
  (lambda (x y base)
    (if (null? y)
        x
        (restOne (restBase  x (restOne y base) base) base))
    )
  )

(define multBase
  (lambda (x y base)
    (if (is-zero? x)
        (zero)
        (sumBase (multBase (restOne x base) y base) y base))
    )
  )

(define is-zero? (lambda (n) (null? n)))

(define zero (lambda () '()))

;/bin16(hex 10,11,12)*16*(hex 2,11,3);/
;;Evaluate primitives of assigns 
(define applyAssigns-primitive
  (lambda (id assign body env)
    (cases assign-op assign
      (declarative-opp ()(extend-env-recursively id body env) )
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
        (eval-expression body (extend-env ids args env))
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
