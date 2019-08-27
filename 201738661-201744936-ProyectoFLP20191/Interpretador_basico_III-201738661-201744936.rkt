#lang eopl
; Proyecto Fundamentos de lenguaje de programacion
; 
; 201738661-201744936 - Interpretador Basico I
; 
; Developers:
; 
; Jorge Eduardo Mayor Fernandez
; Code: 201738661
; 
; Juan Sebastian Velasquez Acevedo
; Code: 201744936
;
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
;;             := (unary-expression) <unary-op> <expression> ;
;;             := (condicional-exp) if <expression> then { <exp-batch> }
;;                                  (elsif <expression> then { <exp-batch> })*
;;                                   else { <exp-batch> } end
;;             := (print-expression) puts  <expression> {, <expression>}*;
;;             := (primitive-exp) (<expression> <binary-op> <expression>)
;;             := (expPrimitiveString) [<expression> <primitiveString>]
;;             := (for-exp) for <text> in <expression> .. <expression> { <exp-batch> } end
;;             := (proc-exp) def <text> (<text>) {, <text>}* { <exp-batch> } end
;;             := (evalProc-exp) { <text> (<expression>) {, expression}* }
;;             := (binary8) prim8 <expressionOct> <binaryOct> <expressionOct>;
;; (expression := (unary8)un8 <expressionOct> <unaryOct> ;
;;             := (binary16) prim16 <expressionHex> <binaryHex> <expressionHex>;
;;             := (unary16) un16 <expressionHex> <unaryHex> ;

;; <expressionHex> := (hex-number) (hex number {, number}*)
;; <expressionOct> := (oct-number) (oct number {, number}*)

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

;;<unary-op>  := (not-op) not
;;            := (not-op) !


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
    (program ( "/" (arbno class-decl) exp-batch  "/") a-program)
    (exp-batch ((arbno expression)) a-batch)
    
    (expression (number) lit-number)
    ;(expression (expressionHex) expHex); change gramatic
    ;(expression (expressionOct) expOct); change gramatic
    (expression (text) lit-id)
    (expression ("\"" text "\"") lit-text)
    (expression ("true") true-val)
    (expression ("false") false-val)
    (expression ("nil") empty-val)
    (expression ("$" text assign-op expression ";") set-dec-exp)
    (expression (unary-op expression ";") unary-expression)
    (expression ("if" expression "then" "{" exp-batch "}"
                      (arbno "elsif" expression "then" "{" exp-batch "}")
                      "else" "{" exp-batch "}" "end") condicional-exp) ;change in gramatic
    (expression ("puts" (separated-list expression ",") ";") print-expression)
    (expression ("(" expression binary-op expression ")") primitive-exp)
    (expression ("[" expression primitiveString "]") expPrimitiveString)
    (expression ("for" text "in" expression ".." expression "{"exp-batch"}" "end") for-exp) ;change in gramatic
    (expression ("def" text "(" (separated-list text ",") ")" "{"exp-batch"}" "end") proc-exp)
    (expression ("{" expression "(" (separated-list expression ",") ")" "}") evalProc-exp) ; revision
    (expression ("bin8" expression binaryOct expression ";") binary8) ; change gramatic
    (expression ("un8" expression unaryOct ";") unary8) ; change gramatic
    (expression ("bin16" expression binaryHex expression ";") binary16) ; change gramatic
    (expression ("un16" expression unaryHex ";") unary16)  ; change gramatic
    
    (expression ("(hex"  (separated-list number "," )  ")") hex-number ) ; change gramatic
    (expression ( "(oct" (separated-list number ",") ")") oct-number ) ; change gramatic
    
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

    (binaryOct ("+8+") sum8)
    (binaryOct ("-8-") rest8)
    (binaryOct ("*8*") mult8)
    
    (unaryOct ("++8++") plusOne8)
    (unaryOct ("--8--") restOne8)

    (binaryHex ("+16+") sum16)
    (binaryHex ("-16-") rest16)
    (binaryHex ("*16*") mult16)
    
    (unaryHex ("++16++") plusOne16)
    (unaryHex ("--16--") restOne16)

    (primitiveString (".length") sizeString) 

    (assign-op ("=") declarative-opp)
    (assign-op ("+=") add-eq)
    (assign-op ("-=") diff-eq)
    (assign-op ("*=") mult-eq)
    (assign-op ("/=") div-eq)
    (assign-op ("**=") pow-eq)

    (unary-op ("not") not-op)
    (unary-op ("!") not-op)

;-----------Classes and Objects----------------
    (class-decl ("class" text "<" text (arbno "@" text) (arbno method-decl) "end") a-class)
    (method-decl ("def" text "(" (separated-list text ",") ")" "{" exp-batch "}" "end") a-method)
    (expression ("Class" text ".new(" (separated-list expression ",") ")") new-obj)
    (expression ("do" text "." text "(" (separated-list expression ",") ")") app-method)
    (expression ("super" text "(" (separated-list expression ",") ")") super-exp)
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
      (a-program (classes body)
                 (elaborate-class-decls! classes)
                 the-class-env)
      )
    )
  )

; Initial Enviroment
(define init-env
  (lambda ()
    (empty-env)))

;valor-verdad? determines whether a given value corresponds to a false or true Boolean value
(define true-value? (lambda (x) #t))

;eval-exp-batch:
(define (eval-batch batch env)
  (cases exp-batch batch
    (a-batch (exps)
             (cond
               [(null? exps) "Exit with code 0"]
               [else (cases expression (car exps)
                       (set-dec-exp (id assign body) (applyAssigns-primitive (listOfString->listOfSymbols (list id))
                                                                             assign
                                                                             (list (eval-expression body env empty))
                                                                             env exps))
                       (proc-exp (id args body) (eval-expression (car exps)
                                                                 (extend-env-recursively (list(string->symbol id))
                                                                                         (listOfString->listOfSymbols args)
                                                                                         body
                                                                                         env)
                                                                 (cdr exps)))
                       (condicional-exp (test-exp true-exp elseiftest elseIfTrue false-exp) (if (eval-expression test-exp env empty)
                                                                                                (eval-batch true-exp env )
                                                                                                (eval-condition elseiftest elseIfTrue false-exp env)
                                                                                                ))
                       (evalProc-exp (id args) 
                                     (let ([name (eval-expression id env exps)]
                                           [args (eval-rands args env exps)])
                                       (if (procval? name)
                                           (apply-procedure name args env)
                                           ("Attemp to apply non-procedure ~s" name))
                                       )
                                     )
                       (else (aux-print exps env))
                       )]
               )
             )
    )
  )

(define aux-print
  (lambda (toPrint env)
    (eopl:pretty-print (eval-expression (car toPrint) env (cdr toPrint)))
    (eval-batch (a-batch (cdr toPrint)) env)
    )
  )

;eval-expression: <expresion> <ambiente>-> number || string || cerradura
; Purpose: Evaluate the expression using cases to determine which datatype is,
; it is used in eval-program. 
(define eval-expression
  (lambda (exp env exps)
    (cases expression exp
      (lit-number (number) number)
      (lit-id (id) (apply-env env (string->symbol id)))
      (lit-text (text) text)
      (true-val () #t)
      (false-val () #f)
      (empty-val () (eopl:pretty-print '=>nil))
      (hex-number (hexRepresentation) (cons 'hex hexRepresentation) )
      (oct-number (octRepresentation) (cons 'oct octRepresentation))
      (set-dec-exp (id assign body) (eval-batch (a-batch exps) env))
      (unary-expression (unary-op body) (apply-unary-exp unary-op (eval-expression body env empty)))
      (condicional-exp (test-exp true-exp elseiftest elseIfTrue false-exp) (eval-batch (a-batch exps) env))
      (print-expression (listExps)
                        (for-each 
                         (lambda (x) 
                           (eopl:pretty-print(eval-expression x env empty))) listExps)); falta revisar los voids
      (primitive-exp (exp1 op exp2)
                     (let ((value1 (eval-expression exp1 env empty))
                           (value2 (eval-expression exp2 env empty))
                           )
                       (apply-binary-exp value1 value2 op))
                     )
      (expPrimitiveString (exp op) (applyString-primitive (eval-expression exp env empty) op))
      (for-exp (iterator exp-ran-1 exp-ran-2 body)
               (letrec ((numberRange1 (eval-expression exp-ran-1 env exps))
                        (numberRange2 (eval-expression exp-ran-2 env exps))
                     (list-iterator
                      (if (< numberRange1 numberRange2) 
                          (getInterval numberRange1 numberRange2) 
                          (reverse (getInterval numberRange2 numberRange1))))
                     (env-let (extend-env (listOfString->listOfSymbols (list iterator)) (list empty) env)))
                 (for-each
                  (lambda (value)
                    (begin
                      (apply-set-refFor (string->symbol iterator) value env-let)
                      (eval-batch body env-let)
                      )
                    )
                  list-iterator)
                 )
               )
      (proc-exp (id args body) (eval-batch (a-batch exps) env))
      (evalProc-exp (id args) (eval-batch (a-batch exps) env))
      (binary8 (exp1 op exp2) (cons "oct" (reverse (applyOct-binary (reverse (cdr (eval-expression exp1 env exps))) (reverse (cdr (eval-expression exp2 env exps))) op))))
      (binary16 (exp1 op exp2) (cons "hex" (reverse (applyHex-binary (reverse (cdr (eval-expression exp1 env exps))) (reverse (cdr (eval-expression exp2 env exps))) op))))
      (unary8 (exp1 op) (cons "oct" (reverse (applyOct-unary (reverse (cdr (eval-expression exp1 env exps))) op))))
      (unary16 (exp1 op) (cons "hex" (reverse (applyHex-unary (reverse (cdr (eval-expression exp1 env exps))) op))))
      
      (new-obj (idClass exps) idClass)
      (app-method (idObj idClass exps) idObj)
      (super-exp (idObj exps) idObj)
      )
    )
  )

(define print-list
  (lambda (list)
    (cond
      [(not(null? list)) (eopl:pretty-print (car list))
                         (print-list (cdr list))])
    )
  )





;Auxiliary functions to convert lists of strings to lists of symbols
(define listOfString->listOfSymbols
  (lambda (ids)
    (cond [(null? ids) empty]
          [else (cons (string->symbol (car ids)) (listOfString->listOfSymbols (cdr ids)))])
    )
  )

(define listOfListString->listOfListSymbols
  (lambda (ids)
    (cond [(null? ids) empty]
          [else (cons (listOfString->listOfSymbols (car ids)) (listOfListString->listOfListSymbols (cdr ids)))])
    )
  )

(define eval-condition
  (lambda (elseiftest elseIfTrue false-exp env)
    (cond [(null? elseiftest) (eval-batch false-exp env)]
          [(eval-expression (car elseiftest) env empty) (eval-batch (car elseIfTrue) env)]
          [#true (eval-condition (cdr elseiftest) (cdr elseIfTrue) false-exp env)])
    )
  )

;Constructs an interval due to its limits
(define getInterval
  (lambda (value1 value2)
    (cond [(= value1 value2) (list value2)]
          [else (cons value1 (getInterval (+ value1 1) value2))])
    )
  )

; auxiliary functions to apply eval-expression to each element of a
; list of operands (expressions)
(define eval-rands
  (lambda (rands env exps)
    (map (lambda (x) (eval-expression x env exps)) rands)
    )
  )

;apply-binary-exp: <primitiva> <list-of-expression> -> number || string
;Purpose: Operates the list of expression(at least one expression acording to grammar)
; depending on what primitive is, which is identified with cases.
; This procedure is used in  eval-expression.

(define apply-binary-exp
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

(define apply-unary-exp
  (lambda (prim exp)
    (cases unary-op prim
      (not-op () (not exp)))
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

(define restOne
  (lambda (n base)
    (cond [(null? n) '()]
          [(and (= 0 (car n)) (null? (cdr n))) '()]
          [(and (null? (cdr n))(= 1 (car n))) '()]
          [(and (< (car n) base) (> (car n) 0 ))
           (cons (- (car n) 1) (cdr n))]
          [else (cons (- base 1) (restOne (cdr n) base))])
    )
  )

(define plusOne
  (lambda (n base)
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

;;Evaluate primitives of assigns 
(define applyAssigns-primitive
  (lambda (id assign body env exps)
    (if (boolean? (apply-env env (car id)))
        (cases assign-op assign
          (declarative-opp () (eval-expression (car exps) (extend-env id body env) (cdr exps)))
          (add-eq () (eopl:error 'Assign "undefined local variable or method '~s'" (car id)))
          (diff-eq () (eopl:error 'Assign "undefined local variable or method '~s'" (car id)))
          (mult-eq () (eopl:error 'Assign "undefined local variable or method '~s'" (car id)))
          (div-eq () (eopl:error 'Assign "undefined local variable or method '~s'" (car id)))
          (pow-eq () (eopl:error 'Assign "undefined local variable or method '~s'" (car id))))
        
        (cases assign-op assign
          (declarative-opp () (apply-set-ref (car id) (car body) env exps))
          (add-eq () (apply-set-ref (car id) (+ (apply-env env (car id)) (car body)) env exps))
          (diff-eq () (apply-set-ref (car id) (- (apply-env env (car id)) (car body)) env exps))
          (mult-eq () (apply-set-ref (car id) (* (apply-env env (car id)) (car body)) env exps))
          (div-eq () (apply-set-ref (car id) (/ (apply-env env (car id)) (car body)) env exps))
          (pow-eq () (apply-set-ref (car id) (expt (apply-env env (car id)) (car body)) env exps))
          )
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
    (primitive-deref ref)
    )
  )

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))
    )
  )

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)
    )
  )

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))
    )
  )

;*******************************************************************************************
;Datatypes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec  vector?)
   (env environment?)
   )
  (recursively-extended-env-record (proc-name (list-of symbol?))
                                   (ids (list-of symbol?))
                                   (body exp-batch?)
                                   (env environment?))
  )

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
               (eval-batch body (extend-env ids args env ))
               )
      )
    )
  )
;; Evalua el cuerpo de expresiones siguientes

(define apply-procedureExps
  (lambda (id body exps env)
    (eval-expression (car exps) (extend-env id body env) (cdr exps))
    )
  )


(define scheme-value? (lambda (v) #t))

;*******************************************************************************************
;Ambientes

(define empty-env  
  (lambda ()
    (empty-env-record)
    )
  )

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)
    )
  )

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define (extend-env-recursively a-proc-name ids body env)
  (let ((vec (make-vector 1)))
    (let ((env (recursively-extended-env-record a-proc-name ids body env)))
      (vector-set! vec 0 (closure ids body env))
      env
      )
    )
  )

(define a-recursive-env
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec pos (closure ids body env)))
           (iota len) idss bodies)
          env)
        )
      )
    )
  )

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end)
          '()
          (cons next (loop (+ 1 next))))
      )
    )
  )

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (let ([result-ref (apply-env-ref env sym)])
      (if (reference? result-ref)
          (deref result-ref)
          result-ref)
      )
    )
  )

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record () #f)
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ([pos (list-find-position sym proc-names)])
                                         (if (number? pos)
                                             (closure idss
                                                      bodies
                                                      env)
                                             (apply-env-ref old-env sym))
                                         )
                                       )
      )
    )
  )

;apply-set-ref: asigna un valor a un id
(define apply-set-ref
  (lambda (id value env exps)
    (let ([result-ref (apply-env-ref env id)])
      (if (reference? result-ref)
          (begin
            (setref! result-ref value)
            (eval-batch (a-batch (cdr exps)) env))
          result-ref)
      )
    )
  )

(define apply-set-refFor
  (lambda (id value env )
    (let ([result-ref (apply-env-ref env id)])
      (if (reference? result-ref)
          (setref! result-ref value)
          result-ref)
      )
    )
  )

; Auxiliary functions

; auxiliary functions to find the position of a symbol
; in the list of symbols of an environment

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)
    )
  )

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f)))
      )
    )
  )

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))


;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;; evaluar
(define aux
   (lambda (x)
     x))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))))))

;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))
      
;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (classes)
    (set! the-class-env classes)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

;;

(interpreter)


;PRUEBAS

;Prueba definiciones alcance de variables
;/
;$b=3;
;
;def sum (a)
;{
;if (a==8) then
;{
;$b=0; b
;}
;else
;{
;b
;}
;end
;}
;end
;{sum (8)}
;
;/

;Prueba definiciones simple
; /
;def sum (a)
;{
;if (a==8) then
;{
;"Es8"
;}
;else
;{
;{sum ((a+ 1))}
;}
;end
;}
;end
;{sum (6)}
;/

;PRUEBA FOR
;/
;for i in 1..6 {
;puts 2;
;}
;end
;/

;PRUEBA DEL IF
; /
;$a=1;
;if((a+2)==3) then {
;puts a;
;$a+=5;
;}
;elsif ((a- 3) <= 0)
;then {
;puts "menorQUe0";
;puts "EstasENElsif";
;}
;elsif (a>100) then
;{
;puts "mayorque100";
;puts "secondelsif";
;}
;else
;{
;puts "else";
;}
;end
;a
;/