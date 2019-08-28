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

(let ((time-stamp "Time-Version: 2019-08-28 16:18:14 dfried>"))
  (eopl:printf " INTERPRETER 2 WITH CHECK OF TYPES ~a~%"
    (substring time-stamp 13 30)))
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
    (expression ("for" text "in" expression ".." expression "{"exp-batch"}" "end") for-exp) ;change in gramatic and new
    (expression ("proc" type-exp "def" text "(" (separated-list type-exp text ",") ")" "{"exp-batch"}" "end") proc-exp) ;new
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

 ;--------------------Types---------------------
    (type-exp ("int") int-type-exp) ;new
    (type-exp ("bool") bool-type-exp) ;new
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")")
              proc-type-exp) ;new
    (type-exp ("oct") oct-type-exp)
    (type-exp ("hex") hex-type-exp)
    (type-exp ("string") string-type-exp)
    (type-exp ("empty") empty-type-exp)
    
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



;interpreter-types + checker (FrontEnd + evaluation + signal for lecture )

(define interpreter-types
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (aux-interpreter  pgm)) 
    (sllgen:make-stream-parser 
      scanner-lexical-specification
      grammar-syntatic-specification)))

(define aux-interpreter
  (lambda (x)
    (type-of-program x)
   ;(if (all-true? (map verify-types (type-of-program x))) (eval-program  x) 'error)
    )
  )

; Function that helps put together a list of Booleans to verify types
(define verify-types
  (lambda (list-types)
    (type? list-types)))

;auxiliary that verifies if the list consists only of true values
(define all-true?
  (lambda (list-of-boolean)
    (cond [(null? list-of-boolean) #t]
          [else (and (car list-of-boolean) (all-true? (cdr list-of-boolean)))])))

;********************************************************************************)(***********

;*******************************************************************************************
;eval-program: <programa> -> number |string | symbol
; Purpose: function that evaluates a program 

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-batch body (init-env)))
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
                       (proc-exp (type-proc id types args body) (eval-expression (car exps)
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
      (proc-exp (type-proc id types args body) (eval-batch (a-batch exps) env))
      (evalProc-exp (id args) (eval-batch (a-batch exps) env))
      (binary8 (exp1 op exp2) (cons "oct" (reverse (applyOct-binary (reverse (cdr (eval-expression exp1 env exps))) (reverse (cdr (eval-expression exp2 env exps))) op))))
      (binary16 (exp1 op exp2) (cons "hex" (reverse (applyHex-binary (reverse (cdr (eval-expression exp1 env exps))) (reverse (cdr (eval-expression exp2 env exps))) op))))
      (unary8 (exp1 op) (cons "oct" (reverse (applyOct-unary (reverse (cdr (eval-expression exp1 env exps))) op))))
      (unary16 (exp1 op) (cons "hex" (reverse (applyHex-unary (reverse (cdr (eval-expression exp1 env exps))) op))))
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
           (if (and
                (string? exp1)
                (string? exp2))
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
      (not-op () (not exp))
    )
  ))


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


;***********************************************************************************************************************
;******************************************************  TypeS *****************************************************
;***********************************************************************************************************************
;***********************************************************************************************************************
;********************************************  Ambientes de tipos  *****************************************************
;***********************************************************************************************************************

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))

;***********************************************************************************************************************
;***********************************************************************************************************************

;***********************************************************************************************************************
;*************************************************   Type Checker     **************************************************
;***********************************************************************************************************************

;type-of-program: <programa> -> type
; función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body) (eval-batch-types body (empty-tenv))))))


;***********************************************************************************************************************
;*************************************************  Datatypes for types    **************************************************
;***********************************************************************************************************************

(define-datatype type type?
  (atomic-type
   (name symbol?))
  (proc-type
   (arg-types (list-of type?))
   (result-type type?)))

(define int-type (atomic-type 'int))
(define bool-type (atomic-type 'bool))
(define oct-type (atomic-type 'oct))
(define hex-type (atomic-type 'hex))
(define string-type (atomic-type 'string))
(define empty-type (atomic-type 'empty))



(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (proc-type-exp (arg-texps resut-texp)
                     (proc-type 
                                (expand-type-expressions arg-texps)
                                (expand-type-expression resut-texp)))
      (oct-type-exp () oct-type)
      (hex-type-exp () hex-type)
      (string-type-exp () string-type)
      (empty-type-exp () empty-type)
      )))

(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))

;***********************************************************************************************************************
;*************************************************  Evaluations for types    **************************************************
;***********************************************************************************************************************

(define (eval-batch-types batch env)
  (cases exp-batch batch
    (a-batch (exps)
             (cond [(null? exps) empty]
                   [else
                    (cases expression (car exps)
                
                      (set-dec-exp (id assign body) 
                                   (type-of-statement id body (cdr exps) env))

                      (proc-exp (type-proc id texps args body)
                                (type-of-recursive-procedure type-proc id texps args body env (cdr exps)))
                      

                
                      (else
                       (cons 
                        (type-of-expression (car exps) env (cdr exps))
                        (eval-batch-types(a-batch(cdr exps)) env)
                        )
                       )
                      )
                    ]
                   )
             )
    )
  )
(define type-of-expression
  (lambda (exp tenv exps)
    (cases expression exp
      (lit-text (text) string-type)
      (lit-number (number) int-type)
      (true-val () bool-type)
      (false-val () bool-type)
      (lit-id (id) (apply-tenv tenv (string->symbol id)))
      (empty-val () empty-type)
      (oct-number (octRepresentation) oct-type)
      (hex-number (hexRepresentation) hex-type)
      
      (condicional-exp (test-exp true-exp elseiftest elseIfTrue false-exp)
                       (letrec
                           ((test-type (type-of-expression test-exp tenv exps))
                            (false-type (eval-batch-types false-exp tenv))
                            (true-type (eval-batch-types true-exp tenv))
                            (types-elsif-else (eval-conditionTypes elseiftest elseIfTrue false-exp tenv exps))
                            (list-of-last-types
                             (if (type? types-elsif-else)
                                 (list (last-of-a-list true-type) types-elsif-else)
                                 (cons (last-of-a-list true-type) types-elsif-else))
                            ))
                         (check-equal-type! test-type bool-type test-exp)
                         (all-true?(recursive-equalType list-of-last-types exp))
                         (car true-type)
                        )
                       )
      
      (proc-exp (type-proc id texps args body) empty) ; it's evaluated in the eval-batch-types for ease

      
      (set-dec-exp (id assign body)  empty); it's evaluated in the eval-batch-types for ease
      
      (unary-expression (unary-op body)
                        (type-of-application
                         (apply-unary-expType unary-op)
                         (types-of-expressions (list body) tenv exps)
                         unary-op
                         (list body)
                         exp
                         ) 
                        )
      
      (print-expression (listExps) (last-of-a-list(eval-batch-types(a-batch listExps) tenv)))
      
      (primitive-exp (exp1 op exp2)
                     (type-of-application
                      (type-of-Binaryprimitive op
                                               (type-of-expression exp1 tenv exps)
                                               (type-of-expression exp2 tenv exps))
                      (types-of-expressions (list exp1 exp2) tenv exps)
                      op
                      (list exp1 exp2)
                      exp
                      )
                     )
      
      (expPrimitiveString (expString op)
                          (type-of-application
                           (applyString-primitiveType op)
                           (types-of-expressions (list expString) tenv exps)
                           op
                           (list expString)
                           exp
                           )
                          )
      (for-exp (iterator exp-ran-1 exp-ran-2 body)
               (cond
                 [(and (equal? (type-of-expression exp-ran-1 tenv exps) int-type) (equal?(type-of-expression exp-ran-2 tenv exps) int-type))
                  (letrec ((extended-env
                            (extend-tenv
                             (list (string->symbol iterator))
                             (list int-type)
                             tenv)))
                    (last-of-a-list (eval-batch-types body extended-env)))
                  ]
                 [else (eopl:error 'check-int-type!
                                   "Types didn’t match: ~s != int or ~s != int in~%~s"
                                   (type-to-external-form (type-of-expression exp-ran-1 tenv exps))
                                   (type-to-external-form (type-of-expression exp-ran-2 tenv exps))
                                   exp)]
                 )
               )
      
      (evalProc-exp (id args)
                    (type-of-expression id tenv exps)
;                    (type-of-application
;                     (type-of-expression id tenv exps)
;                     (types-of-expressions args tenv exps)
;                     id
;                     args
;                     exp
;                     )
                    )
      
      (binary8 (exp1 op exp2)
               (type-of-application
                      (applyOct-binaryType op)
                      (types-of-expressions (list exp1 exp2) tenv exps)
                      op
                      (list exp1 exp2)
                      exp
                      )
               )
      
      (binary16 (exp1 op exp2)
                (type-of-application
                      (applyHex-binaryType op)
                      (types-of-expressions (list exp1 exp2) tenv exps)
                      op
                      (list exp1 exp2)
                      exp
                      )
                )
      
      (unary8 (exp1 op)
              (type-of-application
               (applyOct-unaryType op)
               (types-of-expressions (list exp1) tenv exps)
               op
               (list exp1)
               exp
               )
              )
      
      (unary16 (exp1 op)
               (type-of-application
                (applyHex-unaryType op)
                (types-of-expressions (list exp1) tenv exps)
                op
                (list exp1)
                exp
                )
               )
      )
    )
  )

;check-equal-type!: <type> <type> <expression> -> 
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Types didn’t match: ~s != ~s in~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp)
        #t)))


(define equal-type?
  (lambda (a listed exp)
    (cond
      [(null? listed) empty]
      [else
       (cons (check-equal-type! a (car listed) exp)
             (equal-type? a (cdr listed) exp))
       ])
    ))


(define recursive-equalType
  (lambda (L exp)
    (cond
      [(null? L) empty]
      [else
       (append (equal-type? (car L) (cdr L) exp)
               (recursive-equalType (cdr L) exp))
       ])
    ))


;type-to-external-form: <type> -> lista o simbolo
; recibe un tipo y devuelve una representación del tipo facil de leer
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type)))))))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))
   

;type-of-proc-exp: <lit-text> (list-of <type-exp>) (list-of <symbol>) <exp-batch> <tenv> <list-of-expressions> -> <list-of-Type-expressions>
; Auxiliary function to determine the type of a procedure creation expression and extends
; an environment with the procedure id and its type. For the subsequent evaluation of types in the batch.
;(define type-of-proc-exp
;  (lambda (id texps ids body tenv rest-of-expressions)
;    (let ((arg-types (expand-type-expressions texps)))
;      (letrec
;          ((result-type
;            (last-of-a-list
;             (eval-batch-types body
;                               (extend-tenv (listOfString->listOfSymbols ids) arg-types tenv))))
;           (tenv-for-batch
;            (extend-tenv
;             (list (string->symbol id))
;             (list (proc-type arg-types result-type) )
;             tenv)))
;        (eval-batch-types (a-batch rest-of-expressions) tenv-for-batch)
;        )
;      )
;    )
;  )

;type-of-recursive-procedure: <type-exp> <lit-text> (list-of <type-exp>) (list-of <symbol>) <exp-batch> <tenv> <list-of-expressions> -> <list-of-Type-expressions>
; Auxiliary function to determine the type of a recursive procedure  and extends
; an environment with the procedure id and its type. For the subsequent evaluation of types in the batch.
(define type-of-recursive-procedure
  (lambda (result-texp proc-name texps ids body tenv rest-of-expressions)
    (let ((arg-types (map (lambda (texp)
                             (expand-type-expression texp))
                           texps))
          (result-type (expand-type-expression result-texp)))
      (let ((the-proc-type
              (proc-type arg-types result-type)))
        (let ((tenv-for-body
               (extend-tenv (list (string->symbol proc-name)) (list the-proc-type) tenv)))
          (begin
            (check-equal-type!
             (last-of-a-list
              (eval-batch-types
              body
              (extend-tenv (listOfString->listOfSymbols ids) arg-types tenv-for-body)))
             result-type
             body)
            (eval-batch-types (a-batch rest-of-expressions) tenv-for-body)))))))

;type-of-application: <type> (list-of <type>) <symbol> (list-of <symbol>) <expresion> -> <type>
; función auxiliar para determinar el tipo de una expresión de aplicación de procedimientos
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each
                        check-equal-type!
                        rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "Wrong number of arguments in expression ~s:"
                                  "~%expected ~s~%got ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:~%~s~%had rator type ~s"
                   rator (type-to-external-form rator-type))))))





;types-of-expressions: (list-of <type-exp>) <tenv> -> (list-of <type>)
; función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv exps)
    (map (lambda (exp) (type-of-expression exp tenv exps)) rands)))

;type-of-primitive: <primitive> -> <type>
; función auxiliar para determinar el tipo de una primitiva
(define type-of-Binaryprimitive
  (lambda (prim  exp1Type exp2Type)
    (cases binary-op prim
      (sum () (if (and (equal? exp1Type string-type) (equal? exp2Type string-type))
                  (proc-type (list string-type string-type) string-type)
                  (proc-type (list int-type int-type) int-type))
           )
      (subd () (proc-type (list int-type int-type) int-type))
      (mult () (proc-type (list int-type int-type) int-type))
      (div () (proc-type (list int-type int-type) int-type))
      (mod-op () (proc-type (list int-type int-type) int-type))
      (pow ()  (proc-type (list int-type int-type) int-type))
      (higher ()  (proc-type (list int-type int-type) bool-type))
      (higher-eq () (proc-type (list int-type int-type) bool-type))
      (less ()  (proc-type (list int-type int-type) bool-type))
      (less-eq () (proc-type (list int-type int-type) bool-type))
      (equal ()  (proc-type (list int-type int-type) bool-type))
      (different () (proc-type (list int-type int-type) bool-type))
      (and-op ()  (proc-type (list bool-type bool-type) bool-type))
      (or-op ()  (proc-type (list bool-type bool-type) bool-type))
      (in-range () (proc-type (list int-type int-type) int-type))
      )))
  
(define type-of-Unaryprimitive
  (lambda (prim)
    (cases unary-op prim
      (not-op () (proc-type (list bool-type) bool-type)))))
  
  

  ;;Evaluate primitives for strings 
(define applyString-primitiveType
  (lambda (prim)
    (cases primitiveString prim
      (sizeString () (proc-type (list string-type) int-type))
      )
    )
  )

;;Evaluate unary primitives for Hex
(define applyHex-unaryType
  (lambda (prim)
    (cases unaryHex prim
      (plusOne16 () (proc-type (list hex-type) hex-type))
      (restOne16 () (proc-type (list hex-type) hex-type))
      )
    )
  )

;;Evaluate binary primitives for Hex
(define applyHex-binaryType
  (lambda ( prim)
    (cases binaryHex prim
      (sum16 () (proc-type (list hex-type hex-type) hex-type))
      (rest16 () (proc-type (list hex-type hex-type) hex-type))
      (mult16 () (proc-type (list hex-type hex-type) hex-type))
      )
    )
  )

;;Evaluate unary primitives for Hex
(define applyOct-unaryType
  (lambda ( prim)
    (cases unaryOct prim
      (plusOne8 () (proc-type (list oct-type) oct-type))
      (restOne8 () (proc-type (list oct-type) oct-type))
      )
    )
  )

;;Evaluate binary primitives for Octs 
(define applyOct-binaryType
  (lambda ( prim)
    (cases binaryOct prim
      (sum8 () (proc-type (list oct-type oct-type) oct-type))
      (rest8 () (proc-type (list oct-type oct-type) oct-type))
      (mult8 () (proc-type (list oct-type oct-type) oct-type))
      )
    )
  )
;Check types of the unary op
(define apply-unary-expType
  (lambda (prim)
    (cases unary-op prim
      (not-op () (proc-type (list bool-type) bool-type))
    )
  ))

;auxiliary function that returns the last of a list of each batch of the elsif, if any, and of the else.
;In order to compare the types of the last results.
(define eval-conditionTypes
  (lambda (elseiftest elseIfTrue false-exp tenv exps)
    (cond [(null? elseiftest) (list(last-of-a-list(eval-batch-types false-exp tenv)))]
          [(check-equal-type! (type-of-expression (car elseiftest) tenv exps) bool-type elseiftest)
           (append (list(last-of-a-list(eval-batch-types (car elseIfTrue) tenv))) (eval-conditionTypes (cdr elseiftest) (cdr elseIfTrue) false-exp tenv exps))])
    )
  )

;auxiliary function that returns the last of a list
(define last-of-a-list
  (lambda (list)
    (cond
      [(null? list) empty]
      [else  (car (reverse list))])))



;type-of-statement: <symbol> <expression> <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión let
(define type-of-statement
  (lambda (id rands rest-of-expressions tenv )
    (let ((tenv-for-batch
           (extend-tenv
            (list (string->symbol id))
            (list (type-of-expression rands tenv rest-of-expressions))
            tenv)))
      (eval-batch-types (a-batch rest-of-expressions) tenv-for-batch))))














(interpreter-types)


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
;--------------------------------------------------------------------------------
;;PRUEBAS TIPOS
;(arg-types-to-external-form(type-of-program (scan&parse "/
;if((1+4)==3) then {
;0
;}
;elsif ((1- 1) <= 0)
;then {
;1
;}
;elsif (1>100) then
;{
;2
;}
;else
;{
;3 
;}
;end
;
;/
;")))
;----------------------------------------------------------------------------------
;Pruebas de tipos para procedimientos recursivos
;/
;proc string def y (string a, int b, int x)
;{
;a
;
;
;}
;end
;
;proc int def x (int a, int b, int x)
;{
;a
;b
;{y ("hola",2,3)}
;}
;end
;
;
;/

;/
;proc string def y (string a, int b, int x)
;{
;a
;{y ("a",2,3)}
;a
;}
;end
;y
;/

;/
;proc string def p1 (string a, int b, int x)
;{
;a
;}
;end
;proc (string*int*int->string) def p2 (string a, int b, int x)
;{
;p1
;}
;end
;/
;-------------------------------------------------------------------------------