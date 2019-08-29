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
;;<program> := (a-program) / <exp-batch> /

;;<exp-batch>:= (a-batch) (expression)*

;;<expression> := (lit-number) <number>
;;             := (lit-id) <text>
;;             := (lit-text) "<text>"
;;             := (true-val) true
;;             := (false-val) false
;;             := (empty-val) nil
;;             := (set-dec-exp) $ <letters> <assign-op> <expression> ;
;;             := (unary-expression) <unary-op> <expression> ;
;;             := (condicional-exp) if <expression> then { <exp-batch> }
;;                                  (elsif <expression> then { <exp-batch> })*
;;                                   else { <exp-batch> } end
;;             := (print-expression) puts  <expression> {, <expression>}*;
;;             := (primitive-exp) (<expression> <binary-op> <expression>)
;;             := (expPrimitiveString) [<expression> <primitiveString>]
;;             := (for-exp) for <text> in <expression> .. <expression> { <exp-batch> } end
;;             := (proc-exp) def <text> () { <exp-batch> } end
;;             := (proc-exp) def <text> (<text> {, <text>}*)  { <exp-batch> } end
;;             := (evalProc-exp) { <expression> () }
;;             := (evalProc-exp) { <expression> (<expression> {, <expression>}* ) }
;;             := (binary8) bin8 <expression> <binaryOct> <expression>;
;;             := (unary8)un8 <expression> <unaryOct> ;
;;             := (binary16) prim16 <expression> <binaryHex> <expression>;
;;             := (unary16) un16 <expression> <unaryHex> ;
;;             := (hex-number) (hex number {, number}*)
;;             := (oct-number) (oct number {, number}*)

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
;;             := (rest8) -8-
;;             := (mult8) *8*
;;             := (plusOne8) ++8++
;;             := (restOne8) --8--

;;<binaryHex>  := (sum16) +16+
;;             := (rest16) -16-
;;             := (mult16) *16*
;;             := (plusOne16) ++16++
;;             := (restOne16) --16--

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

(let ((time-stamp "Time-Version: 2019-08-28 16:18:14 dfried>"))
  (eopl:printf " INTERPRETER 2 WITH CHECK OF TYPES ~a~%"
    (substring time-stamp 13 30)))

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
                      "else" "{" exp-batch "}" "end") condicional-exp)
    (expression ("puts" (separated-list expression ",") ";") print-expression)
    (expression ("(" expression binary-op expression ")") primitive-exp)
    (expression ("[" expression primitiveString "]") expPrimitiveString)
    (expression ("for" text "in" expression ".." expression "{"exp-batch"}" "end") for-exp)
    (expression ("def" text "(" (separated-list text ",") ")" "{"exp-batch"}" "end") proc-exp)
    (expression ("{" expression "(" (separated-list expression ",") ")" "}") evalProc-exp)
    (expression ("bin8" expression binaryOct expression ";") binary8)
    (expression ("un8" expression unaryOct ";") unary8)
    (expression ("bin16" expression binaryHex expression ";") binary16)
    (expression ("un16" expression unaryHex ";") unary16)
    
    (expression ("(hex"  (separated-list number "," )  ")") hex-number )
    (expression ( "(oct" (separated-list number ",") ")") oct-number )
    
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
                 (eval-batch body (init-env)))
      )
    )
  )

; Initial Enviroment
(define init-env
  (lambda ()
    (empty-env)))

;true-value?
;determines whether a given value corresponds to a false or true Boolean value
(define true-value? (lambda (x) #t))

;eval-batch
;A batch is set of espressions that are evaluated each one of them and their
;values are stored in a list, that is returned to eval-program.
;This function takes every expression of the batch given and makes the corresponding
;procedure with them.
(define (eval-batch batch env)
  (cases exp-batch batch
    (a-batch (exps)
             (cond
               [(null? exps) exps]
               [else (cases expression (car exps)
                       (set-dec-exp (id assign body) (applyAssigns-primitive (listOfString->listOfSymbols (list id))
                                                                             assign
                                                                             (list (eval-expression body env empty))
                                                                             env exps))
                       (proc-exp (id args body) 
                                 (let ((extended-env (extend-env-recursively (list(string->symbol id))
                                                                             (listOfString->listOfSymbols args)
                                                                             body
                                                                             env)))
                                   (eval-batch (a-batch(cdr exps)) extended-env)
                                   ))
                       (else
                        (cons 
                         (eval-expression (car exps) env (cdr exps))
                         (eval-batch(a-batch(cdr exps)) env)
                         )
                        )
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
      (set-dec-exp (id assign body) empty)
      (unary-expression (unary-op body) (apply-unary-exp unary-op (eval-expression body env empty)))
      (condicional-exp (test-exp true-exp elseiftest elseIfTrue false-exp)
                       (if (eval-expression test-exp env empty)
                           (last-of-a-list(eval-batch true-exp env ))
                           (eval-condition elseiftest elseIfTrue false-exp env)
                           )                                                                                                                    
                       )
      (print-expression (listExps)
                        (last-of-a-list(eval-batch(a-batch listExps) env))); falta revisar los voids
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
                        (env-local (extend-env (listOfString->listOfSymbols (list iterator)) (list empty) env)))              
                 (map
                  (lambda (value)
                    (begin
                      (apply-set-refFor (string->symbol iterator) value env-local)
                      (last-of-a-list(eval-batch body env-local))
                      )
                    )
                  list-iterator)
                 )
               )
      (proc-exp (id args body) empty)
      (evalProc-exp (id args) 
                    (let ([name (eval-expression id env exps)]
                          [args (eval-rands args env exps)])
                      (if (procval? name)
                          (apply-procedure name args env)
                          ("Attemp to apply non-procedure ~s" name))
                      )
                    )
                                     
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
    (cond [(null? elseiftest) (last-of-a-list(eval-batch false-exp env))]
          [(eval-expression (car elseiftest) env empty)( last-of-a-list(eval-batch (car elseIfTrue) env))]
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
          (declarative-opp () (let ((env-for-batch
                                     (extend-env
                                      id
                                      body
                                      env)))
                                (eval-batch (a-batch exps) env-for-batch))
                           )
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
;References
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

;Environment datatype
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

;apply-procedure: evaluates the body of a producedure on the correspnding extended environment
(define apply-procedure
  (lambda (proc args env)
    (cases procval proc
      (closure (ids body env)
               (last-of-a-list(eval-batch body (extend-env ids args env )))
               )
      )
    )
  )

;;Evaluates the rest of the expressions
(define apply-procedureExps
  (lambda (id body exps env)
    (eval-expression (car exps) (extend-env id body env) (cdr exps))
    )
  )


(define scheme-value? (lambda (v) #t))

;*******************************************************************************************
;Environments

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
;Creates an entended environment for recursive procedures
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
;Returns a list from 1 to end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end)
          '()
          (cons next (loop (+ 1 next))))
      )
    )
  )

;Function that searches a symbol on the environment
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

;apply-set-ref: asign a value to an id
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
;auxiliary function that returns the last of a list
(define last-of-a-list
  (lambda (list)
    (cond
      [(null? list) empty]
      [else  (car (reverse list))])))


(interpreter)


;Tests

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

;Aplicado con listas, recursion y suma  
;/
;def sum (a)
;{
;if (a==8) then
;{
;8
;}
;else
;{
;{sum ((a+ 1))}
;}
;end
;}
;end
;(1+{sum (6)})
;/

; Factorial 
;/
;def fact (a)
;{
;if ( (a==0) || (a==1)) then
;{
;1
;}
;else
;{
;(a * {fact ((a- 1))} )
;}
;end
;}
;end
;{fact (5)}
;/


;Fibonacci
;/
;def fibo(i){
;if((i==1) or (i==0)) then
;{
;1
;}
;else
;{
;({fibo((i- 1))} + {fibo((i- 2))})
;}
;end
;}
;end
;for i in 1 .. 5{
;{fibo(i)}
;}
;end
;/
