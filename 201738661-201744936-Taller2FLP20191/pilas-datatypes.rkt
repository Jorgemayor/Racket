#lang eopl

; Taller 2 Fundamentos de lenguaje de programacion
; 
; pilas-datatypes.rkt
; 
; Desarrolladores:
; 
; Jorge Eduardo Mayor Fernandez
; Codigo: 201738661
; 
; Juan Sebastian Velasquez Acevedo
; Codigo: 201744936


;-------------------------------------------------------------------------------
;GRAMATIC 
;------------------------------------------------------------------------------

;;  <stack>     ::= empty-stack
;;              ::= (push <scheme-value> <stack>)

;----------------------------------------------
;DATATYPES
;----------------------------------------------
;;empty-stackP: stack {procedure}
;;      -> '() || String || boolean
;;Purpose:Define stack data type by Datatype with the constructors
; empty-stackType and pushType
(define-datatype stackType stackType?
  (empty-stackType)
  (pushType (element scheme-value?)
            (stackBody stackType?))
  )

;Determine what is a scheme value
(define scheme-value? (lambda (v) #t))

;;topP: topType 
;;      -> String || element
;;Purpose: Returns the top element of the stack without removing it with cases
(define topType
  (lambda (stack)
    (cases stackType stack
      (empty-stackType ()
                       "The stack is null")
      (pushType (element stackBody)
                element)
      )))

; ;popP: popType
; ;      -> stackType
; ;Purpose:Remove the top element from the stack with cases
(define popType
  (lambda (stack)
    (cases stackType stack
      (empty-stackType ()
                       (stack))
      (pushType (element stackBody)
                stackBody)
      )))

;; empty-stackType?: stack 
;;      -> boolean
;;Purpose:Determines if a stack is empty with cases
(define empty-stackType?
  (lambda (stack)
    (cases stackType stack
      (empty-stackType ()
                       #t)
      (pushType (element stackBody)
                #f)
      )))

;Definicion de stacks por medio de Datatypes, para algunas pruebas
(define stackType1
  (pushType 2
            (pushType 'y
                      (pushType 3
                                (pushType 'w
                                          (empty-stackType))))))
(define stackType2
  (pushType 'x
            (empty-stackType)))

;Pruebas
(popType stackType1); Debe retornar #(struct:pushType y #(struct:pushType 3 #(struct:pushType w #(struct:empty-stackType))))
(+ 2 (topType stackType1)); Debe retornar 4
(topType (popType stackType1)); Debe retornar 'y
(topType (popType stackType2)); Debe retornar "The stack is null"
(empty-stackType? (popType stackType2));Debe retornar #t
(empty-stackType? stackType1); Debe retornar #f


;UNPARSE Y PARSE

;unparseStack: stack {stackType}
;             -> {list}
;Purpose:
;Takes a stackType and unparses it into a list, analyzing
;each case of the gramatic.
(define unparseStack
  (lambda (stack)
    (cases stackType stack
      (empty-stackType ()
                       '(emptyStackList))
      (pushType (element stackBody)
                (list 'pushList element (unparseStack stackBody)))
      )
    )
  )


;parseStack: stack {list}
;           -> {stackType}
;Purpose:
;Takes a list and parses it into an abstract syntax tree,
;defined by stackType; analyzing if the given list fixes with
;the definition of the datatype.
(define parseStack
  (lambda (dato)
    (if (pair? dato)
        (cond
          [(eqv? (car dato) 'emptyStackList) (empty-stackType)]
          [(eqv? (car dato) 'pushList)(pushType
                                       (dato->elemento dato)
                                       (parseStack (caddr dato)))]
          [else 'Invalido])
        'Invalido)))

(define dato->elemento
  (lambda (list)
    (cadr list)))


(define stackPrueba (list 'pushList 2 '(emptyStackList)))

;Pruebas
(unparseStack stackType1); Debe retornar (pushList 2 (pushList y (pushList 3 (pushList w (emptyStackList)))))
(unparseStack stackType2); Debe retornar (pushList x (emptyStackList))
(parseStack(unparseStack stackType2));Debe retornar #(struct:pushType x #(struct:empty-stackType)) 
(parseStack stackPrueba); Debe retornar #(struct:pushType 2 #(struct:empty-stackType))
(parseStack stackType1); Debe retornar Invalido
