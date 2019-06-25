#lang eopl

; Taller 2 Fundamentos de lenguaje de programacion
; 
; pilas-procedimientos.rkt
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

;;  <stack>     ::= (empty-stack)
;;              ::= (push <scheme-value> <stack>)

;----------------------------------------------
;PROCEDIMIENTOS
;----------------------------------------------

;;empty-stackP: stack {procedure}
;;      -> '() || String || boolean
;;Purpose:Define an empty stack and in turn is a procedure that receives
;an id sent and returned depending on the procedure that deposits the id
(define empty-stackP
  (lambda ()
    (lambda (id)
      (cond
        [(eqv? id 1)'()]
        [(eqv? id 2) "The stack is null"]
        [(eqv? id 3) #t]))))

;; empty-stackP?: stack {procedure}
;;      -> procedure
;;Purpose:Determines if a stack is empty,
;; calling stack with id 3, which is a procedure associated with pushP or empty-stackP
(define empty-stackP?
  (lambda (stack)
    (stack 3)))



; ;popP: stack {procedure}
; ;      -> procedure
; ;Purpose:Remove the top element from the stack
;; calling stack with id 1, which is a procedure associated with pushP or empty-stackP
(define popP
  (lambda (stack)
    (stack 1)))

;;topP: stack {procedure}
;;      -> procedure
;;Purpose: Returns the top element of the stack without removing it
;; calling stack with id 2, which is a procedure associated with pushP or empty-stackP
(define topP
  (lambda (stack)
    (stack 2)))



;;pushP: element{schemeValue} stack {procedure}
;;      -> stack {procedure}||element {schemeValue} || call to empty stack {procedure} 
;;Purpose:Insert an element in the stack and also manage the id's given by topP,
;;popP and emptyList?
(define pushP
  (lambda (element stack)
    (lambda (id)
      (cond
        [(eqv? id 1) stack]
        [(eqv? id 2) element]
        [(eqv? id 3) (empty-stackP id)]))))

;Test stack built with pushP
(define stackPushP
  (pushP 'a
         (pushP 'b
                (pushP 'c
                       (pushP 'd
                              (empty-stackP))))))


;Pruebas empty-stackP? / empty-stackP
(empty-stackP? (empty-stackP)); Debe retornar true

;Pruebas topP and popP
(topP (empty-stackP)); Debe retornar "The stack is null"
(topP stackPushP); Debe retornar 'a
(topP (pushP 'x stackPushP)); Debe retornar 'x
(topP (popP stackPushP)); Debe retornar 'b


;Pruebas popP
(popP (empty-stackP)); Debe retonar '()
(popP stackPushP); Debe retorna un procedimiento

