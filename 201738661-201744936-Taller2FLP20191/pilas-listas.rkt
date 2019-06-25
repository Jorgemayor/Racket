#lang eopl

; Taller 2 Fundamentos de lenguaje de programacion
; 
; pilas-listas.rkt
; 
; Desarrolladores:
; 
; Jorge Eduardo Mayor Fernandez
; Codigo: 201738661
; 
; Juan Sebastian Velasquez Acevedo
; Codigo: 201744936

;----------------------------------------------
;GRAMATIC
;----------------------------------------------

; <stack> ::= (empty-stack)
;         ::=(push <scheme-value> <stack>)

;----------------------------------------------
;LISTS
;----------------------------------------------

;empty-stack: {void}
;             -> {list}
;Purpose:
;Definicion de pila vacía
(define empty-stack
  (lambda () (list 'empty-stack)))
;Prueba:
(empty-stack)

;Definicion stack Generico
(define stack1 (list 3 3 3 4 52 1 'empty-stack))
(push 'x stack1)

;push: element {any}
;      stack {list}
;      -> {list}
;Purpose:
;Inserts an element at the begginning of the given stack
(define push
  (lambda (element stack)
    (cond
      [(empty-stack? stack) (cons element (empty-stack))]
      [else (cons element stack)])))

;Definiciones de stacks por medio del constructor push (Pruebas)

(define stackPush1
  (push 'a
        (push 'b
              (push 'c
                    (push 'd
                          (empty-stack))))))

(define stackPush2
  (push 1
        (push 2
              (push 3
                    (empty-stack)))))


;pop: stack {list}
;      -> {list}
;Purpose:
;Remove an element at the begginning of the stack
(define pop
  (lambda (stack)
    (cond
      [(empty-stack? stack) '()]
      [else (cdr stack)])))

;Pruebas:
(pop stackPush1)
(pop stackPush1)

;top: stack {list}
;      -> {any}
;Purpose:
;Shows the element at the top of the given stack, without removing it.
(define top
  (lambda (stack)
    (cond
      [(empty-stack? stack) "The stack is null"]
      [else (car stack)])))

;Pruebas:
(top (empty-stack))
(top stackPush1)
(top stackPush2)


;top: stack {list}
;      -> {boolean}
;Purpose:
;Evaluates if the stack is empty
;Returns true if it is, false otherwise.
(define empty-stack?
  (lambda (stack)
    (if (eqv? (car stack) 'empty-stack)
        #t #f))) 

;Pruebas
(empty-stack? stack1)
(empty-stack? (empty-stack))

