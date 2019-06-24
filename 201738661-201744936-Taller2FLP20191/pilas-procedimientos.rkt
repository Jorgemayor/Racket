#lang eopl

;Encabezado

;----------------------------------------------
;PROCEDIMIENTOS
;----------------------------------------------

(define popP
  (lambda (stack)
    (stack 1)))

(define topP
  (lambda (stack)
    (stack 2)))

(define empty-stackP?
  (lambda (stack)
    (stack 3)))

(define empty-stackP
  (lambda ()
    (lambda (id)
      (cond
        [(eqv? id 1)'()]
        [(eqv? id 2) "The stack is null"]
        [(eqv? id 3) #t]))))


(define pushP
  (lambda (element stack)
    (lambda (id)
      (cond
        [(eqv? id 1) stack]
        [(eqv? id 2) element]
        [(eqv? id 3) (empty-stackP id)]))))

(define stackPushP
  (pushP 'a
         (pushP 'b
                (pushP 'c
                       (pushP 'd
                              (empty-stackP))))))

;Pruebas
(empty-stackP? (empty-stackP)); Debe retornar true
(popP (empty-stackP)); Debe retonar '()
(topP (empty-stackP)); Debe retornar "The stack is null"
(topP stackPushP); Debe retornar 'a
(topP (pushP 'x stackPushP)); Debe retornar 'x
(topP (popP stackPushP)); Debe retornar 'b
