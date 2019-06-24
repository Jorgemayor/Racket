;Encabezado

;----------------------------------------------
;LISTAS
;----------------------------------------------
;Definicion pila vacía
(define empty-stack
  (lambda () (list 'empty-stack)))


;Push=>Insertar elemento en una pila

;Insertarlo al inicio
(define push
  (lambda (element stack)
    (cond
      [(empty-stack? stack) (cons element (empty-stack))]
      [else (cons element stack)])))

;Pop=>Retira el elemento superior de la pila
(define pop
  (lambda (stack)
    (cond
      [(empty-stack? stack) '()]
      [else (cdr stack)])))

;Top=>Devuelve el elemento superior de la pila sin retirarlo
(define top
  (lambda (stack)
    (cond
      [(empty-stack? stack) "The stack is null"]
      [else (car stack)])))

;Observador empty-satck? Predicado que se encarga de preguntar
;si la pila está vacía 
(define empty-stack?
  (lambda (stack)
    (if (eqv? (car stack) 'empty-stack)
        #t #f))) 


;Definicion stack1 Generico
(define stack1 (list 3 3 3 4 52 1 'empty-stack))
(push 'x stack1)

;Definicion stackPush por medio del constructor push
(define stackPush
  (push 'a
        (push 'b
              (push 'c
                    (push 'd
                          (empty-stack))))))

;Pruebas
(pop stackPush); Debe retornar (b c d empty-stack)
(top stackPush); Debe retornar 'a
(empty-stack? stack1); Debe retorna #f
(top (empty-stack)); Debe retornar "The stack is Null"
