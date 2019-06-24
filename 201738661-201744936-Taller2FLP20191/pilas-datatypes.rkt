;Encabezado

;----------------------------------------------
;DATATYPES
;----------------------------------------------
;Defino tipo de dato stack por Datatype con los constructores
;empty-stackType y pushType
(define-datatype stackType stackType?
  (empty-stackType)
  (pushType (element scheme-value?)
            (stackBody stackType?))
  )

(define scheme-value? (lambda (v) #t))

;Funciones de observadores, en este caso son extractores
(define topType
  (lambda (stack)
    (cases stackType stack
      (empty-stackType ()
                       "The stack is null")
      (pushType (element stackBody)
                element)
      )))

(define popType
  (lambda (stack)
    (cases stackType stack
      (empty-stackType ()
                       (stack))
      (pushType (element stackBody)
                stackBody)
      )))

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
