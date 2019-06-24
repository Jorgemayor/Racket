#lang eopl

;Encabezado

;-------------------------------------------------------------------------------
;LISTAS
;----------------------------------------------
;Definicion pila vacía

(define empty-tree
  (lambda ()
    (list 'empty-tree)
    )
  )

(define empty-tree?
  (lambda (tree)
    (eqv? (car tree) (car (empty-tree)))
    )
  )

(define make-tree
  (lambda (numero)
    (list numero (empty-tree) (empty-tree))
    )
  )

(define extract-node
  (lambda (tree)
    (car tree)
    )
  )

(define extract-izq
  (lambda (tree)
    (cadr tree)))

(define extract-der
  (lambda (tree)
    (caddr tree)))

(define leaf-tree?
  (lambda (tree)
    (or (empty-tree? tree)
        (and (empty-tree? (extract-izq tree))
             (empty-tree? (extract-der tree))))
    )
  )

(define node-tree?
  (lambda (tree)
    (not (and (empty-tree? (extract-izq tree))
              (empty-tree? (extract-der tree)))
         )
    )
  )

(define validador-orden
  (lambda (tree)
    (letrec ([treeToList (lambda (tree)
                           (cond [(empty-tree? tree) empty]
                                 [else (append (treeToList (extract-izq tree)) (list (extract-node tree)) (treeToList (extract-der tree)))])
                           )]
             [validar-orden (lambda (lista)
                              (cond [(or (null? lista)
                                         (null? (cdr lista))) #t]
                                    [else (and (<= (car lista) (cadr lista))
                                               (validar-orden (cdr lista)))])
                              )]
             [main (validar-orden (treeToList tree))])
      main)
    )
  )

(define insertar-elemento
  (lambda (tree numero)
    (cond [(empty-tree? tree) (make-tree numero)]
          [(= (extract-node tree) numero) tree]
          [(< (extract-node tree) numero) (cons (extract-node tree) (list (extract-izq tree) (insertar-elemento (extract-der tree) numero)))]
          [else (cons (extract-node tree) (list (insertar-elemento (extract-izq tree) numero) (extract-der tree)))]) 
    )
  )

(define arbol1 (list 5 (empty-tree)
                     (empty-tree)))

(define arbol2 (list 5 (list 9 (empty-tree)
                             (list 5 (empty-tree)
                                   (empty-tree)))
                     (list 1 (list 3 (empty-tree)
                                   (empty-tree))
                           (list 0 (empty-tree)
                                 (empty-tree)))))

(define arbol3 (list 5 (list 1 (list 0 (empty-tree)
                                     (empty-tree))
                             (list 3 (empty-tree)
                                   (empty-tree)))
                     (list 9 (empty-tree)
                           (list 10 (empty-tree)
                                 (empty-tree)))))
