#lang eopl
;Encabezado

;-------------------------------------------------------------------------------
;DATATYPES
;----------------------------------------------

(define-datatype treeType treeType?
  (empty-treeType)
  (node (value number?)
        (treeIzq treeType?)
        (treeDer treeType?))
  )


(define empty-treeType?
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      #t)
      (node (value treeIzq treeDer)
            #f))
    )
  )

(define extract-node
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      "The tree is null")
      (node (value treeIzq treeDer)
            value))
    )
  )

(define extract-izq
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      "The tree is null")
      (node (value treeIzq treeDer)
            treeIzq))
    )
  )

(define extract-der
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      "The tree is null")
      (node (value treeIzq treeDer)
            treeDer))
    )
  )

(define leaf-tree?
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      #t)
      (node (value treeIzq treeDer)
            (cond [(and (empty-treeType? treeIzq)
                        (empty-treeType? treeDer)) #t]
                  [else #f]))
      )
    )
  )

(define node-tree?
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      #f)
      (node (value treeIzq treeDer)
            (cond [(or (empty-treeType? treeIzq)
                       (empty-treeType? treeDer)) #f]
                  [else #t]))
      )
    )
  )

(define validador-orden
  (lambda (tree)
    (letrec ([treeToList (cases treeType tree
                           (empty-treeType ()
                                           empty)
                           (node (value treeIzq treeDer)
                                 (append (treeToList treeIzq) (list value) (treeToList treeDer)))
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

(define unparseTree
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      '(emptyTreeList))
      (node (value treeIzq treeDer)
            (list 'value value 'leftTree (unparseTree treeIzq) 'rightTree (unparseTree treeDer))))
    )
  )

(define parseTree
  (lambda (dato)
    (if (and (not (null? dato))
             (not (null? (cdr dato)))
             (not (null? (cddr dato)))
             (null? (cdddr dato)))
        (cond
          [(eqv? (car dato) 'emptyTreeList) (empty-treeType)]
          [(and (eqv? (car dato) 'value)
                (eqv? (caddr dato) 'leftTree)
                (eqv? (caddr (cddr dato)) 'rightTree))
           (node
            (cadr dato)
            (parseTree (caddr (cdr dato)))
            (parseTree (caddr (cdddr dato))))]
          [else 'Invalido])
        'Invalido)
    )
  )
