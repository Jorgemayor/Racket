#lang eopl
;Encabezado

;-------------------------------------------------------------------------------
;DATATYPES
;----------------------------------------------

(define-datatype treeType treeType?
  (empty-treeType)
  (insertar-elemento (node number?)
                     (treeIzq treeType?)
                     (treeDer treeType?))
  )


(define empty-treeType?
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      #t)
      (insertar-elemento (node treeIzq treeDer)
                         #f))
    )
  )

(define extract-node
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      "The tree is null")
      (insertar-elemento (node treeIzq treeDer)
                         node))
    )
  )

(define extract-izq
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      "The tree is null")
      (insertar-elemento (node treeIzq treeDer)
                         treeIzq))
    )
  )

(define extract-der
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      "The tree is null")
      (insertar-elemento (node treeIzq treeDer)
                         treeDer))
    )
  )

(define leaf-tree?
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      #t)
      (insertar-elemento (node treeIzq treeDer)
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
      (insertar-elemento (node treeIzq treeDer)
                         (cond [(or (empty-treeType? treeIzq)
                                    (empty-treeType? treeDer)) #f]
                               [else #t]))
      )
    )
  )

(define validador-orden
  (lambda (tree)
    (cases treeType tree
      (empty-treeType ()
                      #f)
      (insertar-elemento (node treeIzq treeDer)
                         treeDer))
    )
  )