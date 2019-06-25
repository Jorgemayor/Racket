#lang eopl
; Taller 2 Fundamentos de lenguaje de programacion
; 
; abb-listas.rkt
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
;-------------------------------------------------------------------------------

;<binary-tree> ::= (empty-tree) empty
;              ::= (node) <int> <binary-tree> <binary-tree>

;-------------------------------------------------------------------------------
;LISTS
;-------------------------------------------------------------------------------

;arbol-vacio: {void}
;             -> {list}
;Purpose:
;Returns a list with an 'arbol-vacio.
(define arbol-vacio
  (lambda ()
    (list 'arbol-vacio)
    )
  )

;Prueba:
(arbol-vacio)

;-------------------------------------------------------------------------------
;Tree instantiations

(define tree1 (list 5 (arbol-vacio)
                    (arbol-vacio)))

(define tree2 (list 5 (list 9 (arbol-vacio)
                            (list 5 (arbol-vacio)
                                  (arbol-vacio)))
                    (list 1 (list 3 (arbol-vacio)
                                  (arbol-vacio))
                          (list 0 (arbol-vacio)
                                (arbol-vacio)))))

(define tree3 (list 5 (list 1 (list 0 (arbol-vacio)
                                    (arbol-vacio))
                            (list 3 (arbol-vacio)
                                  (arbol-vacio)))
                    (list 9 (arbol-vacio)
                          (list 10 (arbol-vacio)
                                (arbol-vacio)))))

;-------------------------------------------------------------------------------
;arbol-vacio: {tree}
;            -> {boolean}
;Purpose:
;Returns true if the given tree is an arbol-vacio, false otherwise.
(define arbol-vacio?
  (lambda (tree)
    (eqv? (car tree) (car (arbol-vacio)))
    )
  )

;Pruebas:
(arbol-vacio? (arbol-vacio))
(arbol-vacio? tree1)

;-------------------------------------------------------------------------------
;make-tree: number {int}
;           -> {list}
;Purpose:
;Makes a tree with the value and, the left and right
;given trees.
(define make-tree
  (lambda (number leftTree rightTree)
    (list number leftTree rightTree)
    )
  )

;Pruebas:
(make-tree 4 (arbol-vacio) (arbol-vacio))
(make-tree 4 (make-tree 3 (arbol-vacio) (arbol-vacio))
           (make-tree 5 (arbol-vacio) (arbol-vacio)))

;-------------------------------------------------------------------------------
;extract-node: tree {list}
;              -> {int}
;Purpose:
;Extracts the value (node) of a given tree.
(define extract-node
  (lambda (tree)
    (car tree)
    )
  )

;Pruebas:
(extract-node (make-tree 4 (arbol-vacio) (arbol-vacio)))
(extract-node tree1)

;-------------------------------------------------------------------------------
;extract-izq: tree {list}
;             -> {list}
;Purpose:
;Extracts the left tree of a given tree.
(define extract-izq
  (lambda (tree)
    (cadr tree)
    )
  )

;Pruebas:
(extract-izq tree1)
(extract-izq tree2)

;-------------------------------------------------------------------------------
;extract-der: tree {list}
;             -> {list}
;Purpose:
;Extracts the right tree of a given tree.
(define extract-der
  (lambda (tree)
    (caddr tree)
    )
  )

;Pruebas:
(extract-der tree1)
(extract-der tree2)

;-------------------------------------------------------------------------------
;leaf-tree?: tree {list}
;           -> {list}
;Purpose:
;Returns true if the given tree is a leaf, false
;otherwise.
;An arbol-vacio is not taken as a leaf.
(define leaf-tree?
  (lambda (tree)
    (and (not (arbol-vacio? tree))
         (arbol-vacio? (extract-izq tree))
         (arbol-vacio? (extract-der tree)))
    )
  )

;Pruebas:
(leaf-tree? tree1)
(leaf-tree? tree2)

;-------------------------------------------------------------------------------
;node-tree?: tree {list}
;            -> {boolean}
;Purpose:
;Returns true if the given tree is a node, false
;otherwise.
;If a tree has at least one tree on its branches,
;is taken as a node.
(define node-tree?
  (lambda (tree)
    (not (and (arbol-vacio? (extract-izq tree))
              (arbol-vacio? (extract-der tree)))
         )
    )
  )

;Pruebas:
(node-tree? tree1)
(node-tree? tree2)

;-------------------------------------------------------------------------------
;validador-orden: tree {list}
;                 -> {boolean}
;Purpose:
;Validates if a given tree is a well ordered tree, so all the values on the left
;branch of a tree must be smaller than the value in the node.
;The values on the left branch must be bigger than the value in the node.
;At first, the function transforms the tree into a list and then, validates the
;order of the numbers in this list.
(define validador-orden
  (lambda (tree)
    (letrec ([treeToList (lambda (tree)
                           (cond [(arbol-vacio? tree) empty]
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

;Pruebas:
(validador-orden tree1)
(validador-orden tree2)
(validador-orden tree3)

;-------------------------------------------------------------------------------
;insertar-elemento: tree {list}
;                   number {int}
;                   -> {list}
;Purpose:
;Inserts an element into a tree in the correect possition, only
;if the tree i a well ordered tree.
;Returns the tree with the insertion, if the number was not in
;the tree.
(define insertar-elemento
  (lambda (tree number)
    (letrec ([insertar (lambda (tree)
                         (cond [(arbol-vacio? tree) (make-tree number (arbol-vacio) (arbol-vacio))]
                               [(= (extract-node tree) number) tree]
                               [(< (extract-node tree) number) (cons (extract-node tree) (list (extract-izq tree) (insertar (extract-der tree))))]
                               [else (cons (extract-node tree) (list (insertar (extract-izq tree)) (extract-der tree)))])
                         )]
             [main (cond [(validador-orden tree) (insertar tree)]
                         [else 'Invalid])]
             )
      main)
    )
  )

;Pruebas:
(insertar-elemento tree1 3)
(insertar-elemento tree2 4)
(insertar-elemento tree3 1)
(insertar-elemento tree3 4)
