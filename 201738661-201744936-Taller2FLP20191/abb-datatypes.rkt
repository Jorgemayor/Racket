;Encabezado

;-------------------------------------------------------------------------------
;DATATYPES
;----------------------------------------------

(define-datatype treeType treeType?
  (empty-treeType)
  (extract-nodeP (tree treeType?))
  (extract-izqP (tree treeType?))
  (extract-derP (tree treeType?))
  (leaf-treeP? (tree treeType?))
  (node-treeP? (tree treeType?))
  (validador-ordenP (tree treeType?))
  (insertar-elementoP (tree treeType?)
                      (number number?))
  )
