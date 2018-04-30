#lang racket
(require "set.rkt")
(require "map.rkt")

(define t1 (set-insert (set-insert (set-insert (set-insert (set-insert (set-insert
                          (set-insert (set-insert (set-insert (set-nulltr) 1) 1) 2) 3) 4) 5) 6) 7) 8))
(displayln "Tree t1")
(print-set t1)

(define t2 (set-insert (set-insert (set-insert (set-insert (set-insert (set-insert
         (set-insert (set-insert (set-nulltr) 6) 7) 8) 13) 14) 15) 17) 18))
(displayln "Tree t2")
(print-set t2)

(define insert-tree (set-insert t1 25))
(displayln "Insert 25 in t1")
(print-set insert-tree)
(define delete-tree (set-delete t2 13))
(displayln "Delete 13 from t2")
(print-set delete-tree)
(define b (list->set '(1 2 3 4 5 6 8 7 9 )))
(displayln "Conversion of a list into a set")
(print-set b)
(define a (set->list t1))
(displayln "t1 converted into a list")
a
(define c (set-union t1 t2))
(displayln "Union of trees t1 and t2")
(print-set c)
(define d (set-intersection t1 t2))
(displayln "Intersection of t1 and t2")
(print-set d)

(displayln "")
(displayln "----------------------------------------------------")
(displayln "----------------------------------------------------")
(displayln "")

(define t1-map (map-insert (map-insert (map-insert (map-insert (map-insert (map-insert
(map-insert (map-insert (map-nulltr) 1 2) 2 7) 3 4) 4 4) 5 8) 6 2) 7 4) 8 8))
(displayln "Map t1")
(print-map t1-map)
(displayln "Insertion of 5 at key 3 in the tree")
(define inserted (map-insert t1-map 3 5))
(print-map inserted)
(define deleted (map-delete t1-map 5))
(displayln "Deletes key 5 from the map")
(print-map deleted)
(define ss (map-value? t1-map 4))
(displayln "Value at key 4")
ss
(define ll (list->map (list (cons 1 2) (cons 2 7) (cons 3 4) (cons 4 4) (cons 5 8) (cons 6 2)
                                   (cons 7 4) (cons 8 8))))
(displayln "List converted into a map")
(print-map ll)
(displayln "Map converted into a list")
(define mm (map->list t1-map))
mm
