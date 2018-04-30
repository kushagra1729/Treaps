#lang racket
(require pict)
(require pict/tree-layout)

(provide set set-nulltr set-insert set-delete search list->set set->list set-union set-intersection print-set)

(define maxi 1000000000)

(struct set (val pr ltree rtree) #:transparent)
(struct set-nulltr () #:transparent)

;O(log n)
(define (split root x)
  (match root
    [(set-nulltr) (cons (set-nulltr) (set-nulltr))]
    [(set val pr lt rt)
     (cond [(<= x val) (let* ([sp (split lt x)])
                         (cons (car sp) (set (set-val root) (set-pr root) (cdr sp) rt)))]
           [else (let* ([sp (split rt x)])
                         (cons (set (set-val root) (set-pr root) lt (car sp)) (cdr sp)))])]))
;O(log n)
(define (merge l r)
  (cond [(set-nulltr? l) r]
        [(set-nulltr? r) l]
        [(> (set-pr l) (set-pr r))
         (set (set-val l) (set-pr l) (set-ltree l) (merge (set-rtree l) r))]
        [else
         (set (set-val r) (set-pr r) (merge l (set-ltree r)) (set-rtree r))]))

;O(log n)
(define (set-insert tree x)
  (let* ([sp (split tree x)]
         [node (set x (random maxi) (set-nulltr) (set-nulltr))]
         [sp2 (split (cdr sp) (+ x 1))])
    (merge (merge (car sp) node) (cdr sp2))))

;O(log n)
(define (set-delete tree x)
  (let* ([sp (split tree x)]
         [sp2 (split (cdr sp) (+ x 1))])
    (merge (car sp) (cdr sp2))))

;O(log n)
(define (search tree x)
  (cond [(set-nulltr? tree) #f]
        [(= (set-val tree) x) #t]
        [(< x (set-val tree)) (search (set-ltree tree) x)]
        [else (search (set-rtree tree) x)]))

;O(n*log n)
(define (list->set l)
  (cond [(null? l) (set-nulltr)]
        [else (set-insert (list->set (cdr l)) (car l))]))

;O(n)
(define (set->list tree)
  (define (helper tree l)
    (match tree
      [(set-nulltr) l]
      [(set val pr lt rt)
       (let* ([l1 (helper rt l)]
              [l2 (cons val l1)])
         (helper lt l2))]))
  (helper tree '()))

;O(n*log n) (Check if better possible)
(define (set-union t1 t2)
  (let* ([l1 (set->list t1)]
         [l2 (set->list t2)])
    (list->set (merge-duplicates l1 l2))))

(define (merge-duplicates l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [(= (car l1) (car l2)) (cons (car l1) (merge-duplicates (cdr l1) (cdr l2)))]
        [(< (car l1) (car l2)) (cons (car l1) (merge-duplicates (cdr l1) l2))]
        [else (cons (car l2) (merge-duplicates l1 (cdr l2)))]))

;O(n*log n) (Check if better possible)
(define (set-intersection t1 t2)
  (let* ([l1 (set->list t1)]
         [l2 (set->list t2)])
    (list->set (intersect l1 l2))))

(define (intersect l1 l2)
  (cond [(or (null? l2) (null? l1)) '()]
        [(= (car l1) (car l2)) (cons (car l1) (intersect (cdr l1) (cdr l2)))]
        [(< (car l1) (car l2)) (intersect (cdr l1) l2)]
        [else (intersect l1 (cdr l2))]))

(define (node num)
  (cc-superimpose (filled-ellipse 40 40 #:color "Chartreuse" #:border-color "Medium Aquamarine" #:border-width 1) (text (number->string num))))

(define (print-set t)
  (binary-tidier (make-tree t)))

(define (make-tree t)
  (match t
    [(set val pr lt rt) (cond [(and (set-nulltr? lt) (set-nulltr? rt)) (tree-layout #:pict (node val) #f #f)]
                              [(and (set-nulltr? lt) (not (set-nulltr? rt))) (tree-layout #:pict (node val) #f
                                                                                          (tree-edge #:edge-color "green" (make-tree rt)))]
                              [(and (not (set-nulltr? lt)) (set-nulltr? rt)) (tree-layout #:pict (node val)
                                                                                          (tree-edge #:edge-color "green" (make-tree lt)) #f)]
                              [else (tree-layout #:pict (node val) (tree-edge #:edge-color "green" (make-tree lt))
                                                 (tree-edge #:edge-color "green" (make-tree rt)))])]))

(define t (set-insert (set-insert (set-insert (set-insert (set-insert (set-insert (set-insert (set-insert (set-nulltr) 1) 2) 3) 4) 5) 6) 7) 8))