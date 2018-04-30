#lang racket
(require pict)
(require pict/tree-layout)

(provide map map-nulltr map-insert map-delete map-value? list->map map->list print print-map)
         
(define maxi 1000000000)

(struct map (key val pr ltree rtree) #:transparent)
(struct map-nulltr () #:transparent)

;O(log n)
(define (split root x)
  (match root
    [(map-nulltr) (cons (map-nulltr) (map-nulltr))]
    [(map key v pr lt rt)
     (cond [(<= x key) (let* ([sp (split lt x)])
                         (cons (car sp) (map (map-key root) (map-val root) (map-pr root) (cdr sp) rt)))]
           [else (let* ([sp (split rt x)])
                         (cons (map (map-key root) (map-val root) (map-pr root) lt (car sp)) (cdr sp)))])]))
;O(log n)
(define (merge l r)
  (cond [(map-nulltr? l) r]
        [(map-nulltr? r) l]
        [(> (map-pr l) (map-pr r))
         (map (map-key l) (map-val l) (map-pr l) (map-ltree l) (merge (map-rtree l) r))]
        [else
         (map (map-key r) (map-val r) (map-pr r) (merge l (map-ltree r)) (map-rtree r))]))

;O(log n)
(define (map-insert tree x v)
  (let* ([sp (split tree x)]
         [node (map x v (random maxi) (map-nulltr) (map-nulltr))]
         [sp2 (split (cdr sp) (+ x 1))])
    (merge (merge (car sp) node) (cdr sp2))))

;O(log n)
(define (map-delete tree x)
  (let* ([sp (split tree x)]
         [sp2 (split (cdr sp) (+ x 1))])
    (merge (car sp) (cdr sp2))))

;O(log n)
(define (map-value? tree x)
  (cond [(map-nulltr? tree) #f]
        [(= (map-key tree) x) (map-val tree)]
        [(< x (map-key tree)) (map-value? (map-ltree tree) x)]
        [else (map-value? (map-rtree tree) x)]))

;O(n*log n)
(define (list->map l)
  (cond [(null? l) (map-nulltr)]
        [else (map-insert (list->map (cdr l)) (caar l) (cdar l))]))

;O(n)
(define (map->list tree)
  (define (helper tree l)
    (match tree
      [(map-nulltr) l]
      [(map key v pr lt rt)
       (let* ([l1 (helper rt l)]
              [l2 (cons (cons key v) l1)])
         (helper lt l2))]))
  (helper tree '()))

(define (node key val)
  (cc-superimpose (filled-ellipse 40 40 #:color "Chartreuse" #:border-color "Medium Aquamarine" #:border-width 1) (text (string-append (number->string key) ":" (number->string val)))))

(define (print-map t)
  (binary-tidier (make-tree t)))

(define (make-tree t)
  (match t
    [(map key val pr lt rt) (cond [(and (map-nulltr? lt) (map-nulltr? rt)) (tree-layout #:pict (node key val) #f #f)]
                              [(and (map-nulltr? lt) (not (map-nulltr? rt))) (tree-layout #:pict (node key val) #f
                                                                                          (tree-edge #:edge-color "green" (make-tree rt)))]
                              [(and (not (map-nulltr? lt)) (map-nulltr? rt)) (tree-layout #:pict (node key val)
                                                                                          (tree-edge #:edge-color "green" (make-tree lt)) #f)]
                              [else (tree-layout #:pict (node key val) (tree-edge #:edge-color "green" (make-tree lt))
                                                 (tree-edge #:edge-color "green" (make-tree rt)))])]))

;(define t (map-insert (map-insert (map-insert (map-insert (map-insert (map-insert (map-insert (map-insert (map-nulltr) 1 3) 2 2) 3 3) 4 4) 5 5) 6 6) 7 7) 8 8))