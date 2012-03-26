(define (make-graph vertices edges)
  (list vertices edges))


(define graph-vertices car)


(define graph-edges cadr)


(define (make-vertex name)
  (list 'v name))


(define vertex-name cadr)


(define (make-edge from to wt)
  (list 'e from to wt))


(define edge-from cadadr)


(define (edge-to edge)
  (car (cdaddr edge)))


(define (vertex-neighbors graph vertex)
  (let loop ((edges (graph-edges graph))
             (result '()))
    (cond ((null? edges)
           (remove-duplicates result))
          ((equal? (edge-from (car edges)) vertex)
           (loop (cdr edges)
                 (cons (edge-to (car edges))
                       result)))
          ((equal? (edge-to (car edges)) vertex)
           (loop (cdr edges)
                 (cons (edge-from (car edges))
                       result)))
          (else
           (loop (cdr edges)
                 result)))))


(define (remove-duplicates lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst))
         (remove-duplicates (cdr lst)))
        (else
         (cons (car lst)
               (remove-duplicates (cdr lst))))))


(define (build-graph vertices edges)
  (make-graph (map make-vertex vertices)
              (map (lambda (edge-descp)
                     (make-edge (make-vertex (car edge-descp))
                                (make-vertex (cadr edge-descp))
				(caddr edge-descp)))
                   edges)))

