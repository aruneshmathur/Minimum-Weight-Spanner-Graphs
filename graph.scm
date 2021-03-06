(define (make-graph vertices edges)
  (list vertices edges))


(define graph-vertices car)


(define graph-edges cadr)


(define (make-vertex name)
  (list 'v name))


(define vertex cadr)


(define (make-edge-wt wt)
  (list 'w wt))


(define vertex-name cadr)


(define (make-edge from to wt)
  (list 'e from to wt))


(define edge-from cadadr)


(define (edge-to edge)
  (car (cdaddr edge)))


(define (edge-wt edge)
  (car (cdaddr (cdr edge))))


(define (add-edge graph u v w)
  (let loop ((edges (graph-edges graph)))
    (cond
      ((eq? edges '()) (append! edges (list (make-edge (make-vertex u)
				                    (make-vertex v)
					            (make-edge-wt w)))))
      ((eq? (cdr edges) '())
       (set-cdr! edges (list (make-edge (make-vertex u)
					(make-vertex v)
					(make-edge-wt w)))))
      (else (loop (cdr edges))))))


(define (vertex-neighbors graph vertex)
  (let loop ((edges (graph-edges graph))
             (result '()))
    (cond ((null? edges)
           (remove-duplicates result))
          ((equal? (edge-from (car edges)) vertex)
           (loop (cdr edges)
                 (cons (edge-to (car edges))
                       result)))
          (else
           (loop (cdr edges)
                 result)))))


(define (get-edge-wt graph u v)
  (let loop ((edges (graph-edges graph)))
    (cond ((null? edges) '())
	  ((and (equal? (edge-from (car edges))
		       u)
	       (equal? (edge-to (car edges))
		       v))
	   (edge-wt (car edges)))
	  (else
	    (loop (cdr edges))))))


(define (remove-duplicates lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst))
         (remove-duplicates (cdr lst)))
        (else
         (cons (car lst)
               (remove-duplicates (cdr lst))))))

(define (remove-edge edges v1 v2)
  (cond 
    ((null? edges) '())
    ((or (and (eq? (edge-from (car edges)) v1)
	      (eq? (edge-to (car edges)) v2))
	 (and (eq? (edge-from (car edges)) v2)
	      (eq? (edge-to (car edges)) v1)))
     (remove-edge (cdr edges) v1 v2))
    (else 
      (cons (car edges) (remove-edge (cdr edges) v1 v2)))))
	  


(define (build-graph vertices edges)
  (make-graph (map make-vertex vertices)
              (map (lambda (edge-descp)
                     (make-edge (make-vertex (car edge-descp))
                                (make-vertex (cadr edge-descp))
				(make-edge-wt (caddr edge-descp))))
                   edges)))


