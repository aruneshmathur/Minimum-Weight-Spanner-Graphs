(load "dijkstra.scm")

(define (spanner graph t)
  (let loop ((spanner-vertices (graph-vertices graph))
	     (spanner-edges '())
	     (edges (sort (graph-edges graph) 
			  (lambda (x y) (< (edge-wt x)
					   (edge-wt y))))))
    (cond ((null? edges) (make-graph spanner-vertices
				     spanner-edges))
	  
	  ((> (dijkstra (make-graph spanner-vertices
				    spanner-edges)
			(edge-from (car edges))
	                (edge-to (car edges)))
                             
		   (* t (edge-wt (car edges))))
	   (loop spanner-vertices
		 (append spanner-edges (list (make-edge (make-vertex (edge-from (car edges)))
							(make-vertex (edge-to (car edges)))
							(make-edge-wt (edge-wt (car edges))))))
		 (cdr edges)))

	  (else (loop spanner-vertices
		      spanner-edges
		      (cdr edges))))))


(define (FG-spanner graph t)
  (let loop ((spanner-vertices (graph-vertices graph))
	     (spanner-edges '())
	     (edges (sort (graph-edges graph) 
			  (lambda (x y) (< (edge-wt x)
					   (edge-wt y)))))
	     (weight (make-matrix (length (graph-vertices graph)))))

      (cond ((null? edges) (make-graph spanner-vertices
				       spanner-edges))     
	   ((> (get-matrix-val weight (edge-from (car edges)) (edge-to (car edges)))
	      (* t (edge-wt (car edges))))
	    (let*  ((cur_edge (car edges))
		    (u (edge-from cur_edge))
		    (v (edge-to cur_edge)))
	      (begin 
		(map (lambda (x) (let ((val (min (dijkstra (make-graph spanner-vertices
								       spanner-edges)
							   u 
							   (vertex x))
						 (get-matrix-val weight u (vertex x)))))
				   (begin
				     (set-matrix-val! weight u (vertex x) val)
				     (set-matrix-val! weight (vertex x) u val)))) (graph-vertices graph))
		(if (> (get-matrix-val weight u v)
		       (* t (edge-wt cur_edge)))
		  (loop spanner-vertices
			(append spanner-edges (list (make-edge (make-vertex u)
							       (make-vertex v)
							       (make-edge-wt (edge-wt cur_edge)))))
			(cdr edges)
			weight)
		  (loop spanner-vertices
			spanner-edges
			(cdr edges)
			weight)))))
	   (else (loop spanner-vertices
		       spanner-edges
		       (cdr edges)
		       weight)))))
		

(define (make-matrix vertex-count)
  (let loop ((i 0)
	     (matrix (vector-map (lambda (x) (make-vector x 
							  infinity)) 
				 (make-vector (+ vertex-count 1)
					      (+ vertex-count 1)))))
    (cond 
      ((= i vertex-count) matrix)
      (else
	(begin
	  (set-matrix-val! matrix i i 0)
	  (loop (+ i 1)
		matrix))))))


(define (set-matrix-val! matrix i j val)
  (vector-set! (vector-ref matrix i) j val))


(define (get-matrix-val matrix i j)
  (vector-ref (vector-ref matrix i) j))

