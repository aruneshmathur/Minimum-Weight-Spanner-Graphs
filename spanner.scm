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

	      


						 


