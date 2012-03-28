(load "graph.scm")

(define dist-val cadadr)


(define vertex-dist-list cadar)


(define distance-dist-list cdadar)


(define (make-dist-list vertices start)
  (cond 
    ((null? vertices) '())
    ((eq? (vertex (car vertices)) start)
     (cons (list (car vertices) (list 'd 0))
	   (make-dist-list (cdr vertices)
			   start)))
    (else (cons (list (car vertices) (list 'd 9999))
		(make-dist-list (cdr vertices)
				start)))))


(define (set-dist-list! dist_list vertex val)
  (cond 
    ((null? dist_list) (error "The given vertex does not exist"))
    ((equal? (vertex-dist-list (car dist_list))
	     vertex)
     (set-car! (distance-dist-list dist_list) val))
    (else (set-dist-list! (cdr dist_list) 
			  vertex 
			  val))))


(define (get-dist-list dist_list vertex)
  (cond 
    ((null? dist_list) (error "The given vertex does not exist"))
    ((equal? (vertex-dist-list (car dist_list))
	     vertex)
     (car (distance-dist-list dist_list)))
    (else (get-dist-list (cdr dist_list)
			 vertex))))


(define (relax dist_list u v wt)
  (set-dist-list! dist_list 
		  v 
		  (+ (get-dist-list dist_list u) wt)))


(define (update g dist_list cur_vertex neighbors)
  (cond
    ((null? neighbors) '())
    ((null? dist_list) '())

    (else (map (lambda (x) (if (> (get-dist-list dist_list (vertex x))
				  (+ (get-dist-list dist_list
						    cur_vertex)
				     (get-edge-wt g 
						  cur_vertex
						  (vertex x))))
			     (relax dist_list
				    cur_vertex
				    (vertex x)
				    (get-edge-wt g 
						 cur_vertex 
						 (vertex x))))) 
	       neighbors))))



;    ((> (get-dist-list dist_list (vertex (car neighbors)))
;	(+ (get-dist-list dist_list 
;			  cur_vertex)
;	   (get-edge-wt g 
;			cur_vertex 
;			(vertex (car neighbors)))))
;     (begin
;       (relax dist_list 
;	      cur_vertex 
;	      (vertex (car neighbors))
;	      (get-edge-wt g 
;			   cur_vertex 
;			   (vertex (car neighbors))))
;       (update g 
;	       dist_list
;	       cur_vertex
;	       (cdr neighbors))))
;    (else (update g 
;		  dist_list 
;		  cur_vertex 
;		  (cdr neighbors)))))


(define (dijkstra g start end)
  (let loop ((dist_list (make-dist-list (graph-vertices g) start))
	     (visit_list (graph-vertices g))
	     (cur_vertex start))

    (cond 
      ((eq? cur_vertex end) (get-dist-list dist_list cur_vertex))
      ((null? dist_list) (error "Something has horribly gone wrong"))
      (else
	(begin
	  (update g
		  dist_list 
		  cur_vertex 
		  (filter (lambda (x) (member (vertex x) (vertex-neighbors g
								  cur_vertex)))
			  visit_list))
	  (let ((temp_dist_list (filter (lambda (x) (not (eq? (vertex-dist-list x) cur_vertex)))
					dist_list)))

	    (loop temp_dist_list
		  (filter (lambda (x) (not (eq? (vertex x) cur_vertex)))
			  visit_list)
		  (vertex-dist-list (car (sort temp_dist_list (lambda (x y) (< (dist-val x)
									       (dist-val y)))))))))))))



