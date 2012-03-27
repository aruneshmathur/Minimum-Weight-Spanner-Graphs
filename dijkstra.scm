(load "graph.scm")


(define vertex-dist cadadr)


(define vertex-dist-list cadar)


(define distance-dist-list cdadar)


(define (make-dist-list vertices)
  (if (null? vertices)
    '()
    (cons (list (car vertices) (list 'd 9999))
	  (make-dist-list (cdr vertices)))))


(define (set-dist-list! dist_list vertex val)
  (cond ((null? dist_list) (error "The given vertex does not exist"))
	((equal? (vertex-dist-list (car dist_list))
		 vertex)
	 (set-car! (distance-dist-list dist_list) val))
	(else (set-dist-list! (cdr dist_list) 
			      vertex 
			      val))))


(define (get-dist-list dist_list vertex)
  (cond ((null? dist_list) (error "The given vertex does not exist"))
	((equal? (vertex-dist-list (car dist_list))
		 vertex)
	 (car (distance-dist-list dist_list)))
	(else (get-dist-list (cdr dist_list)
			     vertex))))


(define (relax dist_list u v wt)
  (set-dist-list! dist_list 
		  v 
		  (+ (get-dist-list dist_list u) wt)))

