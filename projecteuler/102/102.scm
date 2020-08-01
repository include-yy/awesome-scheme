(define tri-vec (make-vector 1000 #f))

(call-with-input-file "p102_triangles.txt"
  (lambda (po)
    (let f ([i 0])
      (cond
       [(= i 1000) #t]
       [else
	(let* ([a1 (get-datum po)]
	       [a2 (begin (get-char po) (get-datum po))]
	       [b1 (begin (get-char po) (get-datum po))]
	       [b2 (begin (get-char po) (get-datum po))]
	       [c1 (begin (get-char po) (get-datum po))]
	       [c2 (begin (get-char po) (get-datum po))])
	  (vector-set! tri-vec i (make-vector 3))
	  (vector-set! (vector-ref tri-vec i) 0 (cons a1 a2))
	  (vector-set! (vector-ref tri-vec i) 1 (cons b1 b2))
	  (vector-set! (vector-ref tri-vec i) 2 (cons c1 c2))
	  (f (+ i 1)))]))))

(define cross
  (letrec  ([xp (lambda (p) (car p))]
	    [yp (lambda (p) (cdr p))])
    (lambda (p1 p2)
      (if (zero? (- (xp p1) (xp p2)))
	  #f
	  (let ([k (/ (- (yp p1) (yp p2)) (- (xp p1) (xp p2)))])
	    (cond
	     ((= k 0) (yp p1))
	     (else
	      (- (yp p1) (* k (xp p1))))))))))

(define judge
  (lambda (vec)
    (let-syntax ([ref (syntax-rules()
			[(_ v i)
			 (vector-ref v i)])])
      (let ([new-v (vector-sort (lambda (a b) (< (car a) (car b))) vec)])
	(cond
	 ((or (and (<= (car (ref new-v 0)) 0) (<= (car (ref new-v 1)) 0) (<= (car (ref new-v 2)) 0))
	      (and (>= (car (ref new-v 0)) 0) (>= (car (ref new-v 1)) 0) (>= (car (ref new-v 2)) 0)))
	  #f)
	 ((= (car (ref new-v 1)) 0)
	  (cond
	   ((or (= (car (ref new-v 0)) 0) (= (car (ref new-v 2)) 0)) #f)
	   (else
	    (let ([pt (cross (ref new-v 0) (ref new-v 2))])
	      (if (< (* pt (cdr (ref new-v 1))) 0) #t #f)))))
	 (else
	  (cond
	   ((< (car (ref new-v 1)) 0)
	    (let ([fst (cross (ref new-v 0) (ref new-v 2))]
		  [sec (cross (ref new-v 1) (ref new-v 2))])
	      (if (< (* fst sec) 0) #t #f)))
	   ((> (car (ref new-v 1)) 0)
	    (let ([fst (cross (ref new-v 1) (ref new-v 0))]
		  [sec (cross (ref new-v 2) (ref new-v 0))])
	      (if (< (* fst sec) 0) #t #f))))))))))
	       
(do ([i 0 (+ i 1)]
     [sum 0 (if (judge (vector-ref tri-vec i)) (+ sum 1) sum)])
    ((= i 1000) sum))
;;228
